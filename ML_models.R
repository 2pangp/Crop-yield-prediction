#####the analysis based on random sampled datasets

#set the working directory
setwd("~/Desktop/Crop Failure/Research") # mac
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

# Library packages --------------------------------------------------------

library(terra)
library(ggplot2)
library(reshape2)
library(dplyr)
library(mgcv)
library(caret)
library(gbm)
library(ranger)
library(MatchIt)
library(pdp)

# Importing data sets -----------------------------------------------------

Country <- vect("./Data/Country/Country.shp")

#input the datasets of RiceMajor
RiceMajor <- read.csv("./Data/Rice/RiceMajor.csv")
RiceSecond <- read.csv("./Data/Rice/RiceSecond.csv")

RiceMajor <- na.omit(RiceMajor)
RiceSecond <- na.omit(RiceSecond)

# Data cleaning 

Rice <- rbind(RiceMajor, RiceSecond)
Rice <- left_join(subset(Rice, select = -Region), as.data.frame(Country[,c('ISO3', 'Region')]), by = 'ISO3')

#Percentiles
Rice <- Rice[ - which(
    # (Rice$Yield > quantile(Rice$Yield, probs = 0.995, na.rm = T)) |
    (Rice$GrowPDSI < -10) |
    (Rice$GrowPDSI > 10) |
    (Rice$HarvPDSI < -10) |
    (Rice$HarvPDSI > 10)
),]

set.seed(10)
randIdx = sample(1:nrow(Rice), 0.8 * nrow(Rice))
Rice_Trn = Rice[randIdx,]
Rice_Tst = Rice[-randIdx,]

# Data cleaning ----------------------------------------------------------

min(Rice$GrowPDSI)
max(Rice$GrowPDSI)
min(Rice$HarvPDSI)
max(Rice$HarvPDSI)
min(Rice$GDP)
max(Rice$GDP)
min(Rice$Popu)
max(Rice$Popu)
min(Rice$Yield)
max(Rice$Yield)
min(Rice$Yield)
max(Rice$Yield)
min(Rice$HDI)
max(Rice$HDI)
min(Rice$Growtmin)
max(Rice$Growtmin)
min(Rice$Growtmax)
max(Rice$Growtmax)
min(Rice$Harvtmin)
max(Rice$Harvtmin)
min(Rice$Harvtmax)
max(Rice$Harvtmax)

rice_f = as.formula(Yield ~ GrowPDSI + Growtmin + Growtmax + Growsrad + HarvPDSI + Harvtmin + Harvtmax + Harvsrad + PopuDens + HDI + GDP + Year)

# Boosted regression tree -------------------------------------------------

#Rice brt
brt <- gbm(data = Rice_Trn, 
          formula = rice_f, 
          distribution = 'gaussian',
          n.trees = 200000,
          interaction.depth = 6,
          cv.folds = 5,
          bag.fraction = 0.8,
          shrinkage = 0.001, 
          verbose = T)

saveRDS(brt, "./Models/brt_yield.rds")

brt <- readRDS("./Models/brt_yield.rds")
summary(brt)

gbm.perf(object = brt, plot.it = T, oobag.curve = F, overlay = T, method = "cv")

plot(brt, i = 'Year', xlab = 'Year', ylab = 'Yield variation', main = "a) Year")

plot(brt, i = 'GrowPDSI', xlim = c(-10, 10), xlab = 'PDSI', ylab = 'Yield variation', main = "b) PDSI (Growing stage)")
plot(brt, i = 'HarvPDSI', xlim = c(-10, 10), xlab = 'PDSI', ylab = 'Yield variation', main = "c) PDSI (Harvesting stage)")
plot(brt, i = 'Growtmin', xlab = 'Temperature (°C)', ylab = 'Yield variation', main = "d) Minimum temperature (Growing stage)")
plot(brt, i = 'Harvtmin', xlab = 'Temperature (°C)', ylab = 'Yield variation', main = "e) Minimum temperature (Harvesting stage)")
plot(brt, i = 'Growtmax', xlab = 'Temperature (°C)', ylab = 'Yield variation', main = "f) Maximum temperature (Growing stage)")
plot(brt, i = 'Harvtmax', xlab = 'Temperature (°C)', ylab = 'Yield variation', main = "g) Maximum temperature (Harvesting stage)")
plot(brt, i = 'Growsrad', xlab = 'Solar radiation (MJ m-2 day-1)', ylab = 'Yield variation', main = "h) Solar radiation (Growing stage)")
plot(brt, i = 'Harvsrad', xlab = 'Solar radiation (MJ m-2 day-1)', ylab = 'Yield variation', main = "i) Solar radiation (Harvesting stage)")

plot(brt, i = 'HDI', xlim = c(0, 1.1), xlab = 'HDI', ylab = 'Yield (tons / ha)', main = "g) Human Development Index")
plot(brt, i = 'GDP', xlab = 'GDP per capita ($)', ylab = 'Yield (tons / ha)', main = "h) GDP per capita")
plot(brt, i = 'PopuDens', xlab = 'Population Density (km-2)', ylab = 'Yield (tons / ha)', main = "i) Population Density")

###variable importance
varimp <- summary(brt)
row.names(varimp) <- NULL
colnames(varimp) <- c('Variables', 'Importance')
varimp$Importance <- varimp$Importance / sum(varimp$Importance)

ggplot(data = varimp) +
  geom_bar(aes(x = Importance, y = reorder(Variables, Importance), fill = Importance), stat = 'identity') +
  scale_fill_gradient(low = 'darkseagreen3', high = 'darkseagreen4') +
  ggtitle("Variable importance") +
  xlab("Reletive importance") +
  ylab("Variables") +
  labs(fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major.x = element_line(color = 'black', linewidth = 0.2, linetype = 2),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 10))
ggsave("./Figures/Thesis/varimp_brt.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

brt_pred <- predict(brt, newdata = Rice_Tst)
Rice_Tst$brt_pred <- brt_pred

(RMSE(Rice_Tst$Yield, brt_pred) - min(Rice_Tst$Yield)) / (max(Rice_Tst$Yield) - min(Rice_Tst$Yield)) #RMSE
cor(Rice_Tst$Yield, brt_pred) #correlation in testing set

lm_tst <- lm(data = Rice_Tst,
             formula = brt_pred ~ Yield - 1)
summary(lm_tst) #they do not provide r squared if I set the slope and intercept.

###comparison between actual and predicted rice yield variation
ggplot(data = Rice_Tst) +
  geom_point(aes(x = Yield, y = brt_pred), size = 0.5) +
  geom_segment(aes(x = -0.6, y = -0.6, xend = 0.9, yend = 0.9, color = 'cornsilk4'), linetype = "dashed") +
  ggtitle("Comparison between actual and predicted yield variation") +
  xlab("Yield variation (actual)") +
  ylab("Yield variation (predicted)") +
  xlim(-0.6, 0.9) +
  ylim(-0.6, 0.9) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/tst_brt.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)



# RandomForest Ranger -----------------------------------------------------

###randomForest ranger
rf_ranger <- ranger(
  data = Rice_Trn,
  formula = rice_f,
  num.trees = 3000,
  importance = 'impurity',
  verbose = T
)
saveRDS(rf_ranger, "./Models/rf_ranger_yield.rds")

rf_ranger <- readRDS("./Models/rf_ranger_yield.rds")

###variable importance
varimp <- rf_ranger$variable.importance
varimp <- data.frame(sort(varimp, decreasing = T))
varimp$Variables <- row.names(varimp)
row.names(varimp) <- NULL
colnames(varimp) <- c('Importance', 'Variables')
varimp$Importance_rel <- varimp$Importance / sum(varimp$Importance)
varimp <- varimp[,c('Variables', 'Importance', 'Importance_rel')]
sum(varimp$Importance_rel[c(4, 5, 6, 7, 8, 10, 11, 12)])

ggplot(data = varimp) +
  geom_bar(aes(x = Importance_rel, y = reorder(Variables, Importance_rel), fill = Importance_rel), stat = 'identity') +
  scale_fill_gradient(low = 'darkseagreen3', high = 'darkseagreen4') +
  ggtitle("Variable importance (impurity)") +
  xlab("Reletive importance") +
  ylab("Variables") +
  labs(fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major.x = element_line(color = 'black', linewidth = 0.2, linetype = 2),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 10))
ggsave("./Figures/Thesis/varimp_impurity.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

ranger_pred <- predict(rf_ranger, data = Rice_Tst)
Rice_Tst$ranger_pred <- ranger_pred$predictions
RMSE(Rice_Tst$ranger_pred, Rice_Tst$Yield) / (max(Rice_Tst$Yield) - min(Rice_Tst$Yield))

quantile(Rice_Tst$Yield, probs = c(0.10, 0.90, 0.95, 0.99, 0.995, 1.00))
max(Rice)

lm_tst <- lm(data = Rice_Tst,
             formula = ranger_pred ~ Yield - 1)
summary(lm_tst) #they do not provide r squared if I set the slope and intercept.

cor(Rice_Tst$Yield, ranger_pred$predictions) #correlation in testing set

###comparison between actual and predicted rice yield variation
Rice_Tst$Ranger_pred = ranger_pred$predictions
ggplot(data = Rice_Tst) +
  geom_point(aes(x = Yield, y = Ranger_pred, color = Region), size = 0.5) +
  geom_segment(aes(x = 0, y = 0, xend = 17, yend = 17), linetype = "dashed") +
  ggtitle("Comparison between actual and predicted yield variation") +
  xlab("Yield variation (actual)") +
  ylab("Yield variation (predicted)") +
  # xlim(-0.6, 0.9) +
  # ylim(-0.6, 0.9) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2))
  #guides(color = "none")
ggsave("./Figures/Thesis/tst_act_pred.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)


###Year
pdp_Year <- partial(
  rf_ranger,
  pred.var = 'Year',
  progress = T,
  approx = F,
  plot = F
  )

###GrowPDSI
pdp_GrowPDSI <- partial(
  rf_ranger,
  pred.var = 'GrowPDSI',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
  )


###HarvPDSI
pdp_HarvPDSI <- partial(
  rf_ranger,
  pred.var = 'HarvPDSI',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
  )

###Growtmin
pdp_Growtmin <- partial(
  rf_ranger,
  pred.var = 'Growtmin',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
  )

###Harvtmin
pdp_Harvtmin <- partial(
  rf_ranger,
  pred.var = 'Harvtmin',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
  )

###Growtmax
pdp_Growtmax <- partial(
  rf_ranger,
  pred.var = 'Growtmax',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
  )

###Harvtmax
pdp_Harvtmax <- partial(
  rf_ranger,
  pred.var = 'Harvtmax',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
  )

###Growsrad
pdp_Growsrad <- partial(
  rf_ranger,
  pred.var = 'Growsrad',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
)

###Harvsrad
pdp_Harvsrad <- partial(
  rf_ranger,
  pred.var = 'Harvsrad',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
)

###GDP
pdp_GDP <- partial(
  rf_ranger,
  pred.var = 'GDP',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
)

###PopuDens
pdp_PopuDens <- partial(
  rf_ranger,
  pred.var = 'PopuDens',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
)

###HDI
pdp_HDI <- partial(
  rf_ranger,
  pred.var = 'HDI',
  progress = T,
  approx = F,
  grid.resolution = 26,
  plot = F
)

write.csv(pdp_Year, "./Data/Pdp/pdp_Year_yield.csv", row.names = F)
write.csv(pdp_GrowPDSI, "./Data/Pdp/pdp_GrowPDSI_yield.csv", row.names = F)
write.csv(pdp_HarvPDSI, "./Data/Pdp/pdp_HarvPDSI_yield.csv", row.names = F)
write.csv(pdp_Growtmin, "./Data/Pdp/pdp_Growtmin_yield.csv", row.names = F)
write.csv(pdp_Harvtmin, "./Data/Pdp/pdp_Harvtmin_yield.csv", row.names = F)
write.csv(pdp_Growtmax, "./Data/Pdp/pdp_Growtmax_yield.csv", row.names = F)
write.csv(pdp_Harvtmax, "./Data/Pdp/pdp_Harvtmax_yield.csv", row.names = F)
write.csv(pdp_Growsrad, "./Data/Pdp/pdp_Growsrad_yield.csv", row.names = F)
write.csv(pdp_Harvsrad, "./Data/Pdp/pdp_Harvsrad_yield.csv", row.names = F)
write.csv(pdp_GDP, "./Data/Pdp/pdp_GDP_yield.csv", row.names = F)
write.csv(pdp_PopuDens, "./Data/Pdp/pdp_PopuDens_yield.csv", row.names = F)
write.csv(pdp_HDI, "./Data/Pdp/pdp_HDI_yield.csv", row.names = F)


pdp_Year <- read.csv("./Data/Pdp/pdp_Year.csv")
pdp_GrowPDSI <- read.csv("./Data/Pdp/pdp_GrowPDSI.csv")
pdp_HarvPDSI <- read.csv("./Data/Pdp/pdp_HarvPDSI.csv")
pdp_Growtmin <- read.csv("./Data/Pdp/pdp_Growtmin.csv")
pdp_Harvtmin <- read.csv("./Data/Pdp/pdp_Harvtmin.csv")
pdp_Growtmax <- read.csv("./Data/Pdp/pdp_Growtmax.csv")
pdp_Harvtmax <- read.csv("./Data/Pdp/pdp_Harvtmax.csv")
pdp_GDP <- read.csv("./Data/Pdp/pdp_GDP.csv")
pdp_PopuDens <- read.csv("./Data/Pdp/pdp_PopuDens.csv")
pdp_HDI <- read.csv("./Data/Pdp/pdp_HDI.csv")


###pdp_Year
ggplot(data = pdp_Year) +
  geom_line(aes(x = Year, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (Year)") +
  xlab("Year") +
  ylab("Rice yield variation") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_Year.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_GrowPDSI
ggplot(data = pdp_GrowPDSI) +
  geom_line(aes(x = GrowPDSI, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (GrowPDSI)") +
  xlab("PDSI") +
  ylab("Rice yield variation") +
  # ylim(-0.055, -0.015) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_GrowPDSI.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_HarvPDSI
ggplot(data = pdp_HarvPDSI) +
  geom_line(aes(x = HarvPDSI, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (HarvPDSI)") +
  xlab("HarvPDSI") +
  ylab("Yield variation") +
  # ylim(-0.055, -0.015) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_HarvPDSI.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_Growtmin
ggplot(data = pdp_Growtmin) +
  geom_line(aes(x = Growtmin, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (Growtmin)") +
  xlab("Growtmin") +
  ylab("Yield variation") +
  xlim(-3, 28) +
  # ylim(-0.095, -0.005) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_Growtmin.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_Harvtmin
ggplot(data = pdp_Harvtmin) +
  geom_line(aes(x = Harvtmin, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (Harvtmin)") +
  xlab("Harvtmin") +
  ylab("Yield variation") +
  xlim(-3, 28) +
  # ylim(-0.095, -0.005) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_Harvtmin.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_Growtmax
ggplot(data = pdp_Growtmax) +
  geom_line(aes(x = Growtmax, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (Growtmax)") +
  xlab("Growtmax") +
  ylab("Yield variation") +
  # xlim(10, 40.5) +
  # ylim(-0.095, -0.005) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_Growtmax.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_Harvtmax
ggplot(data = pdp_Harvtmax) +
  geom_line(aes(x = Harvtmax, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (Harvtmax)") +
  xlab("Harvtmax") +
  ylab("Yield variation") +
  # xlim(10, 40.5) +
  # ylim(-0.095, -0.005) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_Harvtmax.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_Growsrad
ggplot(data = pdp_Growsrad) +
  geom_line(aes(x = Growsrad, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (Growsrad)") +
  xlab("Growsrad") +
  ylab("Yield variation") +
  xlim(110, 290) +
  # ylim(-0.125, 0) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_Growsrad.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

###pdp_Harvsrad
ggplot(data = pdp_Harvsrad) +
  geom_line(aes(x = Harvsrad, y = yhat), size = 0.5) +
  ggtitle("Partial dependence plot (Harvsrad)") +
  xlab("Harvsrad") +
  ylab("Yield variation") +
  xlim(110, 290) +
  # ylim(-0.125, 0) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.4, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.2, linetype = 2)) +
  guides(color = "none")
ggsave("./Figures/Thesis/pdp_Harvsrad.tif", device = 'tiff', width = 3840 * 2, height = 2160 * 2, units = 'px', dpi = 600)

### Test
test <- data.frame(
  GrowPDSI = c(-10, -8, -6, -4, -2, 2, 4, 6, 8, 10),
  Growtmin = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
  Growtmax = c(30, 30, 30, 30, 30, 30, 30, 30, 30, 30),
  Growsrad = c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200),
  HarvPDSI = c(-10, -8, -6, -4, -2, 2, 4, 6, 8, 10),
  Harvtmin = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
  Harvtmax = c(30, 30, 30, 30, 30, 30, 30, 30, 30, 30),
  Harvsrad = c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200),
  PopuDens = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
  HDI = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
  GDP = c(30000, 30000, 30000, 30000, 30000, 30000, 30000, 30000, 30000, 30000),
  Year = c(2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005)
)
test_result <- stats::predict(rf_ranger, data = test)
test_result$predictions















