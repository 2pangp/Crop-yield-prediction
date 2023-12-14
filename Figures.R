###Figures in master thesis

#set the working directory
setwd("~/Desktop/Crop Failure/Research") # mac
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

# Library packages --------------------------------------------------------

library(terra)
library(ggplot2)
library(viridis)
library(ggpubr)
library(tidyterra)
library(ranger)
library(caret)
library(dplyr)

# Importing basic maps. ---------------------------------------------------
Country <- vect("./Data/Country/Country.shp")
names(Country) <- c('Country', 'ISO3', 'ID', 'Region', 'Region_abbr', 'GTAP_region', 'GTAP_code')
Country_GTAP <- vect("./Data/Country_GTAP/Country_GTAP.shp")
names(Country_GTAP) <- c('Country', 'ISO3', 'GTAP_code', 'Region', 'Region_abbr', 'GTAP_region')
Region <- vect("./Data/Region/Region.shp")
names(Region) <- c('Region', 'Region_abbr')
areaRas = rast("./Data/Country/areaRas_30min.tif") * 100 #unit: ha (after x 100)

# Model (Fig. 2) ----------------------------------------------------------

# input the datasets of RiceMajor
RiceMajor <- read.csv("./Data/Rice/RiceMajor.csv")
RiceSecond <- read.csv("./Data/Rice/RiceSecond.csv")

RiceMajor <- na.omit(RiceMajor)
RiceSecond <- na.omit(RiceSecond)

# Data cleaning 

Rice <- rbind(RiceMajor, RiceSecond)
Rice <- left_join(subset(Rice, select = -Region), as.data.frame(Country[,c('ISO3', 'Region')]), by = 'ISO3') # 227212

# Percentiles
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

rf_ranger <- readRDS("./Models/rf_ranger_yield.rds")

###variable importance
varimp <- rf_ranger$variable.importance
varimp <- data.frame(sort(varimp, decreasing = T))
varimp$Variables <- row.names(varimp)
row.names(varimp) <- NULL
colnames(varimp) <- c('Importance', 'Variables')
varimp$Importance_rel <- varimp$Importance / sum(varimp$Importance)
varimp <- varimp[,c('Variables', 'Importance', 'Importance_rel')]

sum(varimp[varimp$Variables %in% c('HDI', 'GDP', 'PopuDens'),]$Importance_rel)
sum(varimp[varimp$Variables %in% c('GrowPDSI', 'HarvPDSI', 'Growtmin', 'Harvtmin', 'Growtmax', 'Harvtmax', 'Growsrad', 'Harvsrad'),]$Importance_rel)


fig_2a <- ggplot(data = varimp) +
  geom_bar(aes(x = Importance_rel, y = reorder(Variables, Importance_rel)), fill = "#003D7C", stat = 'identity') +
  geom_text(aes(x = Importance_rel, y = reorder(Variables, Importance_rel)), size = 2, hjust = 0, label = c(
    'HDI', 
    'Tmax (planting)',
    'Tmax (harvesting)',
    'GDP',
    'Tmin (harvesting)',
    'SRAD (planting)',
    'SRAD (harvesting)',
    'Tmin (planting)',
    'Population density',
    'Year',
    'PDSI (harvesting)',
    'PDSI (planting)'
  )) +
  xlab("Relative importance") +
  ylab("Variables") +
  labs(fill = NULL) +
  xlim(0.00, 0.20) +
  scale_y_discrete(labels = rev(sprintf('%#.2f', varimp$Importance_rel))) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_text(size = 8),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = 'black', linewidth = 0.1, linetype = 2))


# The accuracy of the predictions.
Ranger_pred <- predict(rf_ranger, data = Rice_Tst)
Rice_Tst$Ranger_pred <- Ranger_pred$predictions

# Global
lm_tst <- lm(data = Rice_Tst, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst)
RMSE(Rice_Tst$Ranger_pred, Rice_Tst$Yield) / (max(Rice_Tst$Yield) - min(Rice_Tst$Yield))

fig_2b <- ggplot(data = Rice_Tst) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linewidth = 0.2, linetype = 'dashed', formula = y ~ x - 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  xlab("Actual yield (tons/ha)") +
  ylab("Predicted yield (tons/ha)") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_text(size = 8),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 21, y = 5, label= "Slope: 0.92", size = 2.5) +
  annotate("text", x = 21, y = 3, label= "R-square: 94.97%", size = 2.5) +
  annotate("text", x = 21, y = 1, label= "RMSE: 5.41%", size = 2.5)

# East Asia
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "East Asia",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_c <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_c)
fig_2c <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("East Asia") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.92", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 95.72%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 7.11%", size = 2)

# Europe
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "Europe",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_d <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_d)
fig_2d <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("Europe") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.85", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 91.18%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 10.78%", size = 2)

# Latin America & Caribbean
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "Latin America & Caribbean",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_e <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_e)
fig_2e <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("Latin America & Caribbean") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.95", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 95.75%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 5.15%", size = 2)

# Middle East & North Africa
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "Middle East & North Africa",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_f <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_f)
fig_2f <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("Middle East & North Africa") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.88", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 89.21%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 8.85%", size = 2)

# North America
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "North America",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_g <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_g)
fig_2g <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("North America") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.95", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 97.11%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 11.00%", size = 2)

# South Asia
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "South Asia",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_h <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_h)
fig_2h <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("South Asia") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.93", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 90.76%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 7.28%", size = 2)

# Southeast Asia
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "Southeast Asia",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_i <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_i)
fig_2i <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("Southeast Asia") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.91", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 94.71%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 7.21%", size = 2)

# Sub-Saharan Africa
Rice_Tst_sub <- Rice_Tst[Rice_Tst$Region == "Sub-Saharan Africa",]
RMSE(Rice_Tst_sub$Ranger_pred, Rice_Tst_sub$Yield) / (max(Rice_Tst_sub$Yield) - min(Rice_Tst_sub$Yield))
lm_tst_j <- lm(data = Rice_Tst_sub, formula = Ranger_pred ~ Yield - 1)
summary(lm_tst_j)
fig_2j <- ggplot(data = Rice_Tst_sub) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  ggtitle("Sub-Saharan Africa") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 18, y = 5, label= "Slope: 0.91", size = 2) +
  annotate("text", x = 18, y = 3, label= "R-square: 90.20%", size = 2) +
  annotate("text", x = 18, y = 1, label= "RMSE: 4.17%", size = 2)

fig_2_row2 <- ggarrange(NULL, fig_2c, fig_2d, fig_2e, fig_2f,  
                        NULL, fig_2g, fig_2h, fig_2i, fig_2j,
                        NULL, NULL, NULL, NULL, NULL,
                        nrow = 3, ncol = 5,
                        labels = c('', 'a', 'b', 'c', 'd', '', 'e', 'f', 'g', 'h', '', '', '', '', ''),
                        font.label = list(size = 8),
                        widths = c(0.1, 1.1, 1, 1, 1),
                        heights = c(1, 1, 0.05)
) +
  annotate("text", x = 0.01, y = 0.5, label= "Predicted yield (tons/ha)", size = 3, angle = 90) +
  annotate("text", x = 0.52, y = 0.02, label= "Actual yield (tons/ha)", size = 3)


fig_2_row1 <- ggarrange(fig_2a, fig_2b, nrow = 1, labels = c('a', 'b'), font.label = list(size = 8))
fig_2_row2 <- ggarrange(fig_2c, fig_2d, fig_2e, fig_2f, nrow = 1, widths = c(1.2, 1, 1, 1), labels = c('c', 'd', 'e', 'f'), font.label = list(size = 8))
fig_2 <- ggarrange(fig_2_row1, fig_2_row2, nrow = 2, heights = c(1.1, 1), labels = NA)
ggsave("./Figures/Thesis/fig_2.tif", fig_2, device = 'tiff', width = 14, height = 10, units = 'cm', dpi = 600)

# Regional accuracy of model. ---------------------------------------------



ggsave("./Figures/Thesis/fig_s2.tif", fig_s2, device = 'tiff', width = 14, height = 9, units = 'cm', dpi = 600)


# S1 pdp of SRAD, Tmin, GDP, PopuDens, HDI, and Year. ---------------------------------------

# Partial dependence plots.

pdp_Year <- read.csv("./Data/Pdp/pdp_Year_yield.csv")
pdp_GrowPDSI <- read.csv("./Data/Pdp/pdp_GrowPDSI_yield.csv")
pdp_HarvPDSI <- read.csv("./Data/Pdp/pdp_HarvPDSI_yield.csv")
pdp_Growtmin <- read.csv("./Data/Pdp/pdp_Growtmin_yield.csv")
pdp_Harvtmin <- read.csv("./Data/Pdp/pdp_Harvtmin_yield.csv")
pdp_Growtmax <- read.csv("./Data/Pdp/pdp_Growtmax_yield.csv")
pdp_Harvtmax <- read.csv("./Data/Pdp/pdp_Harvtmax_yield.csv")
pdp_GrowSRAD <- read.csv("./Data/Pdp/pdp_GrowSRAD_yield.csv")
pdp_HarvSRAD <- read.csv("./Data/Pdp/pdp_HarvSRAD_yield.csv")
pdp_GDP <- read.csv("./Data/Pdp/pdp_GDP_yield.csv")
pdp_PopuDens <- read.csv("./Data/Pdp/pdp_PopuDens_yield.csv")
pdp_HDI <- read.csv("./Data/Pdp/pdp_HDI_yield.csv")

fig_s1a <- ggplot(data = pdp_GrowPDSI) +
  geom_point(aes(x = GrowPDSI, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = GrowPDSI, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("PDSI (planting)") +
  ylab("Yield (tons/ha)") +
  xlim(-10, 10) +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title = element_text(size = 8),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1b <- ggplot(data = pdp_HarvPDSI) +
  geom_point(aes(x = HarvPDSI, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = HarvPDSI, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("PDSI (harvesting)") +
  xlim(-10, 10) +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1c <- ggplot(data = pdp_Growtmax) +
  geom_point(aes(x = Growtmax, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = Growtmax, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("Tmax (planting)") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1d <- ggplot(data = pdp_Harvtmax) +
  geom_point(aes(x = Harvtmax, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = Harvtmax, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("Tmax (harvesting)") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1e <- ggplot(data = pdp_GrowSRAD) +
  geom_point(aes(x = Growsrad, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = Growsrad, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("SRAD (planting)") +
  ylab("Yield (tons/ha)") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1f <- ggplot(data = pdp_HarvSRAD) +
  geom_point(aes(x = Harvsrad, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = Harvsrad, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("SRAD (harvesting)") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1g <- ggplot(data = pdp_Growtmin) +
  geom_point(aes(x = Growtmin, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = Growtmin, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("Tmin (planting)") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1h <- ggplot(data = pdp_Harvtmin) +
  geom_point(aes(x = Harvtmin, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = Harvtmin, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("Tmin (harvesting)") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1i <- ggplot(data = pdp_GDP) +
  geom_point(aes(x = GDP, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = GDP, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("GDP") +
  ylab("Yield (tons/ha)") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))
  
fig_s1j <- ggplot(data = pdp_PopuDens) +
  geom_point(aes(x = PopuDens, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = PopuDens, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("Population density") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1k <- ggplot(data = pdp_HDI) +
  geom_point(aes(x = HDI, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = HDI, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("HDI") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1l <- ggplot(data = pdp_Year) +
  geom_point(aes(x = Year, y = yhat), size = 2, colour = "#003D7C", alpha = 0.2, stroke = 0) +
  geom_smooth(aes(x = Year, y = yhat), method = "gam", se = T, color = "navyblue", linetype = 'dashed', formula = y ~ s(x, k = 4)) +
  xlab("Year") +
  ylim(1, 9) +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.1),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2))

fig_s1 <- ggarrange(fig_s1a, fig_s1b, fig_s1c, fig_s1d, 
                    fig_s1e, fig_s1f, fig_s1g, fig_s1h, 
                    fig_s1i, fig_s1j, fig_s1k, fig_s1l, 
                    labels = 'auto', font.label = list(size = 8), 
                    ncol = 4, nrow = 2, widths = c(1.2, 1, 1, 1), heights = c(1, 1))
ggsave("./Figures/Thesis/fig_s1.tif", fig_s1, device = 'tiff', width = 14, height = 10, units = 'cm', dpi = 600)


# The yield losses

viridis_pal()(10)
region_color <- setNames(c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF"),
                         c("LAC", 
                           "SA", 
                           "MENF",
                           "PAC",
                           "CA",
                           "EU",                    
                           "EA",
                           "SEA",
                           "NM",
                           "SSA"))

# Drought (historical 1-in-10-year events) ---------
yield_country <- Country_GTAP
yield_region <- Region
years <- subset(1990:2015, ! 1990:2015 %in% c(1998, 2005, 2015))

for (simu_year in c(2005))
{
  yield_actual <- sum(
    rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", simu_year, ".tif")),
    rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", simu_year, ".tif")),
    na.rm = T
  )
  names(yield_actual) <- paste0('yield_actual_', simu_year)
  
  yield_country <- extract(yield_actual, yield_country, ID = F, bind = T, na.rm = T, fun = mean)
  yield_region <- extract(yield_actual, yield_region, ID = F, bind = T, na.rm = T, fun = mean)
  
  yield_simu <- list()
  yield_var <- list()
  for (y in years)
  {
    yield_simu[[paste0('yield_simu_', y)]] <- sum(
      rast(paste0("./Data/Maps/Simulations_yield/Drought_", simu_year, "/Drought_major_abs/Drought_major_abs_", y, ".tif")),
      rast(paste0("./Data/Maps/Simulations_yield/Drought_", simu_year, "/Drought_second_abs/Drought_second_abs_", y, ".tif")),
      na.rm = T
    )
    yield_simu[[paste0('yield_simu_', y)]] <- lapp(yield_simu[[paste0('yield_simu_', y)]], function(yield) ifelse(yield == 0, NA, yield))
    names(yield_simu[[paste0('yield_simu_', y)]]) <- paste0("yield_simu_", y)
    
    yield_country <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_country, ID = F, bind = T, na.rm = T, fun = mean)
    yield_region <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_region, ID = F, bind = T, na.rm = T, fun = mean) 
    
    yield_var[[paste0("simu_year_", simu_year, "_", y)]] <- (yield_actual - yield_simu[[paste0('yield_simu_', y)]]) / yield_simu[[paste0('yield_simu_', y)]]
    
    print(paste0("simu_year: ", simu_year, " year: ", y))
  }
  
  names(yield_var) <- NULL
  yield_var <- do.call(c, yield_var) %>% 
    mean(na.rm = T)
  names(yield_var) <- paste0("yield_var_", simu_year)
  
  yield_country <- yield_country %>% 
    as.data.frame() %>%
    mutate((.[,paste0('yield_actual_', simu_year)] - .[,paste0('yield_simu_', years)]) / .[,paste0('yield_simu_', years)]) %>% 
    mutate(yield_var = rowMeans(.[paste0('yield_simu_', years)], na.rm = T),
           yield_var_upper = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) + apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T),
           yield_var_lower = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) - apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T)) %>%
    select(Country, ISO3, GTAP_code, Region, Region_abbr, GTAP_region, yield_var, yield_var_upper, yield_var_lower) %>% 
    na.omit() %>%
    arrange(yield_var)
  
  yield_region <- yield_region %>% 
    as.data.frame() %>%
    mutate((.[,paste0('yield_actual_', simu_year)] - .[,paste0('yield_simu_', years)]) / .[,paste0('yield_simu_', years)]) %>% 
    mutate(yield_var = rowMeans(.[paste0('yield_simu_', years)], na.rm = T),
           yield_var_upper = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) + apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T),
           yield_var_lower = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) - apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T)) %>%
    select(Region, Region_abbr, yield_var, yield_var_upper, yield_var_lower) %>% 
    na.omit() %>%
    arrange(yield_var)
  
  write.csv(yield_country, paste0("./Data/Measurements/yield_country_", simu_year, ".csv"), row.names = F)
  write.csv(yield_region, paste0("./Data/Measurements/yield_region_", simu_year, ".csv"), row.names = F)
  
  print(paste0("simu_year: ", simu_year))
}

yield_var_negative <- lapp(yield_var, fun = function (x) ifelse(x < 0, x, NA))
areaRas_drought <- crop(areaRas, yield_var_negative, mask = T)
global(areaRas_drought, fun = 'sum', na.rm = T)
global(areaRas_drought, fun = 'sum', na.rm = T) / global(crop(areaRas, yield_var, mask = T), fun = 'sum', na.rm = T)

# Plot the raster map of yield variation (Fig. 3).

fig_3a <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9),
    breaks = c(-0.9, -0.7, -0.5, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9),
    labels = c('-0.90', '', '', '', '', '', '0.00', '', '', '', '', '', '0.90')
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = c(0.005, 0.01), 
    legend.justification = c(0, 0), 
    legend.box = "horizontal",
    legend.key.size = unit(0.3, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 4),
    legend.box.background = element_rect(color = 'black', fill = NA, size = 0.1)
  )

fig_3b <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 2, nudge_x = 0) +
  coord_sf(expand = F, xlim = c(35, 60), ylim = c(25, 45)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )

fig_3c <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 2) +
  coord_sf(expand = F, xlim = c(65, 93), ylim = c(9, 36)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )

fig_3d <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 2) +
  coord_sf(expand = F, xlim = c(20, 45), ylim = c(-20, 5)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )

fig_3e <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 2) +
  coord_sf(expand = F, xlim = c(-10, 20), ylim = c(-10, 20)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )


decrease_area <- crop(areaRas, lapp(yield_var, fun = function(x) ifelse(x < 0, 1, NA)), mask = T) 
names(decrease_area) <- 'area'
decrease_area <- extract(decrease_area, Region, fun = sum, na.rm = T, bind = T) %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(area = area / sum(area))

fig_3f <- ggplot(data = decrease_area) +
  geom_bar(aes(x = '', y = area, fill = rev(Region_abbr)), position = 'fill', stat = "identity") +
  geom_text(aes(x = 1.2, y = area, label = scales::percent(area, accuracy = 0.01)),
            color = "black", size = 1.5, position = position_fill(vjust = 0.5)) +
  geom_text(aes(x = 1.5, y = area, label = Region_abbr),
            color = "black", size = 1.7, position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = region_color) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.1),
    panel.grid.minor = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.05), 
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )

Region_agg <- yield_region %>%
  mutate(yield_var = -yield_var,
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_3g <- ggplot(Region_agg) +
  geom_bar(aes(x = yield_var, y = reorder(Region_abbr, yield_var, decreasing = F)), stat = 'identity', fill = '#47039FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F), label = Region_abbr), 
            hjust = 0, size = 2, color = 'black') +
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(-0.08, 0.6)) +
  scale_y_discrete(labels = sprintf('%#.2f', -c(Region_agg[order(Region_agg$yield_var),'yield_var']))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )

# filter the top 10 countries according to yield_var
Country_agg <- yield_country %>% 
  head(10) %>% 
  mutate(yield_var = -yield_var, 
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_3h <- ggplot(Country_agg) +
  geom_bar(aes(x = yield_var, y = reorder(GTAP_region, yield_var, decreasing = F)), stat = 'identity', fill = '#366A9FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F), label = GTAP_region), 
            hjust = -0.03, size = 2, color = 'black') + 
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(0, 1)) +
  scale_y_discrete(labels = rev(sprintf('%#.2f', -Country_agg$yield_var))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )


fig_3_row1 <- ggarrange(fig_3a, ncol=  1, nrow = 1, labels = c('a'), font.label = list(size = 8))
fig_3_row2 <- ggarrange(fig_3b, NULL, fig_3c, NULL, fig_3d, NULL, fig_3e, ncol = 7, nrow = 1, widths = c(1, -0.05, 1, -0.07, 1, -0.05, 1), heights = c(1), labels = c('b', '', 'c', '', 'd', '', 'e'), font.label = list(size = 8))
fig_3_row3 <- ggarrange(fig_3f, NULL, fig_3g, NULL, fig_3h, ncol = 5, nrow = 1, widths = c(0.7, 0, 1, -0, 1), heights = c(1), labels = c('f', '', 'g', '', 'h'), font.label = list(size = 8))
fig_3 <- ggarrange(fig_3_row1, fig_3_row2, fig_3_row3, nrow = 3, heights = c(5.22, 3.45, 3.8), widths = 1)
ggsave("./Figures/Thesis/fig_3.tif", fig_3, device = 'tiff', width = 15, height = 12.5, units = 'cm', dpi = 600)

test <- as.data.frame(Country_GTAP)
test1 <- as.data.frame(Country)

# Heat (historical 1-in-10-year events) ---------
yield_country <- Country_GTAP
yield_region <- Region
years <- subset(1990:2015, ! 1990:2015 %in% c(1998, 2005, 2015))

for (simu_year in c(1998))
{
  yield_actual <- sum(
    rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", simu_year, ".tif")),
    rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", simu_year, ".tif")),
    na.rm = T
  )
  names(yield_actual) <- paste0('yield_actual_', simu_year)
  
  yield_country <- extract(yield_actual, yield_country, ID = F, bind = T, na.rm = T, fun = mean)
  yield_region <- extract(yield_actual, yield_region, ID = F, bind = T, na.rm = T, fun = mean)
  
  yield_simu <- list()
  yield_var <- list()
  for (y in years)
  {
    yield_simu[[paste0('yield_simu_', y)]] <- sum(
      rast(paste0("./Data/Maps/Simulations_yield/Heat_", simu_year, "/Heat_major_abs/Heat_major_abs_", y, ".tif")),
      rast(paste0("./Data/Maps/Simulations_yield/Heat_", simu_year, "/Heat_second_abs/Heat_second_abs_", y, ".tif")),
      na.rm = T
    )
    yield_simu[[paste0('yield_simu_', y)]] <- lapp(yield_simu[[paste0('yield_simu_', y)]], function(yield) ifelse(yield == 0, NA, yield))
    names(yield_simu[[paste0('yield_simu_', y)]]) <- paste0("yield_simu_", y)
    
    yield_country <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_country, ID = F, bind = T, na.rm = T, fun = mean)
    yield_region <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_region, ID = F, bind = T, na.rm = T, fun = mean) 
    
    yield_var[[paste0("simu_year_", simu_year, "_", y)]] <- (yield_actual - yield_simu[[paste0('yield_simu_', y)]]) / yield_simu[[paste0('yield_simu_', y)]]
    
    print(paste0("simu_year: ", simu_year, " year: ", y))
  }
  
  names(yield_var) <- NULL
  yield_var <- do.call(c, yield_var) %>% 
    mean(na.rm = T)
  names(yield_var) <- paste0("yield_var_", simu_year)
  
  yield_country <- yield_country %>% 
    as.data.frame() %>%
    mutate((.[,paste0('yield_actual_', simu_year)] - .[,paste0('yield_simu_', years)]) / .[,paste0('yield_simu_', years)]) %>% 
    mutate(yield_var = rowMeans(.[paste0('yield_simu_', years)], na.rm = T),
           yield_var_upper = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) + apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T),
           yield_var_lower = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) - apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T)) %>%
    select(Country, ISO3, GTAP_code, Region, Region_abbr, GTAP_region, yield_var, yield_var_upper, yield_var_lower) %>% 
    na.omit() %>%
    arrange(yield_var)
  
  yield_region <- yield_region %>% 
    as.data.frame() %>%
    mutate((.[,paste0('yield_actual_', simu_year)] - .[,paste0('yield_simu_', years)]) / .[,paste0('yield_simu_', years)]) %>% 
    mutate(yield_var = rowMeans(.[paste0('yield_simu_', years)], na.rm = T),
           yield_var_upper = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) + apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T),
           yield_var_lower = rowMeans(.[paste0('yield_simu_', years)], na.rm = T) - apply(.[paste0('yield_simu_', years)], 1, sd, na.rm = T)) %>%
    select(Region, Region_abbr, yield_var, yield_var_upper, yield_var_lower) %>% 
    na.omit() %>%
    arrange(yield_var)
  
  write.csv(yield_country, paste0("./Data/Measurements/yield_country_", simu_year, ".csv"), row.names = F)
  write.csv(yield_region, paste0("./Data/Measurements/yield_region_", simu_year, ".csv"), row.names = F)
  
  print(paste0("simu_year: ", simu_year))
}

yield_var_negative <- lapp(yield_var, fun = function (x) ifelse(x < 0, x, NA))
areaRas_heat <- crop(areaRas, yield_var_negative, mask = T)
global(areaRas_heat, fun = 'sum', na.rm = T)
global(areaRas_heat, fun = 'sum', na.rm = T) / global(crop(areaRas, yield_var, mask = T), fun = 'sum', na.rm = T)

# Plot the raster map of yield variation (Fig. 3).

fig_4a <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9),
    breaks = c(-0.9, -0.7, -0.5, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9),
    labels = c('-0.90', '', '', '', '', '', '0.00', '', '', '', '', '', '0.90')
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = c(0.005, 0.01), 
    legend.justification = c(0, 0), 
    legend.box = "horizontal",
    legend.key.size = unit(0.3, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 4),
    legend.box.background = element_rect(color = 'black', fill = NA, size = 0.1)
  )

fig_4b <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 2, nudge_x = 0) +
  coord_sf(expand = F, xlim = c(35, 60), ylim = c(25, 45)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )

fig_4c <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 1.2) +
  coord_sf(expand = F, xlim = c(-100, -60), ylim = c(-15, 25)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )

fig_4d <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 2) +
  coord_sf(expand = F, xlim = c(65, 93), ylim = c(9, 36)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )

fig_4e <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  geom_spatvector(data = Country, color = 'lightsteelblue4', fill = NA) +
  geom_spatvector_text(aes(label = ISO3), size = 2) +
  coord_sf(expand = F, xlim = c(90, 110), ylim = c(9, 29)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = 'none'
  )

decrease_area <- crop(areaRas, lapp(yield_var, fun = function(x) ifelse(x < 0, 1, NA)), mask = T) 
names(decrease_area) <- 'area'
decrease_area <- extract(decrease_area, Region, fun = sum, na.rm = T, bind = T) %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(area = area / sum(area))

fig_4f <- ggplot(data = decrease_area) +
  geom_bar(aes(x = '', y = area, fill = rev(Region_abbr)), position = 'fill', stat = "identity") +
  geom_text(aes(x = 1.2, y = area, label = scales::percent(area, accuracy = 0.01)),
            color = "black", size = 1.5, position = position_fill(vjust = 0.5)) +
  geom_text(aes(x = 1.5, y = area, label = Region_abbr),
            color = "black", size = 1.7, position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = region_color) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.1),
    panel.grid.minor = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.05), 
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )


Region_agg <- yield_region %>%
  mutate(yield_var = -yield_var,
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_4g <- ggplot(Region_agg) +
  geom_bar(aes(x = yield_var, y = reorder(Region_abbr, yield_var, decreasing = F)), stat = 'identity', fill = '#47039FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F), label = Region_abbr), 
            hjust = 0, size = 2, color = 'black') +
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(-0.08, 0.6)) +
  scale_y_discrete(labels = sprintf('%#.2f', -c(Region_agg[order(Region_agg$yield_var),'yield_var']))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )

# filter the top 10 countries according to yield_var
Country_agg <- yield_country %>% 
  head(10) %>% 
  mutate(yield_var = -yield_var, 
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_4h <- ggplot(Country_agg) +
  geom_bar(aes(x = yield_var, y = reorder(GTAP_region, yield_var, decreasing = F)), stat = 'identity', fill = '#366A9FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F), label = GTAP_region), 
            hjust = -0.03, size = 2, color = 'black') + 
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(0, 1)) +
  scale_y_discrete(labels = rev(sprintf('%#.2f', -Country_agg$yield_var))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )

fig_3_row1 <- ggarrange(fig_3a, ncol=  1, nrow = 1, labels = c('a'), font.label = list(size = 8))
fig_3_row2 <- ggarrange(fig_3b, NULL, fig_3c, NULL, fig_3d, NULL, fig_3e, ncol = 7, nrow = 1, widths = c(1, -0.05, 1, -0.07, 1, -0.05, 1), heights = c(1), labels = c('b', '', 'c', '', 'd', '', 'e'), font.label = list(size = 8))
fig_3_row3 <- ggarrange(fig_3f, NULL, fig_3g, NULL, fig_3h, ncol = 5, nrow = 1, widths = c(0.7, 0, 1, -0, 1), heights = c(1), labels = c('f', '', 'g', '', 'h'), font.label = list(size = 8))
fig_3 <- ggarrange(fig_3_row1, fig_3_row2, fig_3_row3, nrow = 3, heights = c(5.22, 3.45, 3.8), widths = 1)
ggsave("./Figures/Thesis/fig_3.tif", fig_3, device = 'tiff', width = 15, height = 12.5, units = 'cm', dpi = 600)


fig_4_row1 <- ggarrange(fig_4a, ncol=  1, nrow = 1, labels = c('a'), font.label = list(size = 8))
fig_4_row2 <- ggarrange(fig_4b, NULL, fig_4c, NULL, fig_4d, NULL, fig_4e, ncol = 7, nrow = 1, widths = c(1, -0.05, 1, -0.07, 1, -0.05, 1), heights = c(1), labels = c('b', '', 'c', '', 'd', '', 'e'), font.label = list(size = 8))
fig_4_row3 <- ggarrange(fig_4f, NULL, fig_4g, NULL, fig_4h, ncol = 5, nrow = 1, widths = c(0.7, 0, 1, -0, 1), heights = c(1), labels = c('f', '', 'g', '', 'h'), font.label = list(size = 8))
fig_4 <- ggarrange(fig_4_row1, fig_4_row2, fig_4_row3, nrow = 3, heights = c(5.22, 3.45, 3.8), widths = 1)
ggsave("./Figures/Thesis/fig_4.tif", fig_4, device = 'tiff', width = 15, height = 12.5, units = 'cm', dpi = 600)


# Heat (2c and 4c 1-in-100-year events) ---------

# 2c
yield_var <- list()
yield_simu <- list()
yield_actual <- list()

yield_country_simu <- Country_GTAP
yield_region_simu <- Region
yield_country_actual <- Country_GTAP
yield_region_actual <- Region

for (y in 1990:2015)
{
  yield_simu[[paste0('yield_simu_', y)]] <- sum(
    rast(paste0("./Data/Maps/Simulations_yield/Heat_2c/Heat_major_abs/Heat_major_abs_", y, ".tif")),
    rast(paste0("./Data/Maps/Simulations_yield/Heat_2c/Heat_second_abs/Heat_second_abs_", y, ".tif")),
    na.rm = T
  )
  names(yield_simu[[paste0('yield_simu_', y)]]) <- paste0("yield_simu_", y)
  
  yield_country_simu <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_country_simu, ID = F, bind = T, na.rm = T, fun = mean)
  yield_region_simu <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_region_simu, ID = F, bind = T, na.rm = T, fun = mean)
  
  yield_actual[[paste0('yield_actual_', y)]] <- sum(
    rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif")),
    rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif")),
    na.rm = T
  )
  yield_actual[[paste0('yield_actual_', y)]] <- lapp(yield_actual[[paste0('yield_actual_', y)]], function(yield) ifelse(yield == 0, NA, yield))
  names(yield_actual[[paste0('yield_actual_', y)]]) <- paste0('yield_actual_', y)
  
  yield_country_actual <- extract(yield_actual[[paste0('yield_actual_', y)]], yield_country_actual, ID = F, bind = T, na.rm = T, fun = mean)
  yield_region_actual <- extract(yield_actual[[paste0('yield_actual_', y)]], yield_region_actual, ID = F, bind = T, na.rm = T, fun = mean)
  
  yield_var[[paste0('yield_var_', y)]] <- (yield_simu[[paste0('yield_simu_', y)]] - yield_actual[[paste0('yield_actual_', y)]]) / yield_actual[[paste0('yield_actual_', y)]]
  
  print(paste0(" year: ", y))
}

names(yield_var) <- NULL
yield_var <- do.call(c, yield_var) %>% 
  mean(na.rm = T)
names(yield_var) <- 'yield_var_2c'

yield_var_negative <- lapp(yield_var, fun = function (x) ifelse(x < 0, x, NA))
areaRas_drought <- crop(areaRas, yield_var_negative, mask = T)
global(areaRas_drought, fun = 'sum', na.rm = T)
global(areaRas_drought, fun = 'sum', na.rm = T) / global(crop(areaRas, yield_var, mask = T), fun = 'sum', na.rm = T)

# Countries
yield_country_simu <- as.data.frame(yield_country_simu)
yield_country_actual <- as.data.frame(yield_country_actual)
yield_country <- (yield_country_simu[,paste0('yield_simu_', 1990:2015)] - yield_country_actual[,paste0('yield_actual_', 1990:2015)]) / yield_country_actual[,paste0('yield_actual_', 1990:2015)]
colnames(yield_country) <- paste0('yield_var_', 1990:2015)
yield_country <- cbind(as.data.frame(Country_GTAP), yield_country)

yield_country <- yield_country %>% 
  mutate(yield_var = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T),
         yield_var_upper = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) + apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T),
         yield_var_lower = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) - apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T)) %>%
  select(Country, ISO3, GTAP_code, Region, Region_abbr, GTAP_region, yield_var, yield_var_upper, yield_var_lower) %>% 
  na.omit() %>%
  arrange(yield_var)

write.csv(yield_country, "./Data/Measurements/yield_country_2c.csv", row.names = F)

# Regions
yield_region_simu <- as.data.frame(yield_region_simu)
yield_region_actual <- as.data.frame(yield_region_actual)
yield_region <- (yield_region_simu[,paste0('yield_simu_', 1990:2015)] - yield_region_actual[,paste0('yield_actual_', 1990:2015)]) / yield_region_actual[,paste0('yield_actual_', 1990:2015)]
colnames(yield_region) <- paste0('yield_var_', 1990:2015)
yield_region <- cbind(as.data.frame(Region), yield_region)

yield_region <- yield_region %>% 
  mutate(yield_var = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T),
         yield_var_upper = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) + apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T),
         yield_var_lower = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) - apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T)) %>%
  select(Region, Region_abbr, yield_var, yield_var_upper, yield_var_lower) %>% 
  na.omit() %>%
  arrange(yield_var)

write.csv(yield_region, "./Data/Measurements/yield_region_2c.csv", row.names = F)

# Plot the raster map of yield variation (Fig. 5).

fig_5a <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9),
    breaks = c(-0.9, -0.7, -0.5, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9),
    labels = c('-0.90', '', '', '', '', '', '0.00', '', '', '', '', '', '0.90')
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = c(0.005, 0.01), 
    legend.justification = c(0, 0), 
    legend.box = "horizontal",
    legend.key.size = unit(0.3, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 4),
    legend.box.background = element_rect(color = 'black', fill = NA, size = 0.1)
  )

decrease_area <- crop(areaRas, lapp(yield_var, fun = function(x) ifelse(x < 0, 1, NA)), mask = T) 
names(decrease_area) <- 'area'
decrease_area <- extract(decrease_area, Region, fun = sum, na.rm = T, bind = T) %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(area = area / sum(area))

fig_5c <- ggplot(data = decrease_area) +
  geom_bar(aes(x = '', y = area, fill = rev(Region_abbr)), position = 'fill', stat = "identity") +
  geom_text(aes(x = 1.2, y = area, label = scales::percent(area, accuracy = 0.01)),
            color = "black", size = 1.5, position = position_fill(vjust = 0.5)) +
  geom_text(aes(x = 1.5, y = area, label = Region_abbr),
            color = "black", size = 1.7, position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = region_color) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.1),
    panel.grid.minor = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.05), 
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )

Region_agg <- yield_region %>%
  mutate(yield_var = -yield_var,
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_5d <- ggplot(Region_agg) +
  geom_bar(aes(x = yield_var, y = reorder(Region_abbr, yield_var, decreasing = F)), stat = 'identity', fill = '#47039FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F), label = Region_abbr), 
            hjust = 0, size = 2, color = 'black') +
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(-1.0, 0.4)) +
  scale_y_discrete(labels = sprintf('%#.2f', -c(Region_agg[order(Region_agg$yield_var),'yield_var']))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )


# filter the top 10 countries according to yield_var
Country_agg <- yield_country %>% 
  head(10) %>% 
  mutate(yield_var = -yield_var, 
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_5e <- ggplot(Country_agg) +
  geom_bar(aes(x = yield_var, y = reorder(GTAP_region, yield_var, decreasing = F)), stat = 'identity', fill = '#366A9FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F), label = GTAP_region), 
            hjust = -0.03, size = 2, color = 'black') + 
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(0, 0.7)) +
  scale_y_discrete(labels = rev(sprintf('%#.2f', -Country_agg$yield_var))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )


# 4c
yield_var <- list()
yield_simu <- list()
yield_actual <- list()

yield_country_simu <- Country_GTAP
yield_region_simu <- Region
yield_country_actual <- Country_GTAP
yield_region_actual <- Region

for (y in 1990:2015)
{
  yield_simu[[paste0('yield_simu_', y)]] <- sum(
    rast(paste0("./Data/Maps/Simulations_yield/Heat_4c/Heat_major_abs/Heat_major_abs_", y, ".tif")),
    rast(paste0("./Data/Maps/Simulations_yield/Heat_4c/Heat_second_abs/Heat_second_abs_", y, ".tif")),
    na.rm = T
  )
  names(yield_simu[[paste0('yield_simu_', y)]]) <- paste0("yield_simu_", y)
  
  yield_country_simu <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_country_simu, ID = F, bind = T, na.rm = T, fun = mean)
  yield_region_simu <- extract(yield_simu[[paste0('yield_simu_', y)]], yield_region_simu, ID = F, bind = T, na.rm = T, fun = mean)
  
  yield_actual[[paste0('yield_actual_', y)]] <- sum(
    rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif")),
    rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif")),
    na.rm = T
  )
  yield_actual[[paste0('yield_actual_', y)]] <- lapp(yield_actual[[paste0('yield_actual_', y)]], function(yield) ifelse(yield == 0, NA, yield))
  names(yield_actual[[paste0('yield_actual_', y)]]) <- paste0('yield_actual_', y)
  
  yield_country_actual <- extract(yield_actual[[paste0('yield_actual_', y)]], yield_country_actual, ID = F, bind = T, na.rm = T, fun = mean)
  yield_region_actual <- extract(yield_actual[[paste0('yield_actual_', y)]], yield_region_actual, ID = F, bind = T, na.rm = T, fun = mean)
  
  yield_var[[paste0('yield_var_', y)]] <- (yield_simu[[paste0('yield_simu_', y)]] - yield_actual[[paste0('yield_actual_', y)]]) / yield_actual[[paste0('yield_actual_', y)]]
  
  print(paste0(" year: ", y))
}

names(yield_var) <- NULL
yield_var <- do.call(c, yield_var) %>% 
  mean(na.rm = T)
names(yield_var) <- 'yield_var_4c'

yield_var_negative <- lapp(yield_var, fun = function (x) ifelse(x < 0, x, NA))
areaRas_drought <- crop(areaRas, yield_var_negative, mask = T)
global(areaRas_drought, fun = 'sum', na.rm = T)
global(areaRas_drought, fun = 'sum', na.rm = T) / global(crop(areaRas, yield_var, mask = T), fun = 'sum', na.rm = T)

# Countries
yield_country_simu <- as.data.frame(yield_country_simu)
yield_country_actual <- as.data.frame(yield_country_actual)
yield_country <- (yield_country_simu[,paste0('yield_simu_', 1990:2015)] - yield_country_actual[,paste0('yield_actual_', 1990:2015)]) / yield_country_actual[,paste0('yield_actual_', 1990:2015)]
colnames(yield_country) <- paste0('yield_var_', 1990:2015)
yield_country <- cbind(as.data.frame(Country_GTAP), yield_country)

yield_country <- yield_country %>% 
  mutate(yield_var = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T),
         yield_var_upper = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) + apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T),
         yield_var_lower = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) - apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T)) %>%
  select(Country, ISO3, GTAP_code, Region, Region_abbr, GTAP_region, yield_var, yield_var_upper, yield_var_lower) %>% 
  na.omit() %>%
  arrange(yield_var)

write.csv(yield_country, './Data/Measurements/yield_country_4c.csv', row.names = F)

# Regions
yield_region_simu <- as.data.frame(yield_region_simu)
yield_region_actual <- as.data.frame(yield_region_actual)
yield_region <- (yield_region_simu[,paste0('yield_simu_', 1990:2015)] - yield_region_actual[,paste0('yield_actual_', 1990:2015)]) / yield_region_actual[,paste0('yield_actual_', 1990:2015)]
colnames(yield_region) <- paste0('yield_var_', 1990:2015)
yield_region <- cbind(as.data.frame(Region), yield_region)

yield_region <- yield_region %>% 
  mutate(yield_var = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T),
         yield_var_upper = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) + apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T),
         yield_var_lower = rowMeans(.[paste0('yield_var_', 1990:2015)], na.rm = T) - apply(.[paste0('yield_var_', 1990:2015)], 1, sd, na.rm = T)) %>%
  select(Region, Region_abbr, yield_var, yield_var_upper, yield_var_lower) %>% 
  na.omit() %>%
  arrange(yield_var)

write.csv(yield_region, './Data/Measurements/yield_region_4c.csv', row.names = F)

# Plot the raster map of yield variation (Fig. 5).

fig_5b <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = yield_var) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.9, 0.9),
    breaks = c(-0.9, -0.7, -0.5, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9),
    labels = c('-0.90', '', '', '', '', '', '0.00', '', '', '', '', '', '0.90')
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4), 
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.line = element_blank(),
    legend.position = c(0.005, 0.01), 
    legend.justification = c(0, 0), 
    legend.box = "horizontal",
    legend.key.size = unit(0.3, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 4),
    legend.box.background = element_rect(color = 'black', fill = NA, size = 0.1)
  )

decrease_area <- crop(areaRas, lapp(yield_var, fun = function(x) ifelse(x < 0, 1, NA)), mask = T) 
names(decrease_area) <- 'area'
decrease_area <- extract(decrease_area, Region, fun = sum, na.rm = T, bind = T) %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(area = area / sum(area))

fig_5f <- ggplot(data = decrease_area) +
  geom_bar(aes(x = '', y = area, fill = rev(Region_abbr)), position = 'fill', stat = "identity") +
  geom_text(aes(x = 1.2, y = area, label = scales::percent(area, accuracy = 0.01)),
            color = "black", size = 1.5, position = position_fill(vjust = 0.5)) +
  geom_text(aes(x = 1.5, y = area, label = Region_abbr),
            color = "black", size = 1.7, position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = region_color) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.1),
    panel.grid.minor = element_line(linetype = 'dashed', color = 'gray', linewidth = 0.05), 
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )


Region_agg <- yield_region %>%
  mutate(yield_var = -yield_var,
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_5g <- ggplot(Region_agg) +
  geom_bar(aes(x = yield_var, y = reorder(Region_abbr, yield_var, decreasing = F)), stat = 'identity', fill = '#47039FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(Region_abbr, yield_var, decreasing = F), label = Region_abbr), 
            hjust = 0, size = 2, color = 'black') +
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(-1.0, 0.45)) +
  scale_y_discrete(labels = sprintf('%#.2f', -c(Region_agg[order(Region_agg$yield_var),'yield_var']))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )

# filter the top 10 countries according to yield_var
Country_agg <- yield_country %>% 
  head(10) %>% 
  mutate(yield_var = -yield_var, 
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_5h <- ggplot(Country_agg) +
  geom_bar(aes(x = yield_var, y = reorder(GTAP_region, yield_var, decreasing = F)), stat = 'identity', fill = '#366A9FFF') +
  geom_errorbar(aes(xmin = yield_var_upper, xmax = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F)), width=0.2, colour="orange", alpha=0.7, size=0.2) +
  geom_text(aes(x = yield_var_lower, y = reorder(GTAP_region, yield_var, decreasing = F), label = GTAP_region), 
            hjust = -0.03, size = 2, color = 'black') + 
  scale_x_continuous(labels = function(x) paste0(-x), limits = c(0, 0.7)) +
  scale_y_discrete(labels = rev(sprintf('%#.2f', -Country_agg$yield_var))) +
  xlab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank()
  )

fig_5_row1 <- ggarrange(fig_5a, ncol=  1, nrow = 1, labels = c('a'), font.label = list(size = 8))
fig_5_row2 <- ggarrange(fig_5b, ncol=  1, nrow = 1, labels = c('b'), font.label = list(size = 8))
fig_5_row3 <- ggarrange(fig_5c, NULL, fig_5d, NULL, fig_5e, ncol = 5, nrow = 1, widths = c(0.7, 0, 1, -0, 1), heights = c(1), labels = c('c', '', 'd', '', 'e'), font.label = list(size = 8))
fig_5_row4 <- ggarrange(fig_5f, NULL, fig_5g, NULL, fig_5h, ncol = 5, nrow = 1, widths = c(0.7, 0, 1, -0, 1), heights = c(1), labels = c('f', '', 'g', '', 'h'), font.label = list(size = 8))
fig_5 <- ggarrange(fig_5_row1, fig_5_row2, fig_5_row3, fig_5_row4, nrow = 4, heights = c(5.5, 5.5, 3.9, 3.9), widths = 1)
ggsave("./Figures/Thesis/fig_5.tif", fig_5, device = 'tiff', width = 15, height = 19, units = 'cm', dpi = 600)



# Fig. 6 The comparison between and inside each region. -------------------

yield_region_drought <- read.csv('./Data/Measurements/yield_region_2005.csv') %>% 
  mutate(events = 'drought')
yield_region_heat <- read.csv('./Data/Measurements/yield_region_1998.csv') %>% 
  mutate(events = 'heat')
yield_region_heat_2c <- read.csv('./Data/Measurements/yield_region_2c.csv') %>% 
  mutate(events = 'heat 2°C')
yield_region_heat_4c <- read.csv('./Data/Measurements/yield_region_4c.csv') %>% 
  mutate(events = 'heat 4°C')
yield_region <- rbind(yield_region_drought, yield_region_heat, yield_region_heat_2c, yield_region_heat_4c) %>% 
  mutate(yield_var = -yield_var,
         yield_var_lower = -yield_var_lower,
         yield_var_upper = -yield_var_upper)

fig_6 <- ggplot(yield_region) +
  geom_bar(aes(x = Region_abbr, y = yield_var, fill = events), 
           stat = 'identity', position = position_dodge(0.7)) +
  scale_x_discrete(limits = c('SA', 'EA', 'SEA', 'SSA', 'LAC', 'NM', 'EU', 'MENF', 'CA', 'PAC')) +
  scale_y_continuous(labels = function(x) paste0(-x), limits = c(-0.7, 0.55)) +
  scale_fill_viridis(discrete = T, option = 'inferno', direction = -1) +
  ylab('Yield variation') +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.ticks.x = element_line(linewidth = 0.1),
    axis.ticks.y = element_blank(),
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.text = element_text(size = 6)
  )
ggsave("./Figures/Thesis/fig_6.tif", fig_6, device = 'tiff', width = 15, height = 9, units = 'cm', dpi = 600)


# The spatial distributions of climatic factors in selected years. --------

# Define a function to draw the figures
fig_climate <- function(climate_factor, years, scenarios = '', value_min, value_max, unit = '', direction = -1)
{
  fig <- list()
  rice_yield <- list()
  factor <- list()

  for (year in years)
  {
    rice_yield[[paste0('major', year)]] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", year, ".tif"))
    rice_yield[[paste0('second', year)]] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", year, ".tif"))

    factor[[paste0('Grow_major', year)]] <- rast(paste0("./Data/Maps/Agroclimate/Grow", climate_factor, "_rice_major", scenarios, "/Grow", climate_factor, scenarios, "_", year, ".tif")) 
    factor[[paste0('Grow_second', year)]] <- rast(paste0("./Data/Maps/Agroclimate/Grow", climate_factor, "_rice_major", scenarios, "/Grow", climate_factor, scenarios, "_", year, ".tif"))
    factor[[paste0('Harv_major', year)]] <- rast(paste0("./Data/Maps/Agroclimate/Harv", climate_factor, "_rice_major", scenarios, "/Harv", climate_factor, scenarios, "_", year, ".tif")) 
    factor[[paste0('Harv_second', year)]] <- rast(paste0("./Data/Maps/Agroclimate/Harv", climate_factor, "_rice_major", scenarios, "/Harv", climate_factor, scenarios, "_", year, ".tif"))
    
    factor[[paste0('Grow_major', year)]] <- crop(factor[[paste0('Grow_major', year)]], rice_yield[[paste0('major', year)]], mask = T)
    factor[[paste0('Grow_second', year)]] <- crop(factor[[paste0('Grow_second', year)]], rice_yield[[paste0('second', year)]], mask = T)
    factor[[paste0('Harv_major', year)]] <- crop(factor[[paste0('Harv_major', year)]], rice_yield[[paste0('major', year)]], mask = T)
    factor[[paste0('Harv_second', year)]] <- crop(factor[[paste0('Harv_second', year)]], rice_yield[[paste0('second', year)]], mask = T)
  }
  
  factor[['Grow_major']] <- factor[paste0('Grow_major', years)] %>% 
    unname() %>% 
    do.call(c, .) %>% 
    mean(na.rm = T)
  factor[['Grow_second']] <- factor[paste0('Grow_second', years)] %>% 
    unname() %>% 
    do.call(c, .) %>% 
    mean(na.rm = T)
  factor[['Harv_major']] <- factor[paste0('Harv_major', years)] %>% 
    unname() %>% 
    do.call(c, .) %>% 
    mean(na.rm = T)
  factor[['Harv_second']] <- factor[paste0('Harv_second', years)] %>% 
    unname() %>% 
    do.call(c, .) %>% 
    mean(na.rm = T)
  
  fig[['Grow_major']] <- ggplot(Country) +
    geom_spatvector(color = 'ivory2', fill = 'ivory2') +
    geom_spatraster(data = factor[['Grow_major']]) +
    labs(fill = unit) +
    coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
    scale_fill_viridis(
      na.value = NA, 
      limits = c(value_min, value_max),
      direction = direction
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
      axis.text = element_text(size = 4),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'bottom',
      legend.title = element_text(size = 4),
      legend.text = element_text(size = 3.5),
      legend.key.size = unit(0.3, 'cm')
    )
  fig[['Grow_second']] <- ggplot(Country) +
    geom_spatvector(color = 'ivory2', fill = 'ivory2') +
    geom_spatraster(data = factor[['Grow_second']]) +
    coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
    scale_fill_viridis(
      na.value = NA, 
      limits = c(value_min, value_max),
      direction = direction
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'none'
    )
  fig[['Harv_major']] <- ggplot(Country) +
    geom_spatvector(color = 'ivory2', fill = 'ivory2') +
    geom_spatraster(data = factor[['Harv_major']]) +
    labs(fill = unit) +
    coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
    scale_fill_viridis(
      na.value = NA, 
      limits = c(value_min, value_max),
      direction = direction
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
      axis.text = element_text(size = 4),
      axis.ticks = element_blank(),
      legend.position = 'none'
    )
  fig[['Harv_second']] <- ggplot(Country) +
    geom_spatvector(color = 'ivory2', fill = 'ivory2') +
    geom_spatraster(data = factor[['Harv_second']]) +
    coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
    scale_fill_viridis(
      na.value = NA, 
      limits = c(value_min, value_max),
      direction = direction
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
      axis.text = element_text(size = 4),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'none'
    )
  
  fig_result <- ggarrange(fig[['Grow_major']], fig[['Grow_second']], fig[['Harv_major']], fig[['Harv_second']], 
                          ncol = 2, nrow = 2, widths = c(1.05, 1), common.legend = T, legend = 'bottom',
                          labels = 'auto', font.label = list(size = 6, color = 'black'))
  return(fig_result)
}

fig_s3 <- fig_climate(climate_factor = 'PDSI', years = c(2005), value_min = -5, value_max = 5, unit = 'PDSI', direction = -1)
fig_s4 <- fig_climate(climate_factor = 'tmax', years = c(1998), value_min = 15, value_max = 40, unit = '°C', direction = 1)
fig_s5 <- fig_climate(climate_factor = 'tmax', years = c(1990:2015), value_min = 15, value_max = 40, scenarios = '_2c', unit = '°C', direction = 1)
fig_s6 <- fig_climate(climate_factor = 'tmax', years = c(1990:2015), value_min = 15, value_max = 40, scenarios = '_4c', unit = '°C', direction = 1)

ggsave("./Figures/Thesis/fig_s3.tif", fig_s3, device = 'tiff', width = 15, height = 7, units = 'cm', dpi = 600)
ggsave("./Figures/Thesis/fig_s4.tif", fig_s4, device = 'tiff', width = 15, height = 7, units = 'cm', dpi = 600)
ggsave("./Figures/Thesis/fig_s5.tif", fig_s5, device = 'tiff', width = 15, height = 7, units = 'cm', dpi = 600)
ggsave("./Figures/Thesis/fig_s6.tif", fig_s6, device = 'tiff', width = 15, height = 7, units = 'cm', dpi = 600)


# Socio-economic schematic
gdp_2015 <- rast("./Data/Maps/Anthropogenic/GDP/GDPpercap2015.tif")
gdp_schem <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = gdp_2015) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_viridis(
    na.value = NA, 
    direction = -1,
    option = 'C'
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )

hdi_2015 <- rast("./Data/Maps/Anthropogenic/HDI/HDI_2015.tif")
# hdi_schem <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = hdi_2015) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_viridis(
    na.value = NA, 
    direction = -1,
    option = 'H'
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )



pop_2015 <- rast("./Data/Maps/Anthropogenic/Population_Density/Popu2015.tif")
pop_schem <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = pop_2015) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_viridis(
    na.value = NA, 
    direction = -1,
    option = 'A',
    limits = c(10, 15000)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text = element_text(size = 4),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )


# Drought (2004, 2005, and 2015)


fig_s8a <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = rice_var_major) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.1, 0.15),
    breaks = c(-0.1, -0.05, -0.02, -0.01, 0, 0.01, 0.02, 0.05, 0.1, 0.15),
    values = scales::rescale(c(-0.1, -0.05, -0.02, -0.01, 0, 0.01, 0.02, 0.05, 0.1, 0.15)),
    labels = c('-0.10', '', '', '', '0.00', '', '', '', '0.10', '0.10+')
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.3),
axis.text.x = element_text(size = 7, hjust = 0.5, vjust = 0.5),     axis.text.y = element_text(size = 7), 
    axis.text.x = element_blank(),
    legend.position = 'none'
  )

fig_s8b <- ggplot(Country) +
  geom_spatvector(color = 'ivory2', fill = 'ivory2') +
  geom_spatraster(data = rice_var_second) +
  coord_sf(expand = F, xlim = c(-130, 150), ylim = c(-35, 55)) +
  scale_fill_gradientn(
    colours = c("#EF7C00", "white", "#003D7C"),
    na.value = NA, 
    limits = c(-0.1, 0.15),
    breaks = c(-0.1, -0.05, -0.02, -0.01, 0, 0.01, 0.02, 0.05, 0.1, 0.15),
    values = scales::rescale(c(-0.1, -0.05, -0.02, -0.01, 0, 0.01, 0.02, 0.05, 0.1, 0.15)),
    labels = c('-0.10', '', '', '', '0.00', '', '', '', '0.10', '0.10+')
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.3),
axis.text.x = element_text(size = 7, hjust = 0.5, vjust = 0.5),     axis.text.y = element_text(size = 7), 
    legend.position = c(0.005, 0.01), 
    legend.justification = c(0, 0), 
    legend.box = "horizontal",
    legend.key.size = unit(1, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 4),
    legend.box.background = element_rect(color = 'black', fill = NA, size = 0.2)
  )

fig_s2 <- ggarrange(fig_s2a, fig_s2b, nrow = 2, labels = 'auto')
ggsave("./Figures/Thesis/fig_s2.tif", fig_s2, device = 'tiff', width = 3840 * 2, height = 2160 * 2.5, units = 'px', dpi = 600)

# Heat (1998, 2009, 2010)

fig_s3a <- ggplot(data = varimp) +
  geom_bar(aes(x = Importance_rel, y = reorder(Variables, Importance_rel)), fill = "#003D7C", stat = 'identity') +
  xlab("Relative importance") +
  ylab("Variables") +
  scale_y_discrete(labels = c(
    'HDI',
    'Tmax (planting)',
    'Tmax (harvesting)',
    'GDP',
    'Tmin (harvesting)',
    'SRAD (planting)',
    'Tmin (planting)',
    'SRAD (harvesting)',
    'Population density',
    'Year',
    'PDSI (harvesting)',
    'PDSI (planting)'  
  )) +
  labs(fill = NULL) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.3),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = 'black', linewidth = 0.1, linetype = 2),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 10))



fig_s3b <- ggplot(data = Rice_Tst) +
  geom_point(aes(x = Yield, y = Ranger_pred), size = 2, colour = "#003D7C", alpha = 0.05, stroke = 0) +
  geom_smooth(aes(x = Yield, y = Ranger_pred), method = "lm", se = T, color = "#EF7C00", linetype = 'dashed', formula = y ~ x - 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "ivory4") +
  xlab("Yield (actual)") +
  ylab("Yield (predicted)") +
  # xlim(-1.00, 0.82) +
  # ylim(-0.87, 0.69) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.3),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.line = element_line(color = 'black', linewidth = 0.2, linetype = 1),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1, linetype = 2)) +
  annotate("text", x = 20, y = 5, label= "Slope: 0.9221", size = 5) +
  annotate("text", x = 20, y = 3, label= "R-square: 0.9497", size = 5) +
  annotate("text", x = 20, y = 1, label= "RMSE: 5.26%", size = 5)





