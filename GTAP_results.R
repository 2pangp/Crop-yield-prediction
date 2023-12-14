###Figures in master thesis

#set the working directory
setwd("~/Desktop/Crop Failure/Research") # mac
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

# Library packages --------------------------------------------------------

library(readxl)
library(reshape2)
library(terra)
library(ggplot2)
library(viridis)
library(ggpubr)
library(tidyterra)
library(reshape2)

# Importing basic maps. ---------------------------------------------------
Country <- vect("./Data/Country/Country.shp")
names(Country) <- c('Country', 'ISO3', 'ID', 'Region', 'Region_abbr', 'GTAP_region', 'GTAP_code')
Country_GTAP <- vect("./Data/Country_GTAP/Country_GTAP.shp")
names(Country_GTAP) <- c('Country', 'ISO3', 'GTAP_code', 'Region', 'Region_abbr', 'GTAP_region')
Region <- vect("./Data/Region/Region.shp")
names(Region) <- c('Region', 'Region_abbr')
RegionList <- read.csv("./Data/wbesg.csv")
GTAPCode <- read.csv("./Data/GTAPCode.csv")
areaRas = rast("./Data/Country/areaRas_30min.tif") * 100 #unit: ha (after x 100)


# Treatments of world map. ------------------------------------------------
Country <- vect("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/WorldCountries/WorldCountriesWithISO.shp")
Country[Country$COUNTRY == "Ivory Coast",]$ISO3 <- "CIV"

unique(RegionList$Region)
RegionList_abbr <- data.frame(unique(RegionList$Region), c('SA', 'SSA', 'EU', 'MENF', 'LAC', 'CA', 'PAC', 'SEA', 'NM', 'EA'))
colnames(RegionList_abbr) <-c('Region', 'Region_abbr')

Country <- left_join(Country, RegionList, by = 'ISO3')
Country <- left_join(Country, RegionList_abbr, by = 'Region')
Country <- Country[,c('COUNTRY', 'ISO3', 'NUMID', 'Region', 'Region_abbr')]
names(Country) <- c('Country', 'ISO3', 'ID', 'Region', 'Region_abbr')

GTAPCode <- read.csv("./Data/GTAPCode.csv")
Country <- left_join(Country, GTAPCode, by = c('ISO3' = 'GTAP_code'))
Country$GTAP_code <- ifelse(is.na(Country$GTAP_region), Country$Region_abbr, Country$ISO3)

writeVector(Country, "./Data/Country/Country.shp", overwrite = T)

Country_GTAP <- Country %>% 
  select(Country, ISO3, GTAP_code, Region, Region_abbr, GTAP_region) %>% 
  aggregate(., by = 'GTAP_code', FUN = mean) %>% 
  select(Country, ISO3, GTAP_code, Region, Region_abbr, GTAP_region)
writeVector(Country_GTAP, "./Data/Country_GTAP/Country_GTAP.shp", overwrite = T)

Region <- Country %>% 
  select(Region, Region_abbr) %>% 
  aggregate(., by = 'Region_abbr', FUN = mean) %>% 
  select(Region, Region_abbr)
writeVector(Region, "./Data/Region/Region.shp", overwrite = T)

unique(Region$Region_abbr)
unique(Country_GTAP$GTAP_code)

# Generating the shock commands to GTAP. ----------------------------------

# Drought
Drought_var <- read.csv("./Data/Measurements/yield_country_2005.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(GTAP_code, Mean_var)
Drought_commands <- data.frame(paste0('Shock aoall("pdr","', tolower(Drought_var$GTAP_code), '") = ', Drought_var$Mean_var*100, ';'))

Drought_var_region <- read.csv("./Data/Measurements/yield_region_2005.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(Region_abbr, Mean_var)
Drought_commands_region <- data.frame(paste0('Shock aoall("pdr","', tolower(Drought_var_region$Region_abbr), '") = ', Drought_var_region$Mean_var*100, ';'))

# Heat
Heat_var <- read.csv("./Data/Measurements/yield_country_1998.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(GTAP_code, Mean_var)
Heat_commands <- data.frame(paste0('Shock aoall("pdr","', tolower(Heat_var$GTAP_code), '") = ', Heat_var$Mean_var*100, ';'))

Heat_var_region <- read.csv("./Data/Measurements/yield_region_1998.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(Region_abbr, Mean_var)
Heat_commands_region <- data.frame(paste0('Shock aoall("pdr","', tolower(Heat_var_region$Region_abbr), '") = ', Heat_var_region$Mean_var*100, ';'))

# Heat_2c
Heat_var_2c <- read.csv("./Data/Measurements/yield_country_2c.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(GTAP_code, Mean_var)
Heat_2c_commands <- data.frame(paste0('Shock aoall("pdr","', tolower(Heat_var_2c$GTAP_code), '") = ', Heat_var_2c$Mean_var*100, ';'))

Heat_var_2c_region <- read.csv("./Data/Measurements/yield_region_2c.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(Region_abbr, Mean_var)
Heat_2c_commands_region <- data.frame(paste0('Shock aoall("pdr","', tolower(Heat_var_2c_region$Region_abbr), '") = ', Heat_var_2c_region$Mean_var*100, ';'))

# Heat_4c
Heat_var_4c <- read.csv("./Data/Measurements/yield_country_4c.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(GTAP_code, Mean_var)
Heat_4c_commands <- data.frame(paste0('Shock aoall("pdr","', tolower(Heat_var_4c$GTAP_code), '") = ', Heat_var_4c$Mean_var*100, ';'))

Heat_var_4c_region <- read.csv("./Data/Measurements/yield_region_4c.csv") %>% 
  mutate(Mean_var = yield_var) %>% 
  select(Region_abbr, Mean_var)
Heat_4c_commands_region <- data.frame(paste0('Shock aoall("pdr","', tolower(Heat_var_4c_region$Region_abbr), '") = ', Heat_var_4c_region$Mean_var*100, ';'))


GTAP_command <- data.frame(cbind(Drought_commands, Heat_commands, Heat_2c_commands, Heat_4c_commands))
colnames(GTAP_command) <- c('Drought', 'Heat', 'Heat_2c', 'Heat_4c')
write.csv(GTAP_command, "./Data/GTAP_command.csv", row.names = F)

GTAP_command_region <- data.frame(cbind(Drought_commands_region, Heat_commands_region, Heat_2c_commands_region, Heat_4c_commands_region))
colnames(GTAP_command_region) <- c('Drought', 'Heat', 'Heat_2c', 'Heat_4c')
write.csv(GTAP_command_region, "./Data/GTAP_command_region.csv", row.names = F)

# Clean the results from GTAP.
GTAP_clean <- function(df, var)
{
  df <- df[df[,var] %in% c('pdr', 'pcr'),]
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df <- df[-1,]
  df$region <- rownames(df)
  rownames(df) <- NULL
  df <- df[,c('region', colnames(df)[1:2])]
}

GTAP_result <- list()
for (var in c('pim', 'pm', 'qo'))
{
  GTAP_result[[paste0('Drought_', var)]] <- read.csv(paste0("./Data/GTAP/Drought_", var, ".csv"))
  GTAP_result[[paste0('Drought_', var)]] <- GTAP_clean(df = GTAP_result[[paste0('Drought_', var)]], var = var)
  GTAP_result[[paste0('Drought_', var)]]$event <- 'drought'
  GTAP_result[[paste0('Drought_', var)]]$var <- var
  GTAP_result[[paste0('Drought_', var)]]$scenario <- 'historical'
  
  GTAP_result[[paste0('Heat_', var)]] <- read.csv(paste0("./Data/GTAP/Heat_", var, ".csv"))
  GTAP_result[[paste0('Heat_', var)]] <- GTAP_clean(df = GTAP_result[[paste0('Heat_', var)]], var = var)
  GTAP_result[[paste0('Heat_', var)]]$event <- 'heat'
  GTAP_result[[paste0('Heat_', var)]]$var <- var
  GTAP_result[[paste0('Heat_', var)]]$scenario <- 'historical'
  
  GTAP_result[[paste0('Heat_2c_', var)]] <- read.csv(paste0("./Data/GTAP/Heat_2c_", var, ".csv"))
  GTAP_result[[paste0('Heat_2c_', var)]] <- GTAP_clean(df = GTAP_result[[paste0('Heat_2c_', var)]], var = var)
  GTAP_result[[paste0('Heat_2c_', var)]]$event <- 'heat'
  GTAP_result[[paste0('Heat_2c_', var)]]$var <- var
  GTAP_result[[paste0('Heat_2c_', var)]]$scenario <- '2c'
  
  
  GTAP_result[[paste0('Heat_4c_', var)]] <- read.csv(paste0("./Data/GTAP/Heat_4c_", var, ".csv"))
  GTAP_result[[paste0('Heat_4c_', var)]] <- GTAP_clean(df = GTAP_result[[paste0('Heat_4c_', var)]], var = var)
  GTAP_result[[paste0('Heat_4c_', var)]]$event <- 'heat'
  GTAP_result[[paste0('Heat_4c_', var)]]$var <- var
  GTAP_result[[paste0('Heat_4c_', var)]]$scenario <- '4c'
}


GTAP_result <- do.call(rbind, GTAP_result)
GTAP_result <- GTAP_result[,c('region', 'scenario', 'event', 'var', 'pdr', 'pcr')]
GTAP_result <- melt(GTAP_result, id.vars = c('region', 'scenario', 'event', 'var'))
colnames(GTAP_result) <- c('GTAP_code', 'Scenario', 'Event', 'Variable', 'Commodity', 'Variation')
GTAP_result$GTAP_code <- toupper(GTAP_result$GTAP_code)
GTAP_result <- right_join(GTAPCode, GTAP_result, by = 'GTAP_code')
GTAP_result <- right_join(distinct(as.data.frame(Country)[,c('Region_abbr', 'GTAP_code')]), GTAP_result, by = c('GTAP_code'))
GTAP_result$Variation <- as.numeric(GTAP_result$Variation)
GTAP_result$Variation <- ifelse(GTAP_result$Variation < -100, -100, GTAP_result$Variation)
colnames(GTAP_result)[1] <- 'Region'

write.csv(GTAP_result, "./Data/GTAP/GTAP_result.csv", row.names = F)


# Fig. 6 Fig. 7 The results from GTAP. -------------------------------------------
GTAP_result <- read.csv("./Data/GTAP/GTAP_result.csv")
Yield_var <- read.csv("./Data/Yield_var.csv")

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


legend_sample <- GTAP_result %>%
  filter(Scenario == 'historical') %>%
  filter(Event == 'drought') %>%
  filter(Variable == 'pim') %>%
  filter(Commodity == 'pdr') %>% 
  ggplot()+
  geom_bar(aes(x = 0, y = 0, fill = Region), stat = 'identity') +
  scale_fill_manual(values = region_color) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.text = element_text(size = 4),
    legend.key.size = unit(0.3, 'cm')
  )
  
max(GTAP_result$Variation)
min(GTAP_result$Variation)

# Fig. 7

# qo
plot_fig_gtap <- function(scenario, event, variable, commodity, title = '', ytitle = '')
{
  subset <- GTAP_result[(GTAP_result$Scenario == scenario) & (GTAP_result$Event == event) & (GTAP_result$Variable == variable) & (GTAP_result$Commodity == commodity),] %>% 
    arrange(Variation) %>% 
    head(10) %>% 
    mutate(Variation = -Variation)
  
  fig <- ggplot(data <- subset) +
    geom_bar(aes(x = Variation, y = reorder(GTAP_region, Variation, decreasing = F), fill = Region), stat = 'identity') +
    geom_text(aes(x = Variation, y = reorder(GTAP_region, Variation, decreasing = F), label = GTAP_region), 
              hjust = -0.03, size = 1.5, color = 'black') +  # Display labels at the end of each bar
    scale_x_continuous(labels = function(x) paste0(-x), limits = c(0, 130)) +
    scale_y_discrete(labels = sprintf('%#.2f', -c(subset[order(subset$Variation)[1:10], 'Variation']))) +
    scale_fill_manual(values = region_color) +
    ggtitle(title) +
    ylab(ytitle) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5),
      axis.title = element_blank(),
      axis.title.y = element_text(size = 6, angle = 90, vjust = 0),
      axis.ticks = element_line(color = 'black', size = 0.1),
      legend.position = 'none',
      plot.title = element_text(hjust= 0.5, vjust = -1.5, size = 8)
    )
  
  return(fig)
}

fig_7a <- plot_fig_gtap(scenario = 'historical', event = 'drought', variable = 'qo', commodity = 'pdr', title = 'Drought', ytitle = 'Paddy rice')
fig_7b <- plot_fig_gtap(scenario = 'historical', event = 'heat', variable = 'qo', commodity = 'pdr', title = 'Heat')
fig_7c <- plot_fig_gtap(scenario = '2c', event = 'heat', variable = 'qo', commodity = 'pdr', title = 'Heat 2C')
fig_7d <- plot_fig_gtap(scenario = '4c', event = 'heat', variable = 'qo', commodity = 'pdr', title = 'Heat 4C')
fig_7e <- plot_fig_gtap(scenario = 'historical', event = 'drought', variable = 'qo', commodity = 'pcr', ytitle = 'Processed rice')
fig_7f <- plot_fig_gtap(scenario = 'historical', event = 'heat', variable = 'qo', commodity = 'pcr')
fig_7g <- plot_fig_gtap(scenario = '2c', event = 'heat', variable = 'qo', commodity = 'pcr')
fig_7h <- plot_fig_gtap(scenario = '4c', event = 'heat', variable = 'qo', commodity = 'pcr')

fig_7_qo <- ggarrange(NULL, NULL, NULL, NULL, fig_7a, fig_7b, fig_7c, fig_7d, fig_7e, fig_7f, fig_7g, fig_7h,
                   ncol = 4,
                   nrow = 3,
                   heights = c(0.1, 1, 1),
                   labels = c('', '', '', '', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 
                   font.label = list(size = 6)) + 
  theme(
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2)
  )


# pim
plot_fig_gtap <- function(scenario, event, variable, commodity, title = '', ytitle = '')
{
  subset <- GTAP_result[(GTAP_result$Scenario == scenario) & (GTAP_result$Event == event) & (GTAP_result$Variable == variable) & (GTAP_result$Commodity == commodity),] %>% 
    arrange(desc(Variation)) %>% 
    head(10)
  
  fig <- ggplot(data <- subset) +
    geom_bar(aes(x = Variation, y = reorder(GTAP_region, Variation, decreasing = F), fill = Region), stat = 'identity') +
    geom_text(aes(x = Variation, y = reorder(GTAP_region, Variation, decreasing = F), label = GTAP_region), 
              hjust = -0.03, size = 1.5, color = 'black') +  # Display labels at the end of each bar
    scale_x_continuous(limits = c(0, 50)) +
    scale_y_discrete(labels = sprintf('%#.2f', c(subset[order(subset$Variation)[1:10], 'Variation']))) +
    scale_fill_manual(values = region_color) +
    ggtitle(title) +
    ylab(ytitle) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5),
      axis.title = element_blank(),
      axis.title.y = element_text(size = 6, angle = 90, vjust = 0),
      legend.position = 'none',
      axis.ticks = element_line(color = 'black', size = 0.1),
      plot.title = element_text(hjust= 0.5, vjust = -1.5, size = 6)
    )
  
  return(fig)
}

fig_7i <- plot_fig_gtap(scenario = 'historical', event = 'drought', variable = 'pim', commodity = 'pdr', title = '', ytitle = 'Paddy rice')
fig_7j <- plot_fig_gtap(scenario = 'historical', event = 'heat', variable = 'pim', commodity = 'pdr', title = '')
fig_7k <- plot_fig_gtap(scenario = '2c', event = 'heat', variable = 'pim', commodity = 'pdr', title = '')
fig_7l <- plot_fig_gtap(scenario = '4c', event = 'heat', variable = 'pim', commodity = 'pdr', title = '')
fig_7m <- plot_fig_gtap(scenario = 'historical', event = 'drought', variable = 'pim', commodity = 'pcr', ytitle = 'Processed rice')
fig_7n <- plot_fig_gtap(scenario = 'historical', event = 'heat', variable = 'pim', commodity = 'pcr')
fig_7o <- plot_fig_gtap(scenario = '2c', event = 'heat', variable = 'pim', commodity = 'pcr')
fig_7p <- plot_fig_gtap(scenario = '4c', event = 'heat', variable = 'pim', commodity = 'pcr')

fig_7_pim <- ggarrange(NULL, NULL, NULL, NULL, fig_7i, fig_7j, fig_7k, fig_7l, fig_7m, fig_7n, fig_7o, fig_7p,
                      ncol = 4,
                      nrow = 3,
                      heights = c(0.1, 1, 1),
                      labels = c('', '', '', '', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'), 
                      font.label = list(size = 6)) + 
  theme(
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2)
  )


# pm
plot_fig_gtap <- function(scenario, event, variable, commodity, title = '', ytitle = '')
{
  subset <- GTAP_result[(GTAP_result$Scenario == scenario) & (GTAP_result$Event == event) & (GTAP_result$Variable == variable) & (GTAP_result$Commodity == commodity),] %>% 
    arrange(desc(Variation)) %>% 
    head(10)
  
  fig <- ggplot(data <- subset) +
    geom_bar(aes(x = Variation, y = reorder(GTAP_region, Variation, decreasing = F), fill = Region), stat = 'identity') +
    geom_text(aes(x = Variation, y = reorder(GTAP_region, Variation, decreasing = F), label = GTAP_region), 
              hjust = -0.03, size = 1.5, color = 'black') +  # Display labels at the end of each bar
    scale_x_continuous(limits = c(0, 180)) +
    scale_y_discrete(labels = sprintf('%#.2f', c(subset[order(subset$Variation)[1:10], 'Variation']))) +
    scale_fill_manual(values = region_color) +
    ggtitle(title) +
    ylab(ytitle) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5),
      axis.title = element_blank(),
      axis.title.y = element_text(size = 6, angle = 90, vjust = 0),
      legend.position = 'none',
      axis.ticks = element_line(color = 'black', size = 0.1),
      plot.title = element_text(hjust= 0.5, vjust = -1.5, size = 6)
    )
  
  return(fig)
}

fig_7q <- plot_fig_gtap(scenario = 'historical', event = 'drought', variable = 'pm', commodity = 'pdr', title = '', ytitle = 'Paddy rice')
fig_7r <- plot_fig_gtap(scenario = 'historical', event = 'heat', variable = 'pm', commodity = 'pdr', title = '')
fig_7s <- plot_fig_gtap(scenario = '2c', event = 'heat', variable = 'pm', commodity = 'pdr', title = '')
fig_7t <- plot_fig_gtap(scenario = '4c', event = 'heat', variable = 'pm', commodity = 'pdr', title = '')
fig_7u <- plot_fig_gtap(scenario = 'historical', event = 'drought', variable = 'pm', commodity = 'pcr', ytitle = 'Processed rice')
fig_7v <- plot_fig_gtap(scenario = 'historical', event = 'heat', variable = 'pm', commodity = 'pcr')
fig_7w <- plot_fig_gtap(scenario = '2c', event = 'heat', variable = 'pm', commodity = 'pcr')
fig_7x <- plot_fig_gtap(scenario = '4c', event = 'heat', variable = 'pm', commodity = 'pcr')

fig_7_pm <- ggarrange(NULL, NULL, NULL, NULL, fig_7q, fig_7r, fig_7s, fig_7t, fig_7u, fig_7v, fig_7w, fig_7x,
                       ncol = 4,
                       nrow = 3,
                      heights = c(0.1, 1, 1),
                       labels = c('', '', '', '', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x'), 
                       font.label = list(size = 6)) + 
  theme(
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2)
  )

fig_7_xlabel <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Percentage change", size = 2.5) +
  theme_void()

fig_7 <- ggarrange(fig_7_qo, fig_7_pim, fig_7_pm, fig_7_xlabel,
                   ncol = 1,
                   nrow = 4,
                   heights = c(1, 1, 1, 0.05),
                   labels = c('Supply', 'Import price', 'Domestic price', ''), 
                   font.label = list(size = 8),
                   common.legend = T,
                   legend = 'bottom',
                   legend.grob = get_legend(legend_sample))

ggsave("./Figures/Thesis/fig_7.tif", fig_7, device = 'tiff', width = 15, height = 21, units = 'cm', dpi = 600)


# Fig. 8 Price increase vs rice intake. -----------------------------------
cal_intake <- read.csv("./Data/cal_intake.csv")
cal_intake <- cal_intake[,c('Area.Code..M49.', 'Year', 'Value')]
colnames(cal_intake) <- c("ID", "Year", "Intake")
cal_intake <- aggregate(cal_intake, Intake~., FUN = "sum")
cal_intake <- aggregate(cal_intake, Intake~ID, FUN = "mean")
cal_intake[cal_intake$ID == 159, 'ID'] <- 156
cal_intake <- right_join(as.data.frame(Country), cal_intake, by = 'ID')
cal_intake <- cal_intake[!is.na(cal_intake$ISO3),]
cal_intake <- aggregate(cal_intake, Intake~GTAP_code, FUN = 'mean')
cal_intake <- cal_intake[cal_intake$GTAP_code != 'NA',]

rice_intake <- read.csv("./Data/rice_intake.csv")
rice_intake <- rice_intake[,c('Area.Code..M49.', 'Year', 'Value')]
colnames(rice_intake) <- c("ID", "Year", "Intake")
rice_intake <- aggregate(rice_intake, Intake~., FUN = "sum")
rice_intake <- aggregate(rice_intake, Intake~ID, FUN = "mean")
rice_intake[rice_intake$ID == 159, 'ID'] <- 156
rice_intake <- right_join(as.data.frame(Country), rice_intake, by = 'ID')
rice_intake <- rice_intake[!is.na(rice_intake$ISO3),]
rice_intake <- aggregate(rice_intake, Intake~GTAP_code, FUN = 'mean')
rice_intake <- rice_intake[rice_intake$GTAP_code != 'NA',]

rice_intake <- left_join(rice_intake, cal_intake, by = 'GTAP_code')
colnames(rice_intake) <- c('GTAP_code', 'Rice', 'All')
rice_intake$Proportion <- (rice_intake$Rice / rice_intake$All)*100

write.csv(rice_intake, './Data/rice_intake_proportion.csv', row.names = F)

# Drought
rice_intake_drought <- left_join(rice_intake, GTAP_result[(GTAP_result$Scenario == 'historical') &
                                                  (GTAP_result$Event == 'drought') & 
                                                  (GTAP_result$Variable == 'pm') &
                                                  (GTAP_result$Commodity == 'pcr'),
                                                  c("GTAP_code", "GTAP_region", "Region", "Scenario", "Event", "Variable", "Commodity", "Variation")], by = 'GTAP_code')
rice_intake_drought <- rice_intake_drought[,c('Region', 'GTAP_region', 'GTAP_code', 'Proportion', 'Variation')]
colnames(rice_intake_drought) <- c('Region', 'GTAP_region', 'GTAP_code', 'Intake', 'Variation')

# Heat
rice_intake_heat <- left_join(rice_intake, GTAP_result[(GTAP_result$Scenario == 'historical') &
                                                            (GTAP_result$Event == 'heat') & 
                                                            (GTAP_result$Variable == 'pm') &
                                                            (GTAP_result$Commodity == 'pcr'),
                                                       c("GTAP_code", "GTAP_region", "Region", "Scenario", "Event", "Variable", "Commodity", "Variation")], by = 'GTAP_code')
rice_intake_heat <- rice_intake_heat[,c('Region', 'GTAP_region', 'GTAP_code', 'Proportion', 'Variation')]
colnames(rice_intake_heat) <- c('Region', 'GTAP_region', 'GTAP_code', 'Intake', 'Variation')

# Heat 2C
rice_intake_heat_2c <- left_join(rice_intake, GTAP_result[(GTAP_result$Scenario == '2c') &
                                                            (GTAP_result$Event == 'heat') & 
                                                            (GTAP_result$Variable == 'pm') &
                                                            (GTAP_result$Commodity == 'pcr'),
                                                          c("GTAP_code", "GTAP_region", "Region", "Scenario", "Event", "Variable", "Commodity", "Variation")], by = 'GTAP_code')
rice_intake_heat_2c <- rice_intake_heat_2c[,c('Region', 'GTAP_region', 'GTAP_code', 'Proportion', 'Variation')]
colnames(rice_intake_heat_2c) <- c('Region', 'GTAP_region', 'GTAP_code', 'Intake', 'Variation')

# Heat 4C
rice_intake_heat_4c <- left_join(rice_intake, GTAP_result[(GTAP_result$Scenario == '4c') &
                                                            (GTAP_result$Event == 'heat') & 
                                                            (GTAP_result$Variable == 'pm') &
                                                            (GTAP_result$Commodity == 'pcr'),
                                                          c("GTAP_code", "GTAP_region", "Region", "Scenario", "Event", "Variable", "Commodity", "Variation")], by = 'GTAP_code')
rice_intake_heat_4c <- rice_intake_heat_4c[,c('Region', 'GTAP_region', 'GTAP_code', 'Proportion', 'Variation')]
colnames(rice_intake_heat_4c) <- c('Region', 'GTAP_region', 'GTAP_code', 'Intake', 'Variation')

# Fig. 8
ggplot(data = rice_intake_drought) +
  geom_point(aes(y = Intake, x = Variation, color = Region)) +
  geom_text(aes(y = Intake, x = Variation, label = ifelse((Intake > 50) | (Variation > 1), GTAP_region, '')), size = 3, vjust = 1.5) +
  scale_color_manual(values = region_color) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey', linewidth = 0.1, linetype = 2),
    panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.2),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title = element_blank(),
    axis.title.y = element_text(size = 8, angle = 90),
    legend.position = 'none',
    plot.title = element_text(hjust= 0.5)
  )

fig_8 <- ggarrange(fig_7_qo, fig_7_pim, fig_7_pm,
                   ncol = 1,
                   nrow = 4,
                   heights = c(1, 1, 1, 0.1),
                   labels = c('Production', 'Import price', 'Domestic price', ''), 
                   font.label = list(size = 17),
                   common.legend = T,
                   legend = 'bottom',
                   legend.grob = get_legend(legend_sample))


