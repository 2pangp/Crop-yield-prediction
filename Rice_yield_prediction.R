###predict the rice yield variation under 2c and 4c scenarios

#set the working directory
setwd("~/Desktop/Crop Failure/Research") # mac
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

# Library packages --------------------------------------------------------

library(terra)
library(ggplot2)
library(reshape2)
library(mgcv)
library(caret)
library(ranger)

Country <- vect("./Data/Country/Country.shp")

rf_ranger <- readRDS("./Models/rf_ranger_yield.rds")
areaRas <- rast("./Data/Country/areaRas_30min.tif") * 100 #unit: ha

# Historical scenario -----------------------------------------------------

# 1-in-10-year events, fitted all by Gamma distribution

###Drought 2005
PopuDens_2005 <- rast("./Data/Maps/Anthropogenic/Population_Density/Popu2005.tif")
GDP_2005 <- rast("./Data/Maps/Anthropogenic/GDP/GDPpercap2005.tif")
HDI_2005 <- rast("./Data/Maps/Anthropogenic/HDI/HDI_2005.tif")
Year <- rast(nrows = 360, ncols = 720, crs = crs(Country), extent = c(-180, 180, -90, 90), vals = 2005)
rice_yield <- list()
rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", 2005, ".tif"))
rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", 2005, ".tif"))

for (y in 1990:2015)
{
  predset <- list()
  predset[['major']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_major/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_major/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_major/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_major/Harvsrad_", y, ".tif")), #Harvsrad
    PopuDens_2005,
    HDI_2005,
    GDP_2005,
    Year
  )
  
  predset[['second']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_second/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_second/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_second/Harvsrad_", y, ".tif")), #Harvsrad
    PopuDens_2005,
    HDI_2005,
    GDP_2005,
    Year
  )
  
  names(predset[['major']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  names(predset[['second']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  predset[['major']] <- crop(predset[['major']], rice_yield[['major']], mask = T)
  predset[['second']] <- crop(predset[['second']], rice_yield[['second']], mask = T)
  
  rice_drought_abs <- list()
  rice_drought_abs[['major']] <- predict(object = predset[['major']], model = rf_ranger, na.rm = T)
  rice_drought_abs[['second']] <- predict(object = predset[['second']], model = rf_ranger, na.rm = T)
  
  writeRaster(rice_drought_abs[['major']], paste0("./Data/Maps/Simulations_yield/Drought_2005/Drought_major_abs/Drought_major_abs_", y, ".tif"), overwrite = T)
  writeRaster(rice_drought_abs[['second']], paste0("./Data/Maps/Simulations_yield/Drought_2005/Drought_second_abs/Drought_second_abs_", y, ".tif"), overwrite = T)
  
  print(y)
}


###Heat 1998
PopuDens_1998 <- rast("./Data/Maps/Anthropogenic/Population_Density/Popu1998.tif")
GDP_1998 <- rast("./Data/Maps/Anthropogenic/GDP/GDPpercap1998.tif")
HDI_1998 <- rast("./Data/Maps/Anthropogenic/HDI/HDI_1998.tif")
Year <- rast(nrows = 360, ncols = 720, crs = crs(Country), extent = c(-180, 180, -90, 90), vals = 1998)

rice_yield <- list()
rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", 1998, ".tif"))
rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", 1998, ".tif"))

for (y in 1990:2015)
{
  predset <- list()
  predset[['major']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_major/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_major/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_major/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_major/Harvsrad_", y, ".tif")), #Harvsrad
    PopuDens_1998,
    HDI_1998,
    GDP_1998,
    Year
  )
  
  predset[['second']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_second/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_second/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_second/Harvsrad_", y, ".tif")), #Harvsrad
    PopuDens_1998,
    HDI_1998,
    GDP_1998,
    Year
  )
  
  names(predset[['major']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  names(predset[['second']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  predset[['major']] <- crop(predset[['major']], rice_yield[['major']], mask = T)
  predset[['second']] <- crop(predset[['second']], rice_yield[['second']], mask = T)
  
  rice_heat_abs <- list()
  rice_heat_abs[['major']] <- predict(object = predset[['major']], model = rf_ranger, na.rm = T)
  rice_heat_abs[['second']] <- predict(object = predset[['second']], model = rf_ranger, na.rm = T)
  
  writeRaster(rice_heat_abs[['major']], paste0("./Data/Maps/Simulations_yield/Heat_1998/Heat_major_abs/Heat_major_abs_", y, ".tif"), overwrite = T)
  writeRaster(rice_heat_abs[['second']], paste0("./Data/Maps/Simulations_yield/Heat_1998/Heat_second_abs/Heat_second_abs_", y, ".tif"), overwrite = T)
  
  print(y)
}

###Drought and heat 2015
PopuDens_2015 <- rast("./Data/Maps/Anthropogenic/Population_Density/Popu2015.tif")
GDP_2015 <- rast("./Data/Maps/Anthropogenic/GDP/GDPpercap2015.tif")
HDI_2015 <- rast("./Data/Maps/Anthropogenic/HDI/HDI_2015.tif")
Year <- rast(nrows = 360, ncols = 720, crs = crs(Country), extent = c(-180, 180, -90, 90), vals = 2015)

rice_yield <- list()
rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", 2015, ".tif"))
rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", 2015, ".tif"))

for (y in 1990:2015)
{
  predset <- list()
  predset[['major']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_major/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_major/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_major/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_major/Harvsrad_", y, ".tif")), #Harvsrad
    PopuDens_2015,
    HDI_2015,
    GDP_2015,
    Year
  )
  
  predset[['second']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_second/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_second/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_second/Harvsrad_", y, ".tif")), #Harvsrad
    PopuDens_2015,
    HDI_2015,
    GDP_2015,
    Year
  )
  
  names(predset[['major']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  names(predset[['second']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  predset[['major']] <- crop(predset[['major']], rice_yield[['major']], mask = T)
  predset[['second']] <- crop(predset[['second']], rice_yield[['second']], mask = T)
  
  rice_drought_heat_abs <- list()
  rice_drought_heat_abs[['major']] <- predict(object = predset[['major']], model = rf_ranger, na.rm = T)
  rice_drought_heat_abs[['second']] <- predict(object = predset[['second']], model = rf_ranger, na.rm = T)
  
  writeRaster(rice_drought_heat_abs[['major']], paste0("./Data/Maps/Simulations_yield/Drought_Heat_2015/Drought_Heat_major_abs/Drought_Heat_major_abs_", y, ".tif"), overwrite = T)
  writeRaster(rice_drought_heat_abs[['second']], paste0("./Data/Maps/Simulations_yield/Drought_Heat_2015/Drought_Heat_second_abs/Drought_Heat_second_abs_", y, ".tif"), overwrite = T)
  
  print(y)
}


# 2c scenario -------------------------------------------------------------

# All
for (y in 1990:2015)
{
  rice_yield <- list()
  rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  Year <- rast(nrows = 360, ncols = 720, crs = crs(Country), extent = c(-180, 180, -90, 90), vals = y)
  
  predset <- list()
  predset[['major']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major_2c/GrowPDSI_2c_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_major_2c/Growtmin_2c_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major_2c/Growtmax_2c_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_major/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major_2c/HarvPDSI_2c_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_major_2c/Harvtmin_2c_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major_2c/Harvtmax_2c_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_major/Harvsrad_", y, ".tif")), #Harvsrad
    rast(paste0("./Data/Maps/Anthropogenic/Population_Density/Popu", y, ".tif")), #Population density
    rast(paste0("./Data/Maps/Anthropogenic/HDI/HDI_", y, ".tif")), #HDI
    rast(paste0("./Data/Maps/Anthropogenic/GDP/GDPpercap", y, ".tif")), #GDP
    Year
  )
  predset[['second']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second_2c/GrowPDSI_2c_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_second_2c/Growtmin_2c_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second_2c/Growtmax_2c_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second_2c/HarvPDSI_2c_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_second_2c/Harvtmin_2c_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second_2c/Harvtmax_2c_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_second/Harvsrad_", y, ".tif")), #Harvsrad
    rast(paste0("./Data/Maps/Anthropogenic/Population_Density/Popu", y, ".tif")), #Population density
    rast(paste0("./Data/Maps/Anthropogenic/HDI/HDI_", y, ".tif")), #HDI
    rast(paste0("./Data/Maps/Anthropogenic/GDP/GDPpercap", y, ".tif")), #GDP
    Year
  )
  
  names(predset[['major']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  names(predset[['second']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  predset[['major']] <- crop(predset[['major']], rice_yield[['major']], mask = T)
  predset[['second']] <- crop(predset[['second']], rice_yield[['second']], mask = T)
  
  rice_heat_abs <- list()
  rice_heat_abs[['major']] <- predict(object = predset[['major']], model = rf_ranger, na.rm = T)
  rice_heat_abs[['second']] <- predict(object = predset[['second']], model = rf_ranger, na.rm = T)
  
  writeRaster(rice_heat_abs[['major']], paste0("./Data/Maps/Simulations_yield/Heat_2c/Heat_major_abs/Heat_major_abs_", y, ".tif"), overwrite = T)
  writeRaster(rice_heat_abs[['second']], paste0("./Data/Maps/Simulations_yield/Heat_2c/Heat_second_abs/Heat_second_abs_", y, ".tif"), overwrite = T)

  print(y)
}


# 4c scenario -------------------------------------------------------------

# All
for (y in 1990:2015)
{
  rice_yield <- list()
  rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  Year <- rast(nrows = 360, ncols = 720, crs = crs(Country), extent = c(-180, 180, -90, 90), vals = y)
  
  predset <- list()
  predset[['major']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major_4c/GrowPDSI_4c_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_major_4c/Growtmin_4c_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major_4c/Growtmax_4c_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_major/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major_4c/HarvPDSI_4c_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_major_4c/Harvtmin_4c_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major_4c/Harvtmax_4c_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_major/Harvsrad_", y, ".tif")), #Harvsrad
    rast(paste0("./Data/Maps/Anthropogenic/Population_Density/Popu", y, ".tif")), #Population density
    rast(paste0("./Data/Maps/Anthropogenic/HDI/HDI_", y, ".tif")), #HDI
    rast(paste0("./Data/Maps/Anthropogenic/GDP/GDPpercap", y, ".tif")), #GDP
    Year
  )
  predset[['second']] <- c(
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second_4c/GrowPDSI_4c_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_second_4c/Growtmin_4c_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second_4c/Growtmax_4c_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second_4c/HarvPDSI_4c_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_second_4c/Harvtmin_4c_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second_4c/Harvtmax_4c_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_second/Harvsrad_", y, ".tif")), #Harvsrad
    rast(paste0("./Data/Maps/Anthropogenic/Population_Density/Popu", y, ".tif")), #Population density
    rast(paste0("./Data/Maps/Anthropogenic/HDI/HDI_", y, ".tif")), #HDI
    rast(paste0("./Data/Maps/Anthropogenic/GDP/GDPpercap", y, ".tif")), #GDP
    Year
  )
  
  names(predset[['major']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  names(predset[['second']]) <- c('GrowPDSI', 'Growtmin', 'Growtmax', 'Growsrad', 'HarvPDSI', 'Harvtmin', 'Harvtmax', 'Harvsrad', 'PopuDens', 'HDI', 'GDP', 'Year')
  predset[['major']] <- crop(predset[['major']], rice_yield[['major']], mask = T)
  predset[['second']] <- crop(predset[['second']], rice_yield[['second']], mask = T)
  
  rice_heat_abs <- list()
  rice_heat_abs[['major']] <- predict(object = predset[['major']], model = rf_ranger, na.rm = T)
  rice_heat_abs[['second']] <- predict(object = predset[['second']], model = rf_ranger, na.rm = T)
  
  writeRaster(rice_heat_abs[['major']], paste0("./Data/Maps/Simulations_yield/Heat_4c/Heat_major_abs/Heat_major_abs_", y, ".tif"), overwrite = T)
  writeRaster(rice_heat_abs[['second']], paste0("./Data/Maps/Simulations_yield/Heat_4c/Heat_second_abs/Heat_second_abs_", y, ".tif"), overwrite = T)

  print(y)
}

