# Library and working directory setting -----------------------------------
setwd("/Users/zhangfengqi/Desktop/Crop Failure/Research") # mac
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

#library
library(terra)
library(MASS)
library(fitdistrplus)
library(PearsonDS)
library(goft)

# Calculation of average drought index in growing season ------------------

###calculate the drought index per growing season
areaRas = rast("./Data/Country/areaRas_30min.tif") * 100 #unit: ha (after x 100)

#Function to calculate annual weighted average pdsi and maximum temperature
#The input spatraster (pdsi and tmax) must be a list with 2 items. The first item named "major" and the second item named "second", corresponding to average pdsi or tmax in specific stage.
sync_drought <- function(pdsi, ref)
{
  drought <- list()
  drought[['major']] <- lapp(pdsi[['major']], fun = function(pdsi) ifelse(pdsi < -1, -pdsi, 0))
  drought[['second']] <- lapp(pdsi[['second']], fun = function(pdsi) ifelse(pdsi < -1, -pdsi, 0))
  
  area <- list()
  area[['major']] <- crop(areaRas, ref[['major']], mask = T)
  area[['second']] <- crop(areaRas, ref[['second']], mask = T)
  
  total_area  <- global(area[['major']], 'sum', na.rm = T)$sum + global(area[['second']], 'sum', na.rm = T)$sum
  drought[['major']] <- (area[['major']] / total_area) * drought[['major']]
  drought[['second']] <- (area[['second']] / total_area) * drought[['second']]
  
  drought_idx <- global(drought[['major']], 'sum', na.rm = T)$sum + global(drought[['second']], 'sum', na.rm = T)$sum
  
  return(drought_idx)
}

sync_flood <- function(pdsi, ref)
{
  flood <- list()
  flood[['major']] <- lapp(pdsi[['major']], fun = function(pdsi) ifelse(pdsi > 0.5, pdsi, 0))
  flood[['second']] <- lapp(pdsi[['second']], fun = function(pdsi) ifelse(pdsi > 0.5, pdsi, 0))

  area <- list()
  area[['major']] <- crop(areaRas, ref[['major']], mask = T)
  area[['second']] <- crop(areaRas, ref[['second']], mask = T)

  total_area  <- global(area[['major']], 'sum', na.rm = T)$sum + global(area[['second']], 'sum', na.rm = T)$sum
  flood[['major']] <- (area[['major']] / total_area) * flood[['major']]
  flood[['second']] <- (area[['second']] / total_area) * flood[['second']]

  flood_idx <- global(flood[['major']], 'sum', na.rm = T)$sum + global(flood[['second']], 'sum', na.rm = T)$sum

  return(flood_idx)
}

sync_heat <- function(tmax, ref)
{
  heat <- list()
  heat[['major']] <- lapp(tmax[['major']], fun = function(tmax) ifelse(tmax > 30, tmax, 0))
  heat[['second']] <- lapp(tmax[['second']], fun = function(tmax) ifelse(tmax > 30, tmax, 0))
  
  area <- list()
  area[['major']] <- crop(areaRas, ref[['major']], mask = T)
  area[['second']] <- crop(areaRas, ref[['second']], mask = T)
  
  total_area  <- global(area[['major']], 'sum', na.rm = T)$sum + global(area[['second']], 'sum', na.rm = T)$sum
  tmax[['major']] <- (area[['major']] / total_area) * tmax[['major']]
  tmax[['second']] <- (area[['second']] / total_area) * tmax[['second']]
  
  heat_idx <- global(tmax[['major']], 'sum', na.rm = T)$sum + global(tmax[['second']], 'sum', na.rm = T)$sum
  
  return(heat_idx)
}

# Forming a group of extreme weather indices from historical data ---------

# Creating lists to store the indices in each year
Grow_drought <- list()
Grow_flood <- list()
Grow_heat <- list()
Harv_drought <- list()
Harv_flood <- list()
Harv_heat <- list()

for (y in 1990:2015)
{
  rice_yield <- list()
  rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  GrowPDSI <- list()
  GrowPDSI[['major']] <- rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major/GrowPDSI_", y, ".tif"))
  GrowPDSI[['second']] <- rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second/GrowPDSI_", y, ".tif"))
  
  Growtmax <- list()
  Growtmax[['major']] <- rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major/Growtmax_", y, ".tif"))
  Growtmax[['second']] <- rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second/Growtmax_", y, ".tif"))
  
  HarvPDSI <- list()
  HarvPDSI[['major']] <- rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major/HarvPDSI_", y, ".tif"))
  HarvPDSI[['second']] <- rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second/HarvPDSI_", y, ".tif"))
  
  Harvtmax <- list()
  Harvtmax[['major']] <- rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major/Harvtmax_", y, ".tif"))
  Harvtmax[['second']] <- rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second/Harvtmax_", y, ".tif"))
  
  Grow_drought[[as.character(y)]] <- sync_drought(pdsi = GrowPDSI, ref = rice_yield)
  Grow_flood[[as.character(y)]] <- sync_flood(pdsi = GrowPDSI, ref = rice_yield)
  Grow_heat[[as.character(y)]] <- sync_heat(tmax = Growtmax, ref = rice_yield)
  Harv_drought[[as.character(y)]] <- sync_drought(pdsi = HarvPDSI, ref = rice_yield)
  Harv_flood[[as.character(y)]] <- sync_flood(pdsi = HarvPDSI, ref = rice_yield)
  Harv_heat[[as.character(y)]] <- sync_heat(tmax = Harvtmax, ref = rice_yield)
  
  print(y)
}

Grow_drought <- unlist(Grow_drought)
Grow_flood <- unlist(Grow_flood)
Grow_heat <- unlist(Grow_heat)
Harv_drought <- unlist(Harv_drought)
Harv_flood <- unlist(Harv_flood)
Harv_heat <- unlist(Harv_heat)

descdist(Grow_drought, boot = 300)
gamma_test(Grow_drought)#
shapiro.test(Grow_drought)
descdist(Grow_flood, boot = 300)
gamma_test(Grow_flood)
shapiro.test(Grow_flood)#
descdist(Grow_heat, boot = 300)
gamma_test(Grow_heat)#
shapiro.test(Grow_heat)
descdist(Harv_drought, boot = 300)
gamma_test(Harv_drought)
shapiro.test(Harv_drought)#
descdist(Harv_flood, boot = 300)
gamma_test(Harv_flood)
shapiro.test(Harv_flood)#
descdist(Harv_heat, boot = 300)
gamma_test(Harv_heat)#
shapiro.test(Harv_heat)

dist_Grow_drought <- fitdist(Grow_drought, 'gamma')
dist_Grow_flood <- fitdist(Grow_flood, 'gamma')
dist_Grow_heat <- fitdist(Grow_heat, 'gamma')
dist_Harv_drought <- fitdist(Harv_drought, 'gamma')
dist_Harv_flood <- fitdist(Harv_flood, 'gamma')
dist_Harv_heat <- fitdist(Harv_heat, 'gamma')

plot(dist_Grow_drought)
plot(dist_Grow_flood)
plot(dist_Grow_heat)
plot(dist_Harv_drought)
plot(dist_Harv_flood)
plot(dist_Harv_heat)

#Calculate the threshold value of each type of disasters
thresh_val <- list()

thresh_val[["Grow_drought_thre"]] <- unlist(quantile(dist_Grow_drought, probs = c(0.999, 0.998, 0.9975, 0.995, 0.99, 0.98, 0.95, 0.90)))
thresh_val[["Grow_flood_thre"]] <- unlist(quantile(dist_Grow_flood, probs = c(0.999, 0.998, 0.9975, 0.995, 0.99, 0.98, 0.95, 0.90)))
thresh_val[["Grow_heat_thre"]] <- unlist(quantile(dist_Grow_heat, probs = c(0.999, 0.998, 0.9975, 0.995, 0.99, 0.98, 0.95, 0.90)))
thresh_val[["Harv_drought_thre"]] <- unlist(quantile(dist_Harv_drought, probs = c(0.999, 0.998, 0.9975, 0.995, 0.99, 0.98, 0.95, 0.90)))
thresh_val[["Harv_flood_thre"]] <- unlist(quantile(dist_Harv_flood, probs = c(0.999, 0.998, 0.9975, 0.995, 0.99, 0.98, 0.95, 0.90)))
thresh_val[["Harv_heat_thre"]] <- unlist(quantile(dist_Harv_heat, probs = c(0.999, 0.998, 0.9975, 0.995, 0.99, 0.98, 0.95, 0.90)))

thresh_val <- do.call(rbind, thresh_val)

thresh_val <- data.frame(thresh_val[,1:8])
colnames(thresh_val) <- c('per1000', 'per500', 'per400', 'per200', 'per100', 'per50', 'per20', 'per10')
row.names(thresh_val) <- c('Grow_drought', 'Grow_flood', 'Grow_heat', 'Harv_drought', 'Harv_flood', 'Harv_heat')
write.csv(thresh_val, "./Data/thresh_val.csv", row.names = T)

# Identifying the years experiencing synchronized weather events ----------

Grow_drought[Grow_drought > thresh_val['Grow_drought', 'per10']]
Grow_flood[Grow_flood > thresh_val['Grow_flood', 'per10']]
Grow_heat[Grow_heat > thresh_val['Grow_heat', 'per10']]
Harv_drought[Harv_drought > thresh_val['Harv_drought', 'per10']]
Harv_flood[Harv_flood > thresh_val['Harv_flood', 'per10']]
Harv_heat[Harv_heat > thresh_val['Harv_heat', 'per10']]

# Creating lists to store the indices in each year (2c) ------
Grow_drought <- list()
Grow_flood <- list()
Grow_heat <- list()
Harv_drought <- list()
Harv_flood <- list()
Harv_heat <- list()

for (y in 1990:2015)
{
  rice_yield <- list()
  rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  GrowPDSI <- list()
  GrowPDSI[['major']] <- rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major_2c/GrowPDSI_2c_", y, ".tif"))
  GrowPDSI[['second']] <- rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second_2c/GrowPDSI_2c_", y, ".tif"))
  
  Growtmax <- list()
  Growtmax[['major']] <- rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major_2c/Growtmax_2c_", y, ".tif"))
  Growtmax[['second']] <- rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second_2c/Growtmax_2c_", y, ".tif"))
  
  HarvPDSI <- list()
  HarvPDSI[['major']] <- rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major_2c/HarvPDSI_2c_", y, ".tif"))
  HarvPDSI[['second']] <- rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second_2c/HarvPDSI_2c_", y, ".tif"))
  
  Harvtmax <- list()
  Harvtmax[['major']] <- rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major_2c/Harvtmax_2c_", y, ".tif"))
  Harvtmax[['second']] <- rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second_2c/Harvtmax_2c_", y, ".tif"))
  
  Grow_drought[[as.character(y)]] <- sync_drought(pdsi = GrowPDSI, ref = rice_yield)
  Grow_flood[[as.character(y)]] <- sync_flood(pdsi = GrowPDSI, ref = rice_yield)
  Grow_heat[[as.character(y)]] <- sync_heat(tmax = Growtmax, ref = rice_yield)
  Harv_drought[[as.character(y)]] <- sync_drought(pdsi = HarvPDSI, ref = rice_yield)
  Harv_flood[[as.character(y)]] <- sync_flood(pdsi = HarvPDSI, ref = rice_yield)
  Harv_heat[[as.character(y)]] <- sync_heat(tmax = Harvtmax, ref = rice_yield)
  
  print(y)
}

Grow_drought <- unlist(Grow_drought)
Grow_flood <- unlist(Grow_flood)
Grow_heat <- unlist(Grow_heat)
Harv_drought <- unlist(Harv_drought)
Harv_flood <- unlist(Harv_flood)
Harv_heat <- unlist(Harv_heat)

# Creating lists to store the indices in each year (2C)
Grow_drought[Grow_drought > thresh_val['Grow_drought', 'per1000']]
Grow_flood[Grow_flood > thresh_val['Grow_flood', 'per1000']]
Grow_heat[Grow_heat > thresh_val['Grow_heat', 'per1000']]
Harv_drought[Harv_drought > thresh_val['Harv_drought', 'per1000']]
Harv_flood[Harv_flood > thresh_val['Harv_flood', 'per1000']]
Harv_heat[Harv_heat > thresh_val['Harv_heat', 'per1000']]

# Creating lists to store the indices in each year (4c) ------
Grow_drought <- list()
Grow_flood <- list()
Grow_heat <- list()
Harv_drought <- list()
Harv_flood <- list()
Harv_heat <- list()

for (y in 1990:2015)
{
  rice_yield <- list()
  rice_yield[['major']] <- rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  rice_yield[['second']] <- rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  GrowPDSI <- list()
  GrowPDSI[['major']] <- rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major_4c/GrowPDSI_4c_", y, ".tif"))
  GrowPDSI[['second']] <- rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second_4c/GrowPDSI_4c_", y, ".tif"))
  
  Growtmax <- list()
  Growtmax[['major']] <- rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major_4c/Growtmax_4c_", y, ".tif"))
  Growtmax[['second']] <- rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second_4c/Growtmax_4c_", y, ".tif"))
  
  HarvPDSI <- list()
  HarvPDSI[['major']] <- rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major_4c/HarvPDSI_4c_", y, ".tif"))
  HarvPDSI[['second']] <- rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second_4c/HarvPDSI_4c_", y, ".tif"))
  
  Harvtmax <- list()
  Harvtmax[['major']] <- rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major_4c/Harvtmax_4c_", y, ".tif"))
  Harvtmax[['second']] <- rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second_4c/Harvtmax_4c_", y, ".tif"))
  
  Grow_drought[[as.character(y)]] <- sync_drought(pdsi = GrowPDSI, ref = rice_yield)
  Grow_flood[[as.character(y)]] <- sync_flood(pdsi = GrowPDSI, ref = rice_yield)
  Grow_heat[[as.character(y)]] <- sync_heat(tmax = Growtmax, ref = rice_yield)
  Harv_drought[[as.character(y)]] <- sync_drought(pdsi = HarvPDSI, ref = rice_yield)
  Harv_flood[[as.character(y)]] <- sync_flood(pdsi = HarvPDSI, ref = rice_yield)
  Harv_heat[[as.character(y)]] <- sync_heat(tmax = Harvtmax, ref = rice_yield)
  
  print(y)
}

Grow_drought <- unlist(Grow_drought)
Grow_flood <- unlist(Grow_flood)
Grow_heat <- unlist(Grow_heat)
Harv_drought <- unlist(Harv_drought)
Harv_flood <- unlist(Harv_flood)
Harv_heat <- unlist(Harv_heat)

# Creating lists to store the indices in each year (4C)
Grow_drought[Grow_drought > thresh_val['Grow_drought', 'per200']]
Grow_flood[Grow_flood > thresh_val['Grow_flood', 'per200']]
Grow_heat[Grow_heat > thresh_val['Grow_heat', 'per200']]
Harv_drought[Harv_drought > thresh_val['Harv_drought', 'per200']]
Harv_flood[Harv_flood > thresh_val['Harv_flood', 'per200']]
Harv_heat[Harv_heat > thresh_val['Harv_heat', 'per200']]

