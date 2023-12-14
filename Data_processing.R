#try to use sample points instead of administrative regions.

setwd("~/Desktop/Crop Failure/Research")
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

#library
library(terra)
library(readxl)
library(dplyr)


#input the map which contains the boundaries of countries
Country = vect("/Users/zhangfengqi/Desktop/Crop Failure/WorldCountries/WorldCountriesWithISO.shp")
areaRas = rast("/Users/zhangfengqi/Desktop/Crop Failure/WorldCountries/areaRas_30min.tif")
RegionList = read_excel("./SocialData/wbesg.xlsx")

#the administrative area
RiceProd = vect("./RiceAtlas/RiceProduction_v1/RiceProduction_v1.shp")

base_major = rast("./Data/Maps/Agromaps/Baseline_major_1990_2015.tif")
base_second = rast("./Data/Maps/Agromaps/Baseline_second_1990_2015.tif")

base_major = crop(base_major, RiceProd, mask = T, snap = 'out', touches = F)
base_second = crop(base_second, RiceProd, mask = T, snap = 'out', touches = F)


# The resampling of the dataset (to 0.5 degree resolution)) ---------------

Template = rast("./Data/Maps/Agromaps/rice_major/rice_major_1989.tif")

###Agroclimate
folders = list.files("./agroclimate")

for (folder in folders)
{
  files = list.files(paste0("./agroclimate/", folder))
  
  if (length(files) > 0)
  {
    for (file in files)
    {
      temp = rast(paste0("./agroclimate/", folder, "/", file))
      temp = resample(temp, Template, method = 'average')
      writeRaster(temp, paste0("./Data/Maps/Agroclimate/", folder, "/", file), overwrite = T)
      
      print(paste0("./Data/Maps/Agroclimate/", folder, "/", file))
    }
  }
  else
  {
    next
  }
}

# srad as supplementary
files <- list.files("./agroclimate/Growsrad_rice_second")

for (file in files)
{
  srad <- rast(paste0("./agroclimate/Growsrad_rice_second/", file))
  srad <- resample(srad, Template, method = 'average')
  writeRaster(srad, paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/", file), overwrite = T)
  
  print(paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/", file))
}

###Anthropogenic
files = list.files("./SocialData/EconMaps/Popu_HYDE")

for (file in files)
{
  temp = rast(paste0("./SocialData/EconMaps/Popu_HYDE/", file))
  temp = resample(temp, Template, method = 'sum')
  temp = temp / areaRas
  writeRaster(temp, paste0("./Data/Maps/Anthropogenic/Population_Density/", file), overwrite = T)
  
  print(paste0("./Data/Maps/Anthropogenic/Population_Density/", file))
}



# Calculate the variation rate of rice yield ------------------------------

major = list()
second = list()

i = 1 
for (y in 1990:2015)
{
  
  
  major[[i]] = rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  second[[i]] = rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  i = i + 1
  
  print(y)
}

major = mean(do.call(c, major), na.rm = T)
second = mean(do.call(c, second), na.rm = T)

writeRaster(major, "./Data/Maps/Agromaps/Baseline_major_1990_2015.tif", overwrite = T)
writeRaster(second, "./Data/Maps/Agromaps/Baseline_second_1990_2015.tif", overwrite = T)

base_major = rast("./Data/Maps/Agromaps/Baseline_major_1990_2015.tif")
base_second = rast("./Data/Maps/Agromaps/Baseline_second_1990_2015.tif")

for (y in 1990:2015)
{
  temp_major = rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  temp_second = rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  temp_major = (temp_major - base_major) / base_major
  temp_second = (temp_second - base_second) / base_second
  
  writeRaster(temp_major, paste0("./Data/Maps/Agromaps/rice_major_var/rice_major_var_", y, ".tif"), overwrite = T)
  writeRaster(temp_second, paste0("./Data/Maps/Agromaps/rice_second_var/rice_second_var_", y, ".tif"), overwrite = T)
  
  print(y)
}

###detrending (time-series)

for (y in 1990:2015)
{
  temp_major = rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  temp_second = rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  base_major_annual = base_major * as.numeric(global(temp_major, fun = 'mean', na.rm = T)) / as.numeric(global(base_major, fun = 'mean', na.rm = T))
  base_second_annual = base_second * as.numeric(global(temp_second, fun = 'mean', na.rm = T)) / as.numeric(global(base_second, fun = 'mean', na.rm = T))
  
  writeRaster(base_major_annual, paste0("./Data/Maps/Agromaps/base_major/base_major_", y, ".tif"), overwrite = T)
  writeRaster(base_second_annual, paste0("./Data/Maps/Agromaps/base_second/base_second_", y, ".tif"), overwrite = T)
  
  temp_major = (temp_major - base_major_annual) / base_major_annual
  temp_second = (temp_second - base_second_annual) / base_second_annual
  
  writeRaster(temp_major, paste0("./Data/Maps/Agromaps/rice_major_var_detrending/rice_major_var_", y, ".tif"), overwrite = T)
  writeRaster(temp_second, paste0("./Data/Maps/Agromaps/rice_second_var_detrending/rice_second_var_", y, ".tif"), overwrite = T)
  
  print(y)
}

###variation (absolute value)

for (y in 1990:2015)
{
  temp_major = rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif"))
  temp_second = rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif"))
  
  temp_major <- temp_major - base_major
  temp_second <- temp_second - base_second
  
  writeRaster(temp_major, paste0("./Data/Maps/Agromaps/rice_major_var_abs/rice_major_var_abs_", y, ".tif"), overwrite = T)
  writeRaster(temp_second, paste0("./Data/Maps/Agromaps/rice_second_var_abs/rice_second_var_abs_", y, ".tif"), overwrite = T)
  
  print(y)
}

# Sampling and extraction (Major) -----------------------------------------

##seed 10
Seed = 10
set.seed(Seed)

Sample_major = spatSample(base_major, size = 8000, na.rm = T, exhaustive = T, method = 'random', as.points = T)
Sample_major$SampleID = seq(1, nrow(Sample_major), 1)
Sample_major = Sample_major[,c('SampleID')]
SampleInfo = extract(RiceProd, Sample_major)
SampleInfo = SampleInfo[!duplicated(SampleInfo$id.y),]

Sample_major$ISO3 = SampleInfo$ISO
Sample_major$Country = SampleInfo$COUNTRY
Sample_major$Admin_lv1 = SampleInfo$REGION
Sample_major$Admin_lv2 = SampleInfo$SUB_REGION
Sample_major$Plant_ST = SampleInfo$PLANT_ST1
Sample_major$Plant_PK = SampleInfo$PLANT_PK1
Sample_major$Plant_END = SampleInfo$PLANT_END1
Sample_major$Harv_ST = SampleInfo$HARV_ST1
Sample_major$Harv_PK = SampleInfo$HARV_PK1
Sample_major$Harv_END = SampleInfo$HARV_END1

writeVector(Sample_major, paste0("./Data/Maps/Samples/RiceMajorSamp/RiceMajorSamp_", Seed, "_8.shp"), overwrite = T)

#data extract#Major
RiceMajor = data.frame()
Sample_major = vect(paste0("./Data/Maps/Samples/RiceMajorSamp/RiceMajorSamp_", Seed, "_8.shp"))

for (y in 1990:2015)
{
  Maps = c(
    rast(paste0("./Data/Maps/Agromaps/rice_major/rice_major_", y, ".tif")), #rice yield map
    rast(paste0("./Data/Maps/Agromaps/rice_major_var/rice_major_var_", y, ".tif")), #rice yield variation map
    rast(paste0("./Data/Maps/Agromaps/rice_major_var_detrending/rice_major_var_", y, ".tif")), #rice yield variation (detrending) map
    rast(paste0("./Data/Maps/Agromaps/rice_major_var_abs/rice_major_var_abs_", y, ".tif")), #rice yield variation (absolute value) map
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_major/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_major/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_major/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_major/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_major/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_major/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_major/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_major/Harvsrad_", y, ".tif")), #Harvsrad
    rast(paste0("./Data/Maps/Anthropogenic/GDP/GDPpercap", y, ".tif")), #GDP
    rast(paste0("./Data/Maps/Anthropogenic/HDI/HDI_", y, ".tif")), #HDI
    rast(paste0("./Data/Maps/Anthropogenic/Population_Density/Popu", y, ".tif")) #Population density
  )
  
  ExtractTemp = extract(x = Maps, y = Sample_major, method = 'simple', buffer = NULL, fun = mean, na.rm = T, ID = F)
  colnames(ExtractTemp) = c("Yield", "Yield_var", "Yield_detrend", "Yield_var_abs", "GrowPDSI", "Growtmin", "Growtmax", "Growsrad", "HarvPDSI", "Harvtmin", "Harvtmax", "Harvsrad", "GDP", "HDI", "PopuDens")
  ExtractTemp$SampleID = Sample_major$SampleID
  ExtractTemp$ISO3 = Sample_major$ISO3
  ExtractTemp$Country = Sample_major$Country
  ExtractTemp$Admin_lv1 = Sample_major$Admin_lv1
  ExtractTemp$Admin_lv2 = Sample_major$Admin_lv2
  ExtractTemp$Year = y
  RiceMajor = rbind(RiceMajor, ExtractTemp)
  print(paste0("Data in ", y ," extracted"))
}

RiceMajor = merge(x = RiceMajor, y = RegionList, by = "ISO3", all.x = T)
RiceMajor = RiceMajor[order(RiceMajor$Year, RiceMajor$SampleID),]
RiceMajor = RiceMajor[, c('SampleID', 'ISO3', 'Year', 'Region', 'Country', 'Admin_lv1', 'Admin_lv2', "Yield", "Yield_var", "Yield_detrend", "Yield_var_abs", "GrowPDSI", "Growtmin", "Growtmax", "Growsrad", "HarvPDSI", "Harvtmin", "Harvtmax", "Harvsrad", "GDP", "HDI", "PopuDens")]

RiceMajor[is.na(RiceMajor$Admin_lv1), c('Admin_lv1')] <- "unknown"
RiceMajor[is.na(RiceMajor$Admin_lv2), c('Admin_lv2')] <- "unknown"

write.csv(RiceMajor, "./Data/Rice/RiceMajor.csv", row.names = F)


# Sampling and extraction (Second) -----------------------------------------

##seed 10
Seed = 10
set.seed(Seed)

Sample_second = spatSample(base_second, size = 8000, na.rm = T, exhaustive = T, method = 'random', as.points = T)
Sample_second$SampleID = seq(1, nrow(Sample_second), 1)
Sample_second = Sample_second[,c('SampleID')]
SampleInfo = extract(RiceProd, Sample_second)
SampleInfo = SampleInfo[!duplicated(SampleInfo$id.y),]

Sample_second$ISO3 = SampleInfo$ISO
Sample_second$Country = SampleInfo$COUNTRY
Sample_second$Admin_lv1 = SampleInfo$REGION
Sample_second$Admin_lv2 = SampleInfo$SUB_REGION
Sample_second$Plant_ST = SampleInfo$PLANT_ST1
Sample_second$Plant_PK = SampleInfo$PLANT_PK1
Sample_second$Plant_END = SampleInfo$PLANT_END1
Sample_second$Harv_ST = SampleInfo$HARV_ST1
Sample_second$Harv_PK = SampleInfo$HARV_PK1
Sample_second$Harv_END = SampleInfo$HARV_END1

writeVector(Sample_second, paste0("./Data/Maps/Samples/RiceSecondSamp/RiceSecondSamp_", Seed, "_1.shp"), overwrite = T)

#data extract#Second
RiceSecond = data.frame()
Sample_second = vect(paste0("./Data/Maps/Samples/RiceSecondSamp/RiceSecondSamp_", Seed, "_1.shp"))

for (y in 1990:2015)
{
  Maps = c(
    rast(paste0("./Data/Maps/Agromaps/rice_second/rice_second_", y, ".tif")), #rice yield map
    rast(paste0("./Data/Maps/Agromaps/rice_second_var/rice_second_var_", y, ".tif")), #rice yield variation map
    rast(paste0("./Data/Maps/Agromaps/rice_second_var_detrending/rice_second_var_", y, ".tif")), #rice yield variation (detrending) map
    rast(paste0("./Data/Maps/Agromaps/rice_second_var_abs/rice_second_var_abs_", y, ".tif")), #rice yield variation (absolute value) map
    rast(paste0("./Data/Maps/Agroclimate/GrowPDSI_rice_second/GrowPDSI_", y, ".tif")), #GrowPDSI
    rast(paste0("./Data/Maps/Agroclimate/Growtmin_rice_second/Growtmin_", y, ".tif")), #Growtmin
    rast(paste0("./Data/Maps/Agroclimate/Growtmax_rice_second/Growtmax_", y, ".tif")), #Growtmax
    rast(paste0("./Data/Maps/Agroclimate/Growsrad_rice_second/Growsrad_", y, ".tif")), #Growsrad
    rast(paste0("./Data/Maps/Agroclimate/HarvPDSI_rice_second/HarvPDSI_", y, ".tif")), #HarvPDSI
    rast(paste0("./Data/Maps/Agroclimate/Harvtmin_rice_second/Harvtmin_", y, ".tif")), #Harvtmin
    rast(paste0("./Data/Maps/Agroclimate/Harvtmax_rice_second/Harvtmax_", y, ".tif")), #Harvtmax
    rast(paste0("./Data/Maps/Agroclimate/Harvsrad_rice_second/Harvsrad_", y, ".tif")), #Harvsrad
    rast(paste0("./Data/Maps/Anthropogenic/GDP/GDPpercap", y, ".tif")), #GDP
    rast(paste0("./Data/Maps/Anthropogenic/HDI/HDI_", y, ".tif")), #HDI
    rast(paste0("./Data/Maps/Anthropogenic/Population_Density/Popu", y, ".tif")) #Population density
  )
  
  ExtractTemp = extract(x = Maps, y = Sample_second, method = 'simple', buffer = NULL, fun = mean, na.rm = T, ID = F)
  colnames(ExtractTemp) = c("Yield", "Yield_var", "Yield_detrend", "Yield_var_abs", "GrowPDSI", "Growtmin", "Growtmax", "Growsrad", "HarvPDSI", "Harvtmin", "Harvtmax", "Harvsrad", "GDP", "HDI", "PopuDens")
  ExtractTemp$SampleID = Sample_second$SampleID
  ExtractTemp$ISO3 = Sample_second$ISO3
  ExtractTemp$Country = Sample_second$Country
  ExtractTemp$Admin_lv1 = Sample_second$Admin_lv1
  ExtractTemp$Admin_lv2 = Sample_second$Admin_lv2
  ExtractTemp$Year = y
  RiceSecond = rbind(RiceSecond, ExtractTemp)
  print(paste0("Data in ", y ," extracted"))
}

RiceSecond = merge(x = RiceSecond, y = RegionList, by = "ISO3", all.x = T)
RiceSecond = RiceSecond[order(RiceSecond$Year, RiceSecond$SampleID),]
RiceSecond = RiceSecond[, c('SampleID', 'ISO3', 'Year', 'Region', 'Country', 'Admin_lv1', 'Admin_lv2', "Yield", "Yield_var", "Yield_detrend", "Yield_var_abs", "GrowPDSI", "Growtmin", "Growtmax", "Growsrad", "HarvPDSI", "Harvtmin", "Harvtmax", "Harvsrad", "GDP", "HDI", "PopuDens")]

RiceSecond[is.na(RiceSecond$Admin_lv1), c('Admin_lv1')] <- "unknown"
RiceSecond[is.na(RiceSecond$Admin_lv2), c('Admin_lv2')] <- "unknown"

write.csv(RiceSecond, "./Data/Rice/RiceSecond.csv", row.names = F)

