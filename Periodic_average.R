#try to use sample points instead of administrative regions.

setwd("~/Desktop/Crop Failure/Research") # Mac
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

#library
library(terra)
library(ggplot2)

#input the map which contains the boundaries of countries
Country <- vect("./Data/Country/Country.shp")

###convert shp files of rice calendar into tif files
RiceProd = vect("./RiceAtlas/RiceProduction_v1/RiceProduction_v1.shp")

for (y in 1990:2015)
{
  PDSI = c(rast(paste0("./climate_dataset/PDSI_4c/PDSI_4c_", y - 1, ".tif")),
           rast(paste0("./climate_dataset/PDSI_4c/PDSI_4c_", y, ".tif")))
  tmin = c(rast(paste0("./climate_dataset/tmin_4c/TerraClimate_4c_tmin_", y - 1, ".tif")),
           rast(paste0("./climate_dataset/tmin_4c/TerraClimate_4c_tmin_", y, ".tif")))
  tmax = c(rast(paste0("./climate_dataset/tmax_4c/TerraClimate_4c_tmax_", y - 1, ".tif")),
           rast(paste0("./climate_dataset/tmax_4c/TerraClimate_4c_tmax_", y, ".tif")))
  srad = c(rast(paste0("./climate_dataset/srad/TerraClimate_srad_", y - 1, ".tif")),
           rast(paste0("./climate_dataset/srad/TerraClimate_srad_", y, ".tif")))
  ppt = c(rast(paste0("./climate_dataset/ppt/TerraClimate_ppt_", y - 1, ".tif")),
          rast(paste0("./climate_dataset/ppt/TerraClimate_ppt_", y, ".tif")))
  
  GrowPDSI_major = list()
  Growtmin_major = list()
  Growtmax_major = list()
  HarvPDSI_major = list()
  Harvtmin_major = list()
  Harvtmax_major = list()
  Growsrad_major <- list()
  Harvsrad_major <- list()
  Growppt_major <- list()
  Harvppt_major <- list()
  
  GrowPDSI_second = list()
  Growtmin_second = list()
  Growtmax_second = list()
  HarvPDSI_second = list()
  Harvtmin_second = list()
  Harvtmax_second = list()
  Growsrad_second <- list()
  Harvsrad_second <- list()
  Growppt_second <- list()
  Harvppt_second <- list()
  
  for (i in 1:nrow(RiceProd))
  {
    PDSI_sub = crop(PDSI, RiceProd[i], mask = T)
    tmin_sub = crop(tmin, RiceProd[i], mask = T)
    tmax_sub = crop(tmax, RiceProd[i], mask = T)
    srad_sub <- crop(srad, RiceProd[i], mask = T)
    ppt_sub <- crop(ppt, RiceProd[i], mask = T)
    
    ###major
    plant_st_major = as.integer(substr(as.Date(RiceProd$PLANT_ST1[i]), 6, 7))
    harv_st_major = as.integer(substr(as.Date(RiceProd$HARV_ST1[i]), 6, 7))
    harv_end_major = as.integer(substr(as.Date(RiceProd$HARV_END1[i]), 6, 7))
    
    if (plant_st_major > harv_st_major)
    {
      grow_Idx_major = c((plant_st_major) : (harv_st_major + 12))
    }
    if (plant_st_major <= harv_st_major)
    {
      grow_Idx_major = c((plant_st_major + 12) : (harv_st_major + 12))
    }
    
    if (harv_st_major > harv_end_major)
    {
      harv_Idx_major = c((harv_st_major) : (harv_end_major + 12))
    }
    if (harv_st_major <= harv_end_major)
    {
      harv_Idx_major = c((harv_st_major + 12) : (harv_end_major + 12))
    }
    
    GrowPDSI_major[[i]] = mean(PDSI_sub[[grow_Idx_major]], na.rm = T)
    Growtmin_major[[i]] = mean(tmin_sub[[grow_Idx_major]], na.rm = T)
    Growtmax_major[[i]] = mean(tmax_sub[[grow_Idx_major]], na.rm = T)
    Growsrad_major[[i]] = mean(srad_sub[[grow_Idx_major]], na.rm = T)
    Growppt_major[[i]] = mean(ppt_sub[[grow_Idx_major]], na.rm = T)
    HarvPDSI_major[[i]] = mean(PDSI_sub[[harv_Idx_major]], na.rm = T)
    Harvtmin_major[[i]] = mean(tmin_sub[[harv_Idx_major]], na.rm = T)
    Harvtmax_major[[i]] = mean(tmax_sub[[harv_Idx_major]], na.rm = T)
    Harvsrad_major[[i]] = mean(srad_sub[[harv_Idx_major]], na.rm = T)
    Harvppt_major[[i]] = mean(ppt_sub[[harv_Idx_major]], na.rm = T)
    
    ###second
    plant_st_second = as.integer(substr(as.Date(RiceProd$PLANT_ST1[i]), 6, 7))
    harv_st_second = as.integer(substr(as.Date(RiceProd$HARV_ST1[i]), 6, 7))
    harv_end_second = as.integer(substr(as.Date(RiceProd$HARV_END1[i]), 6, 7))
    
    if (plant_st_second > harv_st_second)
    {
      grow_Idx_second = c((plant_st_second) : (harv_st_second + 12))
    }
    if (plant_st_second <= harv_st_second)
    {
      grow_Idx_second = c((plant_st_second + 12) : (harv_st_second + 12))
    }
    
    if (harv_st_second > harv_end_second)
    {
      harv_Idx_second = c((harv_st_second) : (harv_end_second + 12))
    }
    if (harv_st_second <= harv_end_second)
    {
      harv_Idx_second = c((harv_st_second + 12) : (harv_end_second + 12))
    }
    
    GrowPDSI_second[[i]] = mean(PDSI_sub[[grow_Idx_second]], na.rm = T)
    Growtmin_second[[i]] = mean(tmin_sub[[grow_Idx_second]], na.rm = T)
    Growtmax_second[[i]] = mean(tmax_sub[[grow_Idx_second]], na.rm = T)
    Growsrad_second[[i]] = mean(srad_sub[[grow_Idx_second]], na.rm = T)
    Growppt_second[[i]] = mean(ppt_sub[[grow_Idx_second]], na.rm = T)
    HarvPDSI_second[[i]] = mean(PDSI_sub[[harv_Idx_second]], na.rm = T)
    Harvtmin_second[[i]] = mean(tmin_sub[[harv_Idx_second]], na.rm = T)
    Harvtmax_second[[i]] = mean(tmax_sub[[harv_Idx_second]], na.rm = T)
    Harvsrad_second[[i]] = mean(srad_sub[[harv_Idx_second]], na.rm = T)
    Harvppt_second[[i]] = mean(ppt_sub[[harv_Idx_second]], na.rm = T)
  }

  GrowPDSI_major = do.call(terra::merge, GrowPDSI_major)
  Growtmin_major = do.call(terra::merge, Growtmin_major)
  Growtmax_major = do.call(terra::merge, Growtmax_major)
  Growsrad_major = do.call(terra::merge, Growsrad_major)
  Growppt_major = do.call(terra::merge, Growppt_major)
  HarvPDSI_major = do.call(terra::merge, HarvPDSI_major)
  Harvtmin_major = do.call(terra::merge, Harvtmin_major)
  Harvtmax_major = do.call(terra::merge, Harvtmax_major)
  Harvsrad_major = do.call(terra::merge, Harvsrad_major)
  Harvppt_major = do.call(terra::merge, Harvppt_major)

  GrowPDSI_second = do.call(terra::merge, GrowPDSI_second)
  Growtmin_second = do.call(terra::merge, Growtmin_second)
  Growtmax_second = do.call(terra::merge, Growtmax_second)
  Growsrad_second = do.call(terra::merge, Growsrad_second)
  Growppt_second = do.call(terra::merge, Growppt_second)
  HarvPDSI_second = do.call(terra::merge, HarvPDSI_second)
  Harvtmin_second = do.call(terra::merge, Harvtmin_second)
  Harvtmax_second = do.call(terra::merge, Harvtmax_second)
  Harvsrad_second = do.call(terra::merge, Harvsrad_second)
  Harvppt_second = do.call(terra::merge, Harvppt_second)
  
  
  print(paste0(y, " calculation was finished!"))
  
  writeRaster(GrowPDSI_major, paste0("./agroclimate/GrowPDSI_rice_major_4c/GrowPDSI_4c_", y, ".tif"), overwrite = T)
  writeRaster(HarvPDSI_major, paste0("./agroclimate/HarvPDSI_rice_major_4c/HarvPDSI_4c_", y, ".tif"), overwrite = T)
  writeRaster(Growtmin_major, paste0("./agroclimate/Growtmin_rice_major_4c/Growtmin_4c_", y, ".tif"), overwrite = T)
  writeRaster(Harvtmin_major, paste0("./agroclimate/Harvtmin_rice_major_4c/Harvtmin_4c_", y, ".tif"), overwrite = T)
  writeRaster(Growtmax_major, paste0("./agroclimate/Growtmax_rice_major_4c/Growtmax_4c_", y, ".tif"), overwrite = T)
  writeRaster(Harvtmax_major, paste0("./agroclimate/Harvtmax_rice_major_4c/Harvtmax_4c_", y, ".tif"), overwrite = T)
  writeRaster(Growsrad_major, paste0("./agroclimate/Growsrad_rice_major/Growsrad_", y, ".tif"), overwrite = T)
  writeRaster(Harvsrad_major, paste0("./agroclimate/Harvsrad_rice_major/Harvsrad_", y, ".tif"), overwrite = T)
  writeRaster(Growppt_major, paste0("./agroclimate/Growppt_rice_major/Growppt_", y, ".tif"), overwrite = T)
  writeRaster(Harvppt_major, paste0("./agroclimate/Harvppt_rice_major/Harvppt_", y, ".tif"), overwrite = T)
  
  writeRaster(GrowPDSI_second, paste0("./agroclimate/GrowPDSI_rice_second_4c/GrowPDSI_4c_", y, ".tif"), overwrite = T)
  writeRaster(HarvPDSI_second, paste0("./agroclimate/HarvPDSI_rice_second_4c/HarvPDSI_4c_", y, ".tif"), overwrite = T)
  writeRaster(Growtmin_second, paste0("./agroclimate/Growtmin_rice_second_4c/Growtmin_4c_", y, ".tif"), overwrite = T)
  writeRaster(Harvtmin_second, paste0("./agroclimate/Harvtmin_rice_second_4c/Harvtmin_4c_", y, ".tif"), overwrite = T)
  writeRaster(Growtmax_second, paste0("./agroclimate/Growtmax_rice_second_4c/Growtmax_4c_", y, ".tif"), overwrite = T)
  writeRaster(Harvtmax_second, paste0("./agroclimate/Harvtmax_rice_second_4c/Harvtmax_4c_", y, ".tif"), overwrite = T)
  writeRaster(Growsrad_second, paste0("./agroclimate/Growsrad_rice_second/Growsrad_", y, ".tif"), overwrite = T)
  writeRaster(Harvsrad_second, paste0("./agroclimate/Harvsrad_rice_second/Harvsrad_", y, ".tif"), overwrite = T)
  writeRaster(Growppt_second, paste0("./agroclimate/Growppt_rice_second/Growppt_", y, ".tif"), overwrite = T)
  writeRaster(Harvppt_second, paste0("./agroclimate/Harvppt_rice_second/Harvppt_", y, ".tif"), overwrite = T)
  
  print(paste0(y, " writing was finished!"))
}

