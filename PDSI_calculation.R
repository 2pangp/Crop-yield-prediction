# Setting work directory
setwd("/Users/zhangfengqi/Desktop/Crop Failure/Research") # mac
setwd("C:/Users/e0950361/Desktop/iCloudDrive/Desktop/Crop Failure/Research") # windows

#library
library(rgdal)
library(sp)
library(sf)
library(terra)
library(raster)
library(SPEI)
library(scPDSI)

setwd("D:/climate")


Country = vect("D:/WorldCountries/WorldCountriesWithISO.shp")

files = list.files("./ppt+4")
for (file in files)
{
  temp = rast(paste0("./ppt+4/", file))
  temp = terra::project(temp, crs(Country))
  writeRaster(temp, paste0("./ppt_4c/ppt_4c_", substr(file, 21, 24), ".tif"))
  print(file)
}

files = list.files("./pet+4")
for (file in files)
{
  temp = rast(paste0("./pet+4/", file))
  temp = terra::project(temp, crs(Country))
  writeRaster(temp, paste0("./pet_4c/pet_4c_", substr(file, 21, 24), ".tif"))
  print(file)
}

###pdsi_2c

#read in rasters
i = 1
ppt = list()
pet = list()
for (y in 1989:2015)
{
  ppt[[i]] = rast(paste0("./ppt_2c/ppt_2c_", y, ".tif"))
  pet[[i]] = rast(paste0("./pet_2c/pet_2c_", y, ".tif"))
  
  print(paste0(y, "imported as the ", i, " layer")) 
  i = i + 1
}

ppt = do.call(c, ppt)
pet = do.call(c, pet)

pdsi_calc = function(ppt, pet)
{
  pdsi = pdsi(P = ppt, PE = pet, sc = T)$X
  return(pdsi)
}

climate = sds(list(ppt, pet))
pdsi = lapp(climate, fun = pdsi_calc)


for (lon in 22:24)
{
  for (lat in 6:6)
  {
    Ext = ext(5 * lon, 5 * (lon + 1) + 0.1, 5 * lat, 5 * (lat + 1) + 0.1)
    ppt_sub = crop(ppt, Ext)
    pet_sub = crop(pet, Ext)
    
    climate = sds(list(ppt_sub, pet_sub))
    
    pdsi_sub = lapp(climate, fun = pdsi_calc)
    writeRaster(pdsi_sub, paste0("./test/pdsi_2c_", 5 * lon, "_", 5 * (lon + 1), "_", 5 * lat, "_", 5 * (lat + 1), ".tif"), overwrite = T)

    print(paste0("lon: ", 5 * lon, 5 * (lon + 1), " lat: ", 5 * lat, 5 * (lat + 1)))
  }
}

Ext = ext(110, 130, 15, 35)
ppt_sub = crop(ppt, Ext)
pet_sub = crop(pet, Ext)

climate = sds(list(ppt_sub, pet_sub))

pdsi_sub = lapp(climate, fun = pdsi_calc)
writeRaster(pdsi_sub, paste0("./test/pdsi_2c_", 5 * lon, "_", 5 * (lon + 1), "_", 5 * lat, "_", 5 * (lat + 1), ".tif"), overwrite = T)























###test
test = pdsi(c(2,3,4,5,7,6,54,3,3,2,4,5,76,45,98,46,87,54,3,3,45,89,87,100), PE = c(2,3,4,5,7,6,54,3,3,2,4,5,2,3,4,5,7,6,54,3,3,2,4,5))

test1 = pdsi(c(76,45,98,46,87,54,3,3,45,89,87,100,2,3,4,5,7,6,54,3,3,2,4,5), PE = c(2,3,4,5,7,6,54,3,3,2,4,5,2,3,4,5,7,6,54,3,3,2,4,5))

test$X
test1$X

test = rast("./pdsi_2c/pdsi_2c_2002_03.tif")
plot(test, range = c(-10, 10))

test = list()
test[[1]] = rast(paste0("./ppt+4/TerraClimate_4c_ppt_", 1995, ".nc"))
test[[2]] = rast(paste0("./pet+4/TerraClimate_4c_pet_", 1995, ".nc"))
test = do.call(c, test)
