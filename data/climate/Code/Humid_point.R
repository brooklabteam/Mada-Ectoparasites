#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

setwd("C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/")


#return a list of the raster to stack
#Doing each folder separate since some are day time and some are night time humidity values

#######################################################################
#Day time values first

files <- list.files("Humidity/Raw-data/Daytime",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.tif") #filters list of files to those with '.tif' file type

files


#Read in the shapefile of points
Sites <- vect("Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
#plot(Sites)

#Create a reference raster to use as key
ref_rast <- rast(files[1])

#create a blank dataset
#rm(Humid_dat)
Humid_dat <- tibble(ID=seq(1,14,by=1)) #create ID column for joining extracted data

#include site names
site_list <-as.data.frame(Sites)[1]
Humid_dat <- cbind(Humid_dat,site_list)

#loop through rasters layers and extract values at points
for (i in files) {
  
  humid_rast <- rast(i)
  names(humid_rast) <- paste0("Humid_pt_daytime_",substr(i,97,102))
  
  humid_rast <- resample(humid_rast, ref_rast) #needed for the new layers
  
  humid_rast <- mask(humid_rast,ref_rast)
  
  Humid_pt <- terra::extract(humid_rast, Sites, na.rm=FALSE)
  
  Humid_dat <- left_join(Humid_dat, Humid_pt)
  
}

#shows list of column names 
ls(Humid_dat)

#Write out file with all NDVI data for sampling point location to table
write.csv(Humid_dat, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Humid_point_daytime.csv")




#######################################################################

#Nighttime values

#Clear vectors
rm(Humid_dat, Humid_pt, humid_rast, ref_rast, site_list, files, i)

# Get list of nighttime humidity files
files <- list.files("Humidity/Raw-data/Nighttime",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.tif") #filters list of files to those with '.tif' file type

files

#Create a reference raster to use as key
ref_rast <- rast(files[1])

#create a blank dataset
#rm(Humid_dat)
Humid_dat <- tibble(ID=seq(1,14,by=1)) #create ID column for joining extracted data

#include site names
site_list <-as.data.frame(Sites)[1]
Humid_dat <- cbind(Humid_dat,site_list)

#loop through rasters layers and extract values at points
for (i in files) {
  
  humid_rast <- rast(i)
  names(humid_rast) <- paste0("Humid_pt_nighttime_",substr(i,101,106))
  
  humid_rast <- resample(humid_rast, ref_rast) #needed for the new layers
  
  humid_rast <- mask(humid_rast,ref_rast)
  
  Humid_pt <- terra::extract(humid_rast, Sites, na.rm=FALSE)
  
  Humid_dat <- left_join(Humid_dat, Humid_pt)
  
}

#shows list of column names 
ls(Humid_dat)

#Write out file with all NDVI data for sampling point location to table
write.csv(Humid_dat, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Humid_point_nighttime.csv")
