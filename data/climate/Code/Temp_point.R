#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)
library(tidyverse)

setwd("C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/")


#return a list of the raster to stack

files <- list.files("Temperature/",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.nc") #filters list of files to those with '.tif' file type

files

#Read the files listed in 'files' into a raster stack
#Temp_stk <- stack(files)

#Read in the shapefile of points
Sites <- vect("Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
#plot(Sites)

#Create a reference raster to use as key
ref_rast <- rast(files[1])

#create a blank dataset
rm(Temp_dat)
Temp_dat <- tibble(ID=seq(1,14,by=1)) #create ID column for joining extracted data

#include site names
site_list <-as.data.frame(Sites)[1]
Temp_dat <- cbind(Temp_dat,site_list)

#loop through rasters layers and extract values at points
for (i in files) {
           
     temp_rast <- rast(i)
     names(temp_rast) <- paste0("Temp_pt_",substr(i,63,68))
     
     temp_rast <- resample(temp_rast, ref_rast) #needed for the new layers
     
     temp_rast <- mask(temp_rast,ref_rast)
     
     Temp_pt <- terra::extract(temp_rast, Sites, na.rm=FALSE)
     
     Temp_dat <- left_join(Temp_dat, Temp_pt)
           
}

#shows list of column names 
ls(Temp_dat)

#Write out file with all NDVI data for sampling point location to table
write.csv(Temp_dat, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Temp_point.csv")

