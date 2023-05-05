#Extract raster (environmental data) from buffers

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

#Reproject shapefile and create 30km buffer. I read shp file in with terra::vect() to use their buffer tool. 
#The buffer function has a bug in Raster
Sites_b <- vect("Shapefiles/Bat_sampling_locations.shp")


#must assign and projection so that we can measure distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'

#read in raster stack (using terra), rename layers, and reproject raster
Temp_stk_b <- rast(c(files))
#original names
names(Temp_stk_b)
#rename
names(Temp_stk_b) <- paste0("Temp_bf_",substr((files),63,68))
#new names
names(Temp_stk_b)
#reproject raster
Temp_stk_prj <- project(Temp_stk_b, 'EPSG:29701')
crs(Temp_stk_prj)

#Reproject shapefile and create 30km buffer. I read shp file in with terra::vect() to use their buffer tool. 
#The buffer function has a bug in Raster
Sites_b <- vect("Shapefiles/Bat_sampling_locations.shp")


#change crs of sites
Sites_b <- project(Sites_b, 'EPSG:29701')
crs(Sites_b)

#sanity check
plot(Temp_stk_prj,1)
points(Sites_b)

Sites_30kbuf <- buffer(Sites_b, 30000)

plot(Temp_stk_prj, 1)
points(Sites_b)
points(Sites_30kbuf)

#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
Temp_bf <- terra::extract(Temp_stk_prj, Sites_30kbuf, 
                          touches=FALSE)#,
                          #fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
Temp_bf <- as.data.frame(Temp_bf)


#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites_30kbuf)[1]
site_list['ID'] <- as.numeric(seq(1,14)) 

Temp_bf_extr <- merge(site_list,Temp_bf, by.x="ID", by.y="ID")

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(Temp_bf_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Temp_buffer.csv")
