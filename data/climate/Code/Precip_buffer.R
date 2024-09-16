#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

setwd("C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/")


#return a list of the raster to stack

files <- list.files("Precipitation/",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.nc") #filters list of files to those with '.tif' file type

files

# Get rid of old vector/list names
#rm(Preip_dat, ref_rast, site_list, files)

#Read in the shapefile of points
Sites <- vect("Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
#plot(Sites)

#Create a reference raster to use as key
ref_rast <- rast(files[1])

#create a blank dataset
Precip_dat <- tibble(ID=seq(1,14,by=1)) #create ID column for joining extracted data

#include site names
site_list <-as.data.frame(Sites)[1]
Precip_dat <- cbind(Precip_dat,site_list)






#must assign and projection so that we can measure distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'
#reproject raster
Precip_stk_prj <- project(PPT_stk_b, 'EPSG:29701')
crs(Precip_stk_prj)

#Reproject shapefile and create 30km buffer. I read shp file in with terra::vect() to use their buffer tool. 
#The buffer function has a bug in Raster
Sites_b <- vect("Shapefiles/Bat_sampling_locations.shp")

# compareCRS(NDVI_stk_b,Sites_b)
# plot(NDVI_stk_b,1)
# points(Sites_b)


#change crs of sites
Sites_b <- project(Sites_b, 'EPSG:29701')
crs(Sites_b)

#sanity check
plot(Precip_stk_prj,1)
points(Sites_b)

Sites_30kbuf <- buffer(Sites_b, 30000)

plot(Precip_stk_prj, 1)
points(Sites_b)
points(Sites_30kbuf)

#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
Precip_bf <- terra::extract(Precip_stk_prj, Sites_30kbuf, 
                          touches=FALSE)#,
                          #fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
Precip_bf <- as.data.frame(Precip_bf)


#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites_30kbuf)[1]
site_list['ID'] <- as.numeric(seq(1,14)) 

Precip_bf_extr <- merge(site_list,Precip_bf, by.x="ID", by.y="ID")

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(Precip_bf_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Precip_buffer.csv")









# 
# #######################################################################
# #Reproject shapefile and create 30km buffer. I read shp file in with terra::vect() to use their buffer tool. 
# #The buffer function has a bug in Raster
# Sites_b <- vect("Shapefiles/Bat_sampling_locations.shp")
# 
# # compareCRS(NDVI_stk_b,Sites_b)
# # plot(NDVI_stk_b,1)
# # points(Sites_b)
# 
# 
# #change crs of sites
# Sites_b <- project(Sites_b, 'EPSG:29701')
# crs(Sites_b)
# 
# #sanity check
# plot(Precip_stk_prj,1)
# points(Sites_b)
# 
# 
# #extract data using buffer
# #values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
# Precip_bf <- terra::extract(Precip_stk_prj, Sites_b, 
#                             touches=FALSE)#,
# #fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
# Precip_bf <- as.data.frame(Precip_bf)
# 
# 
# #Extract Site_ID and joine to NDVI_data
# site_list <-as.data.frame(Sites_b)[1]
# site_list['ID'] <- as.numeric(seq(1,14)) 
# 
# Precip_bf_extr <- merge(site_list,Precip_bf, by.x="ID", by.y="ID")
# 
# #Write out file with all NDVI data points within 30 k buffer to table
# write.csv(Precip_bf_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Precip_point.csv")

