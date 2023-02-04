#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

setwd("C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/")

#Doing each folder separate since some are day time and some are night time humidity values

#######################################################################
#Day time values first

#return a list of the raster to stack

files <- list.files("Humidity/Raw-data/Daytime",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.tif") #filters list of files to those with '.tif' file type

files

#must assign and projection so that we can meauser distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'

#read in raster stack (using terra), rename layers, and reproject raster
Humid_stk_b <- rast(c(files))
#original names
names(Humid_stk_b)
#rename
names(Humid_stk_b) <- paste0("Humid_bf_",substr((files),97,102))
#new names
names(Humid_stk_b)

#reproject raster
Humid_stk_prj <- project(Humid_stk_b, 'EPSG:29701')
crs(Humid_stk_prj)

#Reproject shapefile and create 30km buffer. I read shp file in with terra::vect() to use their buffer tool. 
#The buffer function has a bug in Raster
Sites_b <- vect("Shapefiles/Bat_sampling_locations.shp")

# compareCRS(Humid_stk_b,Sites_b)
# plot(Humid_stk_b,1)
# points(Sites_b)


#change crs of sites
Sites_b <- project(Sites_b, 'EPSG:29701')
crs(Sites_b)

#sanity check
plot(Humid_stk_prj,1)
points(Sites_b)

Sites_30kbuf <- buffer(Sites_b, 30000)

plot(Humid_stk_prj, 1)
points(Sites_b)
points(Sites_30kbuf)

#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
Humid_bf <- terra::extract(Humid_stk_prj, Sites_30kbuf, 
                          touches=FALSE)#,
                          #fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
Humid_bf <- as.data.frame(Humid_bf)


#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites_30kbuf)[1]
site_list['ID'] <- as.numeric(seq(1,14)) 

Humid_bf_extr <- merge(site_list,Humid_bf, by.x="ID", by.y="ID")

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(Humid_bf_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Humid_buffer_daytime.csv")

#######################################################################
#Nighttime values

#return a list of the raster to stack

files <- list.files("Humidity/Raw-data/Nighttime",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.tif") #filters list of files to those with '.tif' file type

files

#must assign and projection so that we can meauser distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'

#read in raster stack (using terra), rename layers, and reproject raster
Humid_stk_b <- rast(c(files))
#original names
names(Humid_stk_b)
#rename
names(Humid_stk_b) <- paste0("Humid_bf_",substr((files),101,106))
#new names
names(Humid_stk_b)

#reproject raster
Humid_stk_prj <- project(Humid_stk_b, 'EPSG:29701')
crs(Humid_stk_prj)

#Reproject shapefile and create 30km buffer. I read shp file in with terra::vect() to use their buffer tool. 
#The buffer function has a bug in Raster
Sites_b <- vect("Shapefiles/Bat_sampling_locations.shp")

# compareCRS(Humid_stk_b,Sites_b)
# plot(Humid_stk_b,1)
# points(Sites_b)


#change crs of sites
Sites_b <- project(Sites_b, 'EPSG:29701')
crs(Sites_b)

#sanity check
plot(Humid_stk_prj,1)
points(Sites_b)

Sites_30kbuf <- buffer(Sites_b, 30000)

plot(Humid_stk_prj, 1)
points(Sites_b)
points(Sites_30kbuf)

#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
Humid_bf <- terra::extract(Humid_stk_prj, Sites_30kbuf, 
                           touches=FALSE)#,
#fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
Humid_bf <- as.data.frame(Humid_bf)


#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites_30kbuf)[1]
site_list['ID'] <- as.numeric(seq(1,14)) 

Humid_bf_extr <- merge(site_list,Humid_bf, by.x="ID", by.y="ID")

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(Humid_bf_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Humid_buffer_nighttime.csv")

