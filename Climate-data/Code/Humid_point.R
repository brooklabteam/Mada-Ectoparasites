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

#Read the files listed in 'files' into a raster stack
Humid_stk <- stack(files)

#Rename the layers to shorten the file name

#original names
names(Humid_stk)

#rename
names(Humid_stk) <- paste0("Humid_pt_daytime_",substr((files),97,100))

#new names
names(Humid_stk)

# Open bat sampling site shapefile (points to extract data)
Sites <- readOGR(dsn = "Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
plot(Sites)
crs(Sites)

Sites_table <- as.data.frame(Sites)

#Ensure that raster and point file have matching CRS. TRUE = matching coordinate reference system (crs)
compareCRS(Humid_stk,Sites)

#Sanity check
plot(Humid_stk,1)
points(Sites)

#######################################################################

# Extract raster data using points
Humid_pt <- extract(Humid_stk, Sites)
Humid_pt <- as.data.frame(Humid_pt)

#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites)[1]

Humid_pt_extr <- cbind(site_list,Humid_pt)

#Write out file with all NDVI data for sampling point location to table
write.csv(Humid_pt_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Humid_point_daytime.csv")

#######################################################################

#Nighttime values

files <- list.files("Humidity/Raw-data/Nighttime",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.tif") #filters list of files to those with '.tif' file type

files

#Read the files listed in 'files' into a raster stack
Humid_stk <- stack(files)

#Rename the layers to shorten the file name

#original names
names(Humid_stk)

#rename
names(Humid_stk) <- paste0("Humid_pt_nighttime_",substr((files),101,106))

#new names
names(Humid_stk)

# Open bat sampling site shapefile (points to extract data)
Sites <- readOGR(dsn = "Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
plot(Sites)
crs(Sites)

Sites_table <- as.data.frame(Sites)

#Ensure that raster and point file have matching CRS. TRUE = matching coordinate reference system (crs)
compareCRS(Humid_stk,Sites)

#Sanity check
plot(Humid_stk,1)
points(Sites)

#######################################################################

# Extract raster data using points
Humid_pt <- extract(Humid_stk, Sites)
Humid_pt <- as.data.frame(Humid_pt)

#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites)[1]

Humid_pt_extr <- cbind(site_list,Humid_pt)

#Write out file with all NDVI data for sampling point location to table
write.csv(Humid_pt_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Humid_point_nighttime.csv")
