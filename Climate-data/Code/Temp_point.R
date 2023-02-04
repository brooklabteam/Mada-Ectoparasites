#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

setwd("C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/")


#return a list of the raster to stack

files <- list.files("Temperature/",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.nc") #filters list of files to those with '.tif' file type

files

#Read the files listed in 'files' into a raster stack
Temp_stk <- stack(files)

#Rename the layers to shortent the file name

#original names
names(Temp_stk)

#rename
names(Temp_stk) <- paste0("Temp_pt_",substr((files),63,68))

#new names
names(Temp_stk)

# Open bat sampling site shapefile (points to extract data)
Sites <- readOGR(dsn = "Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
plot(Sites)
crs(Sites)

Sites_table <- as.data.frame(Sites)

#Ensure that raster and point file have matching CRS. TRUE = matching coordinate reference system (crs)
compareCRS(Temp_stk,Sites)

#Sanity check
plot(Temp_stk,1)
points(Sites)

#######################################################################

# Extract raster data using points
Temp_pt <- extract(Temp_stk, Sites)
Temp_pt <- as.data.frame(Temp_pt)

#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites)[1]

Temp_pt_extr <- cbind(site_list,Temp_pt)

#Write out file with all NDVI data for sampling point location to table
write.csv(Temp_pt_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Temp_point.csv")

