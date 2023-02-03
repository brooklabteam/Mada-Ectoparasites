#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

setwd("C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/")

#return a list of the raster to stack

files <- list.files("NDVI/RAW-data/",
             recursive=TRUE, #returns all files in multiple directories/folders
             full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
             pattern = "\\.tif") #filters list of files to those with '.tif' file type

files

#Read the files listed in 'files' into a raster stack
NDVI_stk <- stack(files)

#Rename the layers to shortent the file name

#original names
names(NDVI_stk)

#rename
names(NDVI_stk) <- paste0("NDVI_pt_",substr(names(NDVI_stk),62,67))

#new names
names(NDVI_stk)

# Open bat sampling site shapefile (points to extract data)
Sites <- readOGR(dsn = "Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
plot(Sites)
crs(Sites)

Sites_table <- as.data.frame(Sites)

#Ensure that raster and point file have matching CRS. TRUE = matching coordinate reference system (crs)
compareCRS(NDVI_stk,Sites)

#Sanity check
plot(NDVI_stk,1)
points(Sites)

#######################################################################

# Extract raster data using points
NDVI_pt <- extract(NDVI_stk, Sites)
NDVI_pt <- as.data.frame(NDVI_pt)

#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites)[1]

NDVI_pt_extr <- cbind(site_list,NDVI_pt)

#Write out file with all NDVI data for sampling point location to table
write.csv(NDVI_pt_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/NDVI_point.csv")

