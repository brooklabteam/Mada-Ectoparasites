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

#Read in the shapefile of points
Sites <- vect("Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
#plot(Sites)

#Create a reference raster to use as key
ref_rast <- rast(files[1])

#create a blank dataset
NDVI_dat <- tibble(ID=seq(1,14,by=1)) #create ID column for joining extracted data

#include site names
site_list <-as.data.frame(Sites)[1]
NDVI_dat <- cbind(NDVI_dat,site_list)

#loop through rasters layers and extract values at points
for (i in files) {
  
  NDVI_rast <- rast(i)
  names(NDVI_rast) <- paste0("NDVI_pt_",substr(i,90,95))
  
  NDVI_rast <- resample(NDVI_rast, ref_rast) #needed for the new layers
  
  NDVI_rast <- mask(NDVI_rast,ref_rast)
  
  NDVI_pt <- terra::extract(NDVI_rast, Sites, na.rm=FALSE)
  
  NDVI_dat <- left_join(NDVI_dat, NDVI_pt)
  
}

#shows list of column names 
ls(NDVI_dat)

#Write out file with all NDVI data for sampling point location to table
write.csv(NDVI_dat, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/NDVI_point_KYupdate.csv")


