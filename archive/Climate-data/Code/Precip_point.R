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

#loop through rasters layers and extract values at points
for (i in files) {
  
  precip_rast <- rast(i)
  names(precip_rast) <- paste0("Precip_pt_",substr(i,71,76))
  
  precip_rast <- resample(precip_rast, ref_rast) #needed for the new layers
  
  precip_rast <- mask(precip_rast,ref_rast)
  
  Precip_pt <- terra::extract(precip_rast, Sites, na.rm=FALSE)
  
  Precip_dat <- left_join(Precip_dat, Precip_pt)
  
}

#shows list of column names 
ls(Precip_dat)

#Write out file with all NDVI data for sampling point location to table
write.csv(Precip_dat, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Precip_point.csv")

