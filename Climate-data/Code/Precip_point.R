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

#files have different extents and need to be corrected before stacking

#read in first set of files with matching extent
stk_a <- rast(c(files[1:3],
                files[13:24]))

#read in second set of files with matching extent
stk_b <- rast(c(files[4:12]))

#crop to one another to make extents match (forces matching min/max x and y values)
stk_a_c <- crop(stk_a, stk_b)
stk_b_c <- crop(stk_b, stk_a)

#combine raster stacks
PPT_stk_b <- c(stk_a_c, stk_b_c)

#original names
names(PPT_stk_b)
#rename
names(PPT_stk_b) <- paste0("Precip_pt_",substr((files),71,76))
#new names
names(PPT_stk_b)


# Open bat sampling site shapefile (points to extract data)
Sites <- readOGR(dsn = "Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
plot(Sites)
crs(Sites)

Sites_table <- as.data.frame(Sites)

#Ensure that raster and point file have matching CRS. TRUE = matching coordinate reference system (crs)
compareCRS(PPT_stk_b,Sites)

#Sanity check
plot(PPT_stk_b,1)
points(Sites)


#must assign and projection so that we can measure distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'
#reproject raster
Precip_stk_prj <- project(PPT_stk_b, 'EPSG:29701')
crs(Precip_stk_prj)

#######################################################################
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


#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
Precip_bf <- terra::extract(Precip_stk_prj, Sites_b, 
                            touches=FALSE)#,
#fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
Precip_bf <- as.data.frame(Precip_bf)


#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites_b)[1]
site_list['ID'] <- as.numeric(seq(1,14)) 

Precip_bf_extr <- merge(site_list,Precip_bf, by.x="ID", by.y="ID")

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(Precip_bf_extr, "C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/Climate_tables/Precip_point.csv")
