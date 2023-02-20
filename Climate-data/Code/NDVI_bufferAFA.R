#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

setwd("D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/Climate-data/")

#return a list of the raster to stack

files <- list.files("NDVI/RAW-data/",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.tif") #filters list of files to those with '.tif' file type

files


#must assign and projection so that we can meauser distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'

#read in raster stack (using terra), rename layers, and reproject raster
#read in first set of files with matching extent
stk_a <- rast(c(files[1:18]))
stk_c <- rast(c(files[19:30]))
stk_c1 <- rast(c(files[31:50]))
stk_b <- rast(c(files[51:60]))
stk_d <- rast(c(files[61:82]))
stk_e <- rast(c(files[c(83,84)]))
stk_f <- rast(c(files[85:87]))
#original names
names(stk_d)
#rename
names(stk_a) <- paste0("NDVI_bf_",substr(names(stk_a),71,76))
names(stk_b) <- paste0("NDVI_bf_",substr(names(stk_b),71,76))
names(stk_c) <- paste0("NDVI_bf_",substr(names(stk_c),71,76))
names(stk_c1) <- paste0("NDVI_bf_",substr(names(stk_c1),71,76))
names(stk_e) <- paste0("NDVI_bf_",substr(names(stk_e),71,76))
names(stk_f) <- paste0("NDVI_bf_",substr(names(stk_f),71,76))
names(stk_d) <- paste0("NDVI_bf_",substr(names(stk_d),71,76))
#new names
names(stk_a)
#reproject raster
stk_prj_a <- project(stk_a, 'EPSG:29701')
stk_prj_b <- project(stk_b, 'EPSG:29701')
stk_prj_c <- project(stk_c, 'EPSG:29701')
stk_prj_c1 <- project(stk_c1, 'EPSG:29701')
stk_prj_d <- project(stk_d, 'EPSG:29701')
stk_prj_e <- project(stk_e, 'EPSG:29701')
stk_prj_f <- project(stk_f, 'EPSG:29701')
crs(stk_a)

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
plot(stk_prj_a,1)
points(Sites_b)

Sites_30kbuf <- buffer(Sites_b, 30000)

plot(stk_prj_a, 1)
points(Sites_b)
points(Sites_30kbuf)

#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
NDVI_bf1 <- extract(stk_prj_a, Sites_30kbuf,touches=FALSE)
NDVI_bf1 <- as.data.frame(NDVI_bf1)

NDVI_bf2 <- extract(stk_prj_b, Sites_30kbuf,touches=FALSE)
NDVI_bf2 <- as.data.frame(NDVI_bf2)


NDVI_bf3 <- extract(stk_prj_c, Sites_30kbuf,touches=FALSE)
NDVI_bf3 <- as.data.frame(NDVI_bf3)

NDVI_bf4 <- extract(stk_prj_c1, Sites_30kbuf,touches=FALSE)
NDVI_bf4 <- as.data.frame(NDVI_bf4)

NDVI_bf5 <- extract(stk_prj_d, Sites_30kbuf,touches=FALSE)
NDVI_bf5 <- as.data.frame(NDVI_bf5)

NDVI_bf6 <- extract(stk_prj_e, Sites_30kbuf,touches=FALSE)
NDVI_bf6 <- as.data.frame(NDVI_bf6)

NDVI_bf7 <- extract(stk_prj_f, Sites_30kbuf,touches=FALSE)
NDVI_bf7 <- as.data.frame(NDVI_bf7)
View(NDVI_bf7)

#combine all of the extracted NDVI data

NDVI_bf_extr<-cbind(NDVI_bf1,NDVI_bf2,NDVI_bf3,NDVI_bf4,NDVI_bf5,NDVI_bf6,NDVI_bf7)
names(NDVI_bf_extr)

NDVI_bf_extr<-NDVI_bf_extr[-c(20,31,44,65,88,91,95)] # Remove the reapeted columns
#Extract Site_ID 
site_list <-as.data.frame(Sites_30kbuf)[1]
site_list['ID'] <- as.numeric(seq(1,14))

#Joine Site_ID to NDVI_data
NDVI_bf_extr<-merge(site_list,NDVI_bf_extr)

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(NDVI_bf_extr, "D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/Climate-data/Climate_tables/NDVI_bf.csv")
