#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

# I creat my home work directory 
homewd="D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"

# creat the working directory
setwd(paste0(homewd,"Climate-data/"))

files <- list.files("Temperature/",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.nc") #filters list of files to those with '.tif' file type

files

#must assign and projection so that we can meauser distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'

#read in raster stack (using terra), rename layers, and reproject raster
Temp_stka <- rast(files[1:60])
Temp_stkb <- rast(files[61:84])

Temp_stkb<-crop(Temp_stkb,Temp_stka)
#Read the files listed in 'files' into a raster stack
Temp_stk<-cbind(Temp_stka,Temp_stkb)

#Temp_stka<-stack(Temp_stka)
#Temp_stkb<-stack(Temp_stkb)
#original names
names(Temp_stkb)
#rename
names(Temp_stka) <- paste0("Temp_bf_",substr((files[1:60]),63,68))
names(Temp_stkb) <- paste0("Temp_bf_",substr((files[61:84]),63,68))
#new names
class(Temp_stkb)
Temp_stk<-cbind(Temp_stka,Temp_stkb)

#reproject raster
Temp_stk_prja <- project(Temp_stka,'EPSG:29701')
crs(Temp_stk_prja)
Temp_stk_prjb <- project(Temp_stkb,'EPSG:29701')
crs(Temp_stk_prjb)

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
plot(Temp_stk_prja,1)
points(Sites_b)

Sites_30kbuf <- buffer(Sites_b, 30000)

plot(Temp_stk_prja, 2)
points(Sites_b)
points(Sites_30kbuf)

#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.
Temp_bfa <- terra::extract(Temp_stk_prja, Sites_30kbuf, 
                          touches=FALSE)#,
Temp_bfb <- terra::extract(Temp_stk_prjb, Sites_30kbuf, 
                           touches=FALSE)
#fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
Temp_bfa <- as.data.frame(Temp_bfa)
Temp_bfb <- as.data.frame(Temp_bfb)

# Combine the two data


Temp_bf<-cbind(Temp_bfa[-2,],Temp_bfb)
Temp_bf<-Temp_bf[,-62]
#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites_30kbuf)[1]
site_list['ID'] <- as.numeric(seq(1,14)) 

Temp_bf_extr <- merge(site_list,Temp_bf, by.x="ID", by.y="ID")

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(Temp_bf_extr, paste0(homewd,"/Climate-data/Climate_tables/Temp_buffer_AFA.csv"))
