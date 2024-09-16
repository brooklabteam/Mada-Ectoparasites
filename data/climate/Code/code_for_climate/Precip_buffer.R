#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)

#setwd("C:/Users/kathe/Documents/GitHub/Mada-Ectoparasites/Climate-data/")

# I creat my home work directory 
homewd="D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"

# creat the working directory
setwd(paste0(homewd,"Climate-data/"))

#return a list of the raster to stack

files <- list.files("Precipitation/",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.nc") #filters list of files to those with '.tif' file type

files

#files have different extents and need to be corrected before stacking

#read in first set of files with matching extent

#
PPT_stka <- rast(files[1:60])
PPT_stkb <- rast(files[61:63])
PPT_stkc <- rast(files[64:72])
PPT_stkd <- rast(files[73:84])
#PPT_stk_f <- rast(c(files[85:87]))


#original names
names(PPT_stkb)

#rename
names(PPT_stka) <- paste0("Precip_pt_",substr(files[1:60],71,76))
names(PPT_stkb) <- paste0("Precip_pt_",substr(files[61:63],71,76))
names(PPT_stkc) <- paste0("Precip_pt_",substr(files[64:72],71,76))
names(PPT_stkd) <- paste0("Precip_pt_",substr(files[73:84],71,76))

#new names
names(PPT_stkb)



# Open bat sampling site shapefile (points to extract data)
Sites <- vect("Shapefiles/Bat_sampling_locations.shp")

# compareCRS(NDVI_stk_b,Sites_b)
# plot(NDVI_stk_b,1)
# points(Sites_b)


#change crs of sites
Sites<- project(Sites, 'EPSG:29701')
crs(Sites)

#Ensure that raster and point file have matching CRS. TRUE = matching coordinate reference system (crs)
compareCRS(PPT_stka,Sites)

#Sanity check
plot(PPT_stk_prja,1)
points(Sites)


#must assign and projection so that we can measure distance in meaningful units 
#we can't do this in unprojected geographic lat/long
#new projection will have units of 'm'
#reproject raster
PPT_stk_prja <- project(PPT_stka,'EPSG:29701')
crs(PPT_stk_prja)
PPT_stk_prjb <- project(PPT_stkb,'EPSG:29701')
crs(PPT_stk_prjb)
PPT_stk_prjc <- project(PPT_stkc, 'EPSG:29701')
crs(PPT_stk_prjc)
PPT_stk_prjd <- project(PPT_stkd, 'EPSG:29701')
crs(PPT_stk_prjd)


#######################################################################

# Extract raster data using points
PPT_pta <- extract(PPT_stk_prja, Sites)
PPT_pta <- as.data.frame(PPT_pta)

PPT_ptb <- extract(PPT_stk_prjb, Sites)
PPT_ptb <- as.data.frame(PPT_ptb)

PPT_ptc <- extract(PPT_stk_prjc, Sites)
PPT_ptc <- as.data.frame(PPT_ptc)

PPT_ptd <- extract(PPT_stk_prjd, Sites)
PPT_ptd <- as.data.frame(PPT_ptd)

# Combine the two data

PPT_pt<-cbind(PPT_pta,PPT_ptb,PPT_ptc,PPT_ptd)

View(PPT_pt)
#Extract Site_ID and joine to PPT_data
site_list <-as.data.frame(Sites)[1]
PPT_pt_extr <- cbind(site_list,PPT_pt)


#remove the un used columne
names(PPT_pt_extr)
PPT_pt_extr<-PPT_pt_extr[-c(63,67,77)]

#Write out file with all NDVI data for sampling point location to table
write.csv(PPT_pt_extr, paste0(homewd,"/Climate-data/Climate_tables/Precip_point_AFA.csv"))

#######################################################################
#######################################################################

#rename
names(PPT_stka) <- paste0("Precip_bf_",substr(files[1:60],71,76))
names(PPT_stkb) <- paste0("Precip_bf_",substr(files[61:63],71,76))
names(PPT_stkc) <- paste0("Precip_bf_",substr(files[64:72],71,76))
names(PPT_stkd) <- paste0("Precip_bf_",substr(files[73:84],71,76))
#Reproject shapefile and create 30km buffer. I read shp file in with terra::vect() to use their buffer tool. 
#The buffer function has a bug in Raster
Sites <- vect("Shapefiles/Bat_sampling_locations.shp")
#change crs of sites
Sites <- project(Sites, 'EPSG:29701')
crs(Sites)


#reproject raster
PPT_stk_prja <- project(PPT_stka,'EPSG:29701')
crs(PPT_stk_prja)
PPT_stk_prjb <- project(PPT_stkb,'EPSG:29701')
crs(PPT_stk_prjb)
PPT_stk_prjc <- project(PPT_stkc, 'EPSG:29701')
crs(PPT_stk_prjc)
PPT_stk_prjd <- project(PPT_stkd, 'EPSG:29701')
crs(PPT_stk_prjd)


#Add a buffer of 30 km (30000meters) around each point

Sites_30kbuf <- buffer(Sites, 30000)

plot(PPT_stk_prja, 2)
points(Sites)
points(Sites_30kbuf)

#change crs of sites
Sites <- project(Sites, 'EPSG:29701')
crs(Sites)


#extract data using buffer
#values of each cell within a buffer is returned. Use group_by() to calc min, max, mean, etc.

# Extract raster data using points
PPT_bfa <- extract(PPT_stk_prja, Sites_30kbuf,touchs=FALSE)
PPT_bfa <- as.data.frame(PPT_bfa)

PPT_bfb <- extract(PPT_stk_prjb, Sites_30kbuf,touchs=FALSE)
PPT_bfb <- as.data.frame(PPT_bfb)

PPT_bfc <- extract(PPT_stk_prjc, Sites_30kbuf,touchs=FALSE)
PPT_bfc <- as.data.frame(PPT_bfc)

PPT_bfd <- extract(PPT_stk_prjd, Sites_30kbuf,touchs=FALSE)
PPT_bfd <- as.data.frame(PPT_bfd)

#fun = mean, na.rm = TRUE) # Can use this to get mean, max, min of 30 km buffer
Precip_bfa <- as.data.frame(PPT_bfa)

# Combine all of the extracted data

Precip_bf<-cbind(PPT_bfa,PPT_bfb,PPT_bfc,PPT_bfd)
View(Precip_bf)
# Remove the un used column 
names(Precip_bf)
Precip_bf<-Precip_bf[-c(62,66,76)]
#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites)[1]
site_list['ID'] <- as.numeric(seq(1,14)) 

Precip_bf_extr <- merge(site_list,Precip_bf, by.x="ID", by.y="ID")

#Write out file with all NDVI data points within 30 k buffer to table
write.csv(PPT_pt_extr, paste0(homewd,"/Climate-data/Climate_tables/Precip_buffer_AFA.csv"))
