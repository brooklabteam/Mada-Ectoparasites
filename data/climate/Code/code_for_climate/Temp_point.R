#Extract raster (environmental data) from points

library(raster)
library(rgdal)
library(terra)


# I creat my home work directory 
homewd="D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"

# creat the working directory
setwd(paste0(homewd,"Climate-data/"))

#return a list of the raster to stack

files <- list.files("Temperature/",
                    recursive=TRUE, #returns all files in multiple directories/folders
                    full.names = TRUE, #returns full filepath (this will also be the layer name in the stack)
                    pattern = "\\.nc") #filters list of files to those with '.tif' file type

files

#
Temp_stka <- rast(files[1:60])
Temp_stkb <- rast(files[61:84])
#Read the files listed in 'files' into a raster stack

Temp_stka<-stack(Temp_stka)
Temp_stkb<-stack(Temp_stkb)
#Rename the layers to shortent the file name

#original names
names(Temp_stka)

#rename
names(Temp_stka) <- paste0("Temp_pt_",substr((files[1:60]),62,68))
names(Temp_stkb) <- paste0("Temp_pt_",substr((files[61:84]),62,68))
#new names
names(Temp_stka)
names(Temp_stkb)

# Open bat sampling site shapefile (points to extract data)
Sites <- readOGR(dsn = "Shapefiles/Bat_sampling_locations.shp", layer = "Bat_sampling_locations")
plot(Sites)
crs(Sites)

Sites_table <- as.data.frame(Sites)

#Ensure that raster and point file have matching CRS. TRUE = matching coordinate reference system (crs)
compareCRS(Temp_stka,Sites)

#Sanity check
plot(Temp_stk,1)
points(Sites)

#######################################################################

# Extract raster data using points
Temp_pta <- extract(Temp_stka, Sites)
Temp_pta <- as.data.frame(Temp_pta)

Temp_ptb <- extract(Temp_stkb, Sites)
Temp_ptb <- as.data.frame(Temp_ptb)

# Combine the two data

Temp_pt<-cbind(Temp_pta,Temp_ptb)

#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites)[1]

Temp_pt_extr <- cbind(site_list,Temp_pt)

#Write out file with all NDVI data for sampling point location to table
write.csv(Temp_pt_extr, paste0(homewd,"/Climate-data/Climate_tables/Temp_point_AFA.csv"))
