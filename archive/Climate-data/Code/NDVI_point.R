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


#read in first set of files with matching extent
stk_a <- rast(c(files[1:18]))
stk_c <- rast(c(files[19:30]))
stk_c1 <- rast(c(files[31:50]))
stk_b <- rast(c(files[51:60]))
stk_d <- rast(c(files[61:82]))
stk_e <- rast(c(files[c(83,84)]))
stk_f <- rast(c(files[85:87]))
#read in second set of files with matching extent

#combine raster stacks

NDVI_stk_b<-c(stk_b,stk_c)
NDVI_stk_b<-c(NDVI_stk_b,stk_a)

#Read the files listed in 'files' into a raster stack
NDVI_stk<-stack(NDVI_stk_b)

NDVI_stk_c1<-stack(stk_c1)
NDVI_stk_e<-stack(stk_e)
NDVI_stk_f<-stack(stk_f)
NDVI_stk_d<-stack(stk_d)
#Rename the layers to shortent the file name

#original names
names(NDVI_stk)
?subst
#rename
names(NDVI_stk) <- paste0("NDVI_pt_",substr(names(NDVI_stk),71,76))
names(NDVI_stk_c1) <- paste0("NDVI_pt_",substr(names(NDVI_stk_c1),71,76))
names(NDVI_stk_e) <- paste0("NDVI_pt_",substr(names(NDVI_stk_e),71,76))
names(NDVI_stk_f) <- paste0("NDVI_pt_",substr(names(NDVI_stk_f),71,76))
names(NDVI_stk_d) <- paste0("NDVI_pt_",substr(names(NDVI_stk_d),71,76))
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
plot(NDVI_stk_b,13)
points(Sites)

#######################################################################

# Extract raster data using points
NDVI_pt <- extract(NDVI_stk, Sites)
NDVI_pt <- as.data.frame(NDVI_pt)


NDVI_pt1 <- extract(NDVI_stk_e, Sites)
NDVI_pt1 <- as.data.frame(NDVI_pt1)

NDVI_pt2 <- extract(NDVI_stk_f, Sites)
NDVI_pt2 <- as.data.frame(NDVI_pt2)

NDVI_pt3 <- extract(NDVI_stk_d, Sites)
NDVI_pt3 <- as.data.frame(NDVI_pt3)

NDVI_pt4 <- extract(NDVI_stk_c1, Sites)
NDVI_pt4 <- as.data.frame(NDVI_pt4)

#Extract Site_ID and joine to NDVI_data
site_list <-as.data.frame(Sites)[1]

NDVI_pt_extr<-cbind(site_list,NDVI_pt,NDVI_pt1,NDVI_pt2,NDVI_pt3,NDVI_pt4 )

#Write out file with all NDVI data for sampling point location to table
write.csv(NDVI_pt_extr, "D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/Climate-data/Climate_tables/NDVI_point.csv")

