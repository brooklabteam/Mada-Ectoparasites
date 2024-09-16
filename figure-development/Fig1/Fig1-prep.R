rm(list=ls())
library(tidyverse)
library(ggalluvial)
library(png)



# Set working drive
homewd = "/Users/carabrook/Developer/Mada-Ectoparasites"
setwd(homewd)

#######################################  Overall Alluvial  ######################################


# reads in the blood meal data set
data <- read.csv(paste0(homewd,"/data/20200628_AA_SG_Ectoparasite_Species.csv"))

# creates a new dataframe with a new column with the proportion of blood meals detected
alluvial <- data %>%
  as.data.frame() %>%
  mutate(Ecto_type = recode(Ecto_type, Batflies = "Bat fly")) %>% 
  select(Bat_species, Ecto_type) %>%
  dplyr::filter(!is.na(Ecto_type)) %>%
  dplyr::filter(Ecto_type != "Egg") %>%
  mutate(Bat_species = as.factor(Bat_species),
         Ecto_type = as.factor(Ecto_type)) %>%
  count(Bat_species, Ecto_type)
  
Plot <- ggplot(alluvial, aes(y = n, axis1 = Bat_species, axis2 = Ecto_type)) +
  geom_alluvium(aes(fill = Ecto_type), width =1/5, knot.pos = 0, na.rm=FALSE) +
  geom_stratum(width = 1/5,  color = "black", alpha = 0) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) # makes it really hard to tell that Pteropus rufus  only has 2 samples which are mites. looks like they have bat flies too. Need to play with labels and box sizes.
Plot


# Resources
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

### ALLUVIAL PLOT Using the Bipartite package

#Packages that we going to use for this analyse
library(bipartite)


# Import dataset
# (I use this lab dataset For the alluvial plot)
# This is the  data that We identified in the lab but not the one from the field
head(data)


# I try to check the list of ectoparasites
unique(data$Ecto_type)
data$Ecto_type[data$Ecto_type=="Bat fly"]<-"Bat flies"
data$Ecto_type[data$Ecto_type=="Batflies"]<-"Bat flies"
# Then try to check the bat species and change the name of the species
unique(data$Bat_species)
data$Bat_species[data$Bat_species=="Eiodolon_dupreaneum"]<-"Eiodolon dupreaneums"
data$Bat_species[data$Bat_species=="Pteropus_rufus"]<-"Pteropus rufus"
data$Bat_species[data$Bat_species=="Rousettus_madagascariensis"]<-"Rousettus madagascariensis"
data$Ecto_genus[is.na(data$Ecto_genus)]<-"Other mites"
unique(data$Ecto_genus)
#Since we don't want to use the "Egg" as ectoparasiste and we want to remove the NA 
# so we gona do a subset of the data with out "Egg" and NA 
data<-subset(data,Ecto_type!="Egg")
names(data)

# Now we are going to selecte the colume that we need for the alluvial plot
# here we need the two column "Bat species"=> column 3 and "Ecto type"=> column 5
bip<-data[c(3,5)]

# And from this two columns we gona make a contengency table with the frequence on it
bip1=table(bip$Bat_species,bip$Ecto_type)
bip1=print.table(bip1)
head(bip1)


# Now I save this data here 
write.csv(bip1,paste0(homewd,"/data/angelo_ectos.csv"), row.names = F)

# I re-import the data 
dat<-read.csv(paste0(homewd,"/data/angelo_ectos.csv"),sep = ',',dec = '.')

head(dat)
# Then I make the first column with the bat species  name (here named X by defauls) as column name
row.names(dat)<-rownames(bip1)
head(dat)


# Now I plot the interaction 
plotweb(dat,
        text.rot = 360,
        method="normal",
        bor.col.interaction ="grey80", col.high = "grey10",col.low="green",
        ybig = 1,arrow = "down.center",labsize = 1,x.lim = c(0,1.25),y.lim = c(0,2))


names(data)
bip2<-data[c(3,6)];
unique(bip2$Ecto_genus)
bip2$Ecto_genus[ is.na(bip2$Ecto_genus)]<-"Other mites"
#And I try to build a contengency table with the frequency 
bip2=table(bip2$Bat_species,bip2$Ecto_genus)
bip2=print.table(bip2)
head(bip2)
row.names(bip2)[row.names(bip2)=="Eiodolon dupreanums"] <- "Eidolon dupreanum"

write.csv(bip2,paste0(homewd,"/data/angelo.ectos2.csv"), row.names = F)


dat2<-read.csv(paste0(homewd,"/data/angelo.ectos2.csv"),sep = ',',dec = '.')

# Then I change the name of each row by the first column
row.names(dat2)<- c("Eidolon dupreanum", "Pteropus rufus", "Rousettus madagascariensis")
head(dat2)

png(paste0(homewd, "/final-figures/Fig1.png"))


# And I plot the web above the first one  (if add=TRUE )
plotweb(dat2, y.width.low=0.05, y.width.high=0.05, method="normal",
        low.y=1.4,high.y=1.8, col.low="green", text.low.col="black", 
        low.lab.dis=.05, low.lablength=20, bor.col.interaction ="grey80",
        high.lablength=12, high.lab.dis = .06,low.xoff = 0.0001,arrow = "down.center",
        x.lim = c(0,1.25),y.lim = c(0,1))
#?plotweb

#now bring in images

path_ecto<-paste0(homewd,"/images/nowbg/")
bat<-readPNG(paste0(path_ecto,"bats.png"))
rasterImage(bat,1.1, # back
            .4, # down
            1.2, #front
            .7) #up
com<-readPNG(paste0(path_ecto,"community.png"));rasterImage(com,1.1,.9,1.24,1.2)

fl<-readPNG(paste0(path_ecto,"fl.png"));rasterImage(fl,1,1.92,1.1,2.1)
om<-readPNG(paste0(path_ecto,"om.png"));rasterImage(om,0.8,1.92,0.87,2.1) 
tk<-readPNG(paste0(path_ecto,"tk.png"));rasterImage(tk,0.66,1.92,0.75,2.1)
me<-readPNG(paste0(path_ecto,"me.png"));rasterImage(me,0.52,1.92,.60,2.1) 
ms<-readPNG(paste0(path_ecto,"ms.png"));rasterImage(ms,0.3,1.9,.47,2.25) 
em<-readPNG(paste0(path_ecto,"em.png"));rasterImage(em,0.2,1.92,.33,2.2)
eg<-readPNG(paste0(path_ecto,"eg.png"));rasterImage(eg,0.1,1.97,.2,2.12)
cp<-readPNG(paste0(path_ecto,"cp.png"));rasterImage(cp,0.0,1.95,.13,2.3)

#and save the whole plot
dev.off()

