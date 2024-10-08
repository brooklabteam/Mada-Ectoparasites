rm(list=ls())
library(tidyverse)
library(ggplot2)
library(ggalluvial)
library(png)



# Set working drive
homewd = "/Users/carabrook/Developer/Mada-Ectoparasites"
setwd(homewd)

#######################################  Overall Alluvial  ######################################


# reads in the blood meal data set
data <- read.csv(paste0(homewd,"/data/20200628_AA_SG_EidRou.csv"))

# head(data)
# # creates a new dataframe with a new column with the proportion of blood meals detected
# alluvial <- data %>%
#   as.data.frame() %>%
#   mutate(Ecto_type = recode(Ecto_type, Batflies = "Bat fly")) %>% 
#   select(Bat_species, Ecto_type) %>%
#   dplyr::filter(!is.na(Ecto_type)) %>%
#   dplyr::filter(Ecto_type != "Egg") %>%
#   mutate(Bat_species = as.factor(Bat_species),Ecto_type = as.factor(Ecto_type)) %>%
#   count(Bat_species, Ecto_type)
#   
# Plot <- ggplot(alluvial, aes(y = n, axis1 = Bat_species, axis2 = Ecto_type)) +
#   geom_alluvium(aes(fill = Ecto_type), width =1/5, knot.pos = 0, na.rm=FALSE) +
#   geom_stratum(width = 1/5,  color = "black", alpha = 0) +
#   geom_label(stat = "stratum", aes(label = after_stat(stratum))) # makes it really hard to tell that Pteropus rufus  only has 2 samples which are mites. looks like they have bat flies too. Need to play with labels and box sizes.
# Plot


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
data$Bat_species[data$Bat_species=="Eiodolon_dupreaneum"]<-"Eidolon dupreanum"
data$Bat_species[data$Bat_species=="Rousettus_madagascariensis"]<-"Rousettus madagascariensis"
data$Ecto_genus[is.na(data$Ecto_genus)]<-"Other mites"
unique(data$Ecto_genus)
#Since we don't want to use the "Egg" as ectoparasiste and we want to remove the NA 
# so we gona do a subset of the data with out "Egg" and NA 
data<-subset(data,Ecto_type!="Egg")
names(data)

#and remove those from dataset for which we don't have corresponding bat metadata
cap <- read.csv(file= paste0(homewd, "/data/ecto_dat_long.csv"), header = T, stringsAsFactors = F)
head(cap,2)

cap$bat_flies <-as.numeric(cap$bat_flies)
cap$meglastreblidae <-as.numeric(cap$meglastreblidae)
cap$fleas <-as.numeric(cap$fleas)
cap$mites <-as.numeric(cap$mites)
cap$ticks <-as.numeric(cap$ticks)
cap$t_star <-as.numeric(cap$t_star)

cap$total_ectos =rowSums(cbind(cap$bat_flies, cap$meglastreblidae, cap$fleas,cap$mites, cap$ticks,cap$t_star), na.rm = T)

setdiff(data$Bat_SampleID, cap$sampleid) #"KELunlabeld1" "MIZ305X"      "MIZ306X"      "MIZ307X"      "MIZ388"      "MIZ478"    
#these will get discarded

data = subset(data, Bat_SampleID!="KELunlabeld1" & Bat_SampleID!= "MIZ305X"& Bat_SampleID!="MIZ306X"  & Bat_SampleID!="MIZ307X"& Bat_SampleID!= "MIZ388"& Bat_SampleID!="MIZ478")
data=subset(data, !is.na(Ecto_type))
names(data)[names(data)=="Bat_SampleID"] <- "sampleid"
setdiff(data$sampleid, cap$sampleid)
length(unique(intersect(data$sampleid, cap$sampleid))) #818
length(unique(data$sampleid)) #818
cap$sampleid[duplicated(cap$sampleid)]

#cap <- dplyr::select(cap, sampleid, roost_site, sampling_session, processing_date, bat_species, bat_sex, bat_flies, meglastreblidae, fleas, mites, ticks, t_star, mass_forearm_residual)

#and make into long
#reshape(cap, direction="long")

#MERGE the two data sets from the "SampleID" colum

# 1) read in both datasets with:  datasetName <- read.csv("datasetName.csv")
# 2) check colnames of both with: colnames(datasetName)
# 3) if my ID column has different names, I change them to match with: 

# colnames(Ectos)[IDcolumnNumber] <- c("IDcolumnName")


# 4) once you have matching column names, you can use the merge function

afa<-merge(data, cap, by = "sampleid",all=T)

head(afa)
names(afa)

unique(afa$bat_sex)

afa = subset(afa, !is.na(bat_sex) & bat_sex!="unknown")

data <- dplyr::select(afa, 1:13)
data = subset(data, !is.na(Ecto_type))
head(data)


# Now we are going to selecte the colume that we need for the alluvial plot
# here we need the two column "Bat species"=> column 4 and "Ecto type"=> column 5
bip<-data[c(4,5)]

# And from this two columns we gona make a contengency table with the frequence on it
bip1=table(bip$Bat_species,bip$Ecto_type)
# Now I save this data here 
write.csv(bip1,paste0(homewd,"/data/angelo_ectos.csv"), row.names = F)

# I re-import the data 
dat<-read.csv(paste0(homewd,"/data/angelo_ectos.csv"),sep = ',',dec = '.')

head(dat)

# Then I make the first column with the bat species  name (here named X by defauls) as column name
row.names(dat)<-rownames(bip1)
head(dat)


# Now I plot the interaction  - this is plot 1
plotweb(dat,
        text.rot = 360,
        method="normal",
        bor.col.interaction ="grey80", col.high = "grey10",col.low="green",
        ybig = 1,arrow = "down.center",labsize = 1,x.lim = c(0,1.25),y.lim = c(0,2))


names(data)
bip2<-data[c(4,6)];
unique(bip2$Ecto_genus)
bip2$Ecto_genus[ is.na(bip2$Ecto_genus)]<-"Other mites"
#And I try to build a contengency table with the frequency 
bip2=table(bip2$Bat_species,bip2$Ecto_genus)
bip2=print.table(bip2)
head(bip2)


write.csv(bip2,paste0(homewd,"/data/angelo.ectos2.csv"), row.names = F)


dat2<-read.csv(paste0(homewd,"/data/angelo.ectos2.csv"),sep = ',',dec = '.')

# Then I change the name of each row by the first column
row.names(dat2)<- c("Eidolon dupreanum",  "Rousettus madagascariensis")
head(dat2)

#png(paste0(homewd, "/final-figures/Fig1.png"))

names(dat2)[names(dat2)=="Other.mites"] <- "Other mites"
# And I plot the web above the first one  (if add=TRUE )
plotweb(dat2, y.width.low=0.05, y.width.high=0.05, method="normal",
         col.low="green", text.low.col="black", 
        low.lab.dis=.05, low.lablength=20, bor.col.interaction ="grey80",
        arrow = "down.center")
#?plotweb

