######################################
#Clear work environment
rm(list=ls())

#Set working directory - add your working directory here
homewd= "/Users/carabrook/Developer/Mada-Ectoparasites"
#Angelo-- add your working directory here and add a "#" before my working directory above

#Place individual log files within this folder
setwd(paste0(homewd))

# Packages to use 
library(tidyverse)
library(readr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(ggplot2)
library(gratia)

# Data importation
ectos <- read.csv(paste0(homewd,"/data/20200628_AA_SG_EidRou.csv"), sep = ",")
head(ectos)
cap <- read.csv(file= paste0(homewd, "/data/ecto_dat_long.csv"), header = T, stringsAsFactors = F)
head(ectos)
head(cap,2)
names(ectos)
names(cap)
#MERGE the two data sets from the "SampleID" colum

# 1) read in both datasets with:  datasetName <- read.csv("datasetName.csv")
# 2) check colnames of both with: colnames(datasetName)
# 3) if my ID column has different names, I change them to match with: 

# colnames(Ectos)[IDcolumnNumber] <- c("IDcolumnName")
names(ectos)[names(ectos)=="Bat_SampleID"] <- c("sampleid") 

# 4) once you have matching column names, you can use the merge function

afa<-merge(ectos, cap, by = "sampleid",all=T)

head(afa)
names(afa)
# Data CHOICE
# Count is the number of Cyclopodia male+female
# Male and Female= numbers of the cyclopodia Male and Female
data1 <- dplyr::select(afa, roost_site, processing_date,
                       bat_species, bat_sex, 
                       bat_age_class, bat_weight_g,
                       body_length_cm, sampleid,
                       bat_forearm_mm, 
                       Ecto_species,
                       Count, Male, Female)

data1<-dplyr::mutate(afa,Daty=processing_date)
data1$processing_date <- as.Date(data1$processing_date, format ="%m/%d/%y")
data1$Daty <- as.Date(data1$Daty, format = "%m/%d/%y")
data1<-data_sep<-separate(data1, col = Daty, into = c("year", "month","day"), sep = "-")

#View(data1)
head(data1,2)
tail(data1)
str(data1)

data1$Count[is.na(data1$Count)]<-0  # I change NA to O because that means there is no ectoparasite in the individual
# how ever i should let NA for the number of ectoparasites in each sex


data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
data1$ body_length_cm<-as.numeric(data1$ body_length_cm)
data1$ month<-as.numeric(data1$ month)
data1$year<-as.numeric(data1$year)
data1$day<-as.numeric(data1$day)
data1$Count<-as.numeric(data1$Count)
data1$bat_flies<-as.numeric(data1$bat_flies)

ECD<-data1%>%
  filter(bat_species=="Eidolon dupreanum",
         Ecto_species!="")


head(ECD)
unique(ECD$Ecto_genus)
tail(ECD,2)


cor.test(ECD$Count[ECD$Ecto_genus=="Cyclopodia"],ECD$bat_flies[ECD$Ecto_genus=="Cyclopodia"])
p1 <- ggplot(data=subset(ECD,Ecto_genus=="Cyclopodia"))+
  geom_point(aes(x=Count,y=bat_flies),size=3, alpha=.2,color="red")+
  labs(title="Cyclopodia dubia",x="Recount in the lab",y= "Field count")+
  geom_text(aes(x=5, y=17, label="R=0.92,p<0.001***"),col="grey0", size=5)+
  theme_bw()+
  theme(axis.text = element_text(size=12),
        plot.title = element_text(face = "italic"),
        axis.title = element_text(size=14)); p1


REM<-data1%>%
  filter(bat_species=="Rousettus madagascariensis",
         Ecto_species!="")

head(REM)
unique(REM$Ecto_genus)


cor.test(REM$Count[REM$Ecto_genus=="Eucampsipoda"],REM$bat_flies[REM$Ecto_genus=="Eucampsipoda"])
p2 <- ggplot(data=subset(REM,Ecto_genus=="Eucampsipoda"))+
  geom_point(aes(x=Count,y=bat_flies),size=3, alpha=.2,color="red")+
  geom_text(aes(x=8, y=36, label="R=0.91,p<0.001***"),col="grey0", size=5)+
  labs(title="Eucampsipoda madagascariensis",x="Recount in the lab",y= "Field count")+
  theme_bw()+
  theme(axis.text = element_text(size=12),
        plot.title = element_text(face = "italic"),
        axis.title = element_text(size=14)); p2


FigS1 <- cowplot::plot_grid(p1,p2, labels = c("A", "B"), label_size = 22, ncol = 2, nrow = 1)

ggsave(file = paste0(homewd, "/final-figures/FigS1.png"),
       plot = FigS1,
       units="mm",  
       width=90, 
       height=55, 
       scale=2.5, 
       dpi=300)
