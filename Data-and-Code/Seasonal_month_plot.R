
rm(list=ls())

library(tidyverse)
library(readr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(ggplot2)
library(lubridate)
library(cowplot)
library(gratia)
library(patchwork)
# I creat my home work directory 
homewd="D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"

# creat the working directory
setwd(paste0(homewd))

# call the Ectoparasite data from the working directory
dat<-read.csv("Data-and-Code/Angelo_catch_dat_bart_age_2_13_2021.csv")
View(dat)
names(dat)
data1 <- dplyr::select(dat, roost_site, collection_date,
                       bat_species, bat_sex, 
                       bat_age_class, bat_weight_g,
                       body_length_cm, 
                       bat_forearm_mm, bat_tibia_mm,
                       ear_length_mm, gonad_length_mm, 
                       gonad_width_mm,meglastreblidae,bat_flies)


#check for NAs in dataset
nrow(data1[is.na(data1$collection_date),]) #0
nrow(data1[is.na(data1$bat_forearm_mm),]) #10
nrow(data1[is.na(data1$bat_weight_g),]) #19
nrow(data1[is.na(data1$bat_tibia_mm),]) #9
nrow(data1[is.na(data1$ear_length_mm),]) #10
nrow(data1[is.na(data1$meglastreblidae),]) #1311
nrow(data1[is.na(data1$bat_flies),]) #201
# Make numeric
data1$meglastreblidae<-as.numeric(data1$meglastreblidae)
data1$bat_flies<-as.numeric(data1$bat_flies)
data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
data1$ body_length_cm<-as.numeric(data1$ body_length_cm)
data1$ ear_length_mm<-as.numeric(data1$ ear_length_mm)
data1$bat_forearm_mm<-as.numeric(data1$bat_forearm_mm)
data1$bat_tibia_mm<-as.numeric(data1$bat_tibia_mm)
#change the date formaat
data1$dmy<-dmy(data1$collection_date)
data1$month<-month(data1$dmy)
# ROUSETTUS MAROMIZAHA
Rou<-subset(data1, bat_species=="Rousettus madagascariensis"&roost_site=="Maromizaha")
head(Rou)

# change the NA to 0 (zero)

Rou$bat_flies[is.na(Rou$bat_flies)]<-0 
unique(Rou$bat_sex)
Rou$bat_sex[Rou$bat_sex=="unknown"]<- "female"
Rou$bat_sex[Rou$bat_sex==""]<- NA
Rou$bat_sex[is.na(Rou$bat_sex)]<-"female" 


gamRou <- gam(bat_flies~ s(month, k=7, bs = "cc"), data = Rou)

plot(gamRou)

Rou$predicted_count <- predict.gam(gamRou,type="response")

#and to add in the confidence intervals
Rou$predicted_count_SE <- predict.gam(gamRou,type="response", se.fit = T)$se.fit
Rou$predicted_count_lci <- Rou$predicted_count -1.96*Rou$predicted_count_SE
Rou$predicted_count_uci <- Rou$predicted_count +1.96*Rou$predicted_count_SE
Rou$predicted_count_lci[Rou$predicted_count_lci<0] <- 0#now add the predictions to each dataframe



r1<-ggplot(data = Rou) + 
  geom_point(aes(x= month, y= bat_flies), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Month of the year")+ 
  ylab("Count of ectoparasite")+
  ggtitle("Eucampsipoda madagascariensis (Maromizaha)")+
  geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = Rou, aes(x=month, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_text(),
        axis.title.y = element_text())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));r1


#### CLIMATIC DATA ###########################################
## HUMIDITY DAY NIGHT ####
HD<-read.csv("Climate-data/Climate_tables/Humnday_trasposed.csv")

#Plot of the  Humidity at day
hdm<-ggplot(data = HD)+
  geom_point(aes(x=month,y=Maromizaha),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=Maromizaha))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Day time humidity")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title= element_blank(),
                   axis.text.x = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));hdm

## HUMIDITY NIGHT TIME ####

HN<-read.csv("Climate-data/Climate_tables/Humnight_trasposed.csv")

hnm<-ggplot(data = HN)+
  geom_point(aes(x=month,y=Maromizaha),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=Maromizaha))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Night time humidity")+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title.x= element_blank(),
                   axis.text.x = element_blank())+
  scale_y_continuous(breaks = seq(80,92,3))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));hnm

## TEMPERATURE ####
temp<-read.csv("Climate-data/Climate_tables/temp_trasposed.csv")


tpm<-ggplot(data = temp)+
  geom_point(aes(x=month,y=Maromizaha),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=Maromizaha))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Temperature")+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.x = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));tpm

## NDVI ###################
NDVI<-read.csv("Climate-data/Climate_tables/NDVI_trasposed.csv")

nm<-ggplot(data = NDVI)+
  geom_point(aes(x=month,y=Maromizaha),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=Maromizaha))+
  xlab("Month of the year (2013 jan-2020 mar)")+ 
  ylab("NDVI")+
  #ggtitle("Angavokely(loess)")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank(),
                   #axis.title.x = element_blank(),
                   axis.title.y = element_blank()
  )+
  scale_y_continuous(breaks=round(seq(.4,.9,.04),2))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));nm

## Combine all of the plot for AngavoKely


# Eidolon AngavoKely
Eid<-subset(data1, bat_species=="Eidolon dupreanum"&roost_site=="AngavoKely")
head(Eid)

# change the NA to 0 (zero)

Eid$bat_flies[is.na(Eid$bat_flies)]<-0 
unique(Eid$bat_sex)
Eid$bat_sex[Eid$bat_sex=="unknown"]<- "female"
Eid$bat_sex[Eid$bat_sex==""]<- NA
Eid$bat_sex[is.na(Eid$bat_sex)]<-"female" 


gamEid <- gam(bat_flies~ s(month, k=7, bs = "cc"), data = Eid)

plot(gamEid)

Eid$predicted_count <- predict.gam(gamEid,type="response")

#and to add in the confidence intervals
Eid$predicted_count_SE <- predict.gam(gamEid,type="response", se.fit = T)$se.fit
Eid$predicted_count_lci <- Eid$predicted_count -1.96*Eid$predicted_count_SE
Eid$predicted_count_uci <- Eid$predicted_count +1.96*Eid$predicted_count_SE
Eid$predicted_count_lci[Eid$predicted_count_lci<0] <- 0#now add the predictions to each dataframe



b2<-ggplot(data = Eid) + 
  geom_point(aes(x= month, y= bat_flies), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Month of the year")+ 
  ylab("Count E.madagascariensis")+
  ggtitle("Cyclopodia dubia (Angavokely)")+
  geom_ribbon(data = Eid, aes(x= month, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = Eid, aes(x=month, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));b2


#### CLIMATIC DATA ###########################################
## HUMIDITY DAY NIGHT ####
HD<-read.csv("Climate-data/Climate_tables/Humnday_trasposed.csv")

#Plot of the  Humidity at day
hde<-ggplot(data = HD)+
  geom_point(aes(x=month,y=AngavoKely),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoKely))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Day time humidity")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title= element_blank(),
                   axis.text.x = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));hde

## HUMIDITY NIGHT TIME ####

HN<-read.csv("Climate-data/Climate_tables/Humnight_trasposed.csv")

hne<-ggplot(data = HN)+
  geom_point(aes(x=month,y=AngavoKely),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoKely))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Night time humidity")+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title= element_blank(),
                   axis.text.x = element_blank())+
  scale_y_continuous(breaks = seq(80,92,3))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));hne

## TEMPERATURE ####
temp<-read.csv("Climate-data/Climate_tables/temp_trasposed.csv")


tpe<-ggplot(data = temp)+
  geom_point(aes(x=month,y=AngavoKely),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoKely))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Temperatur")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title = element_blank(),
                   axis.text.x = element_blank())+
  scale_y_continuous(breaks = seq(12,20,2))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));tpe

## NDVI ###################
NDVI<-read.csv("Climate-data/Climate_tables/NDVI_trasposed.csv")

ne<-ggplot(data = NDVI)+
  geom_point(aes(x=month,y=AngavoKely),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoKely))+
  xlab("Month of the year (2013 jan-2020 mar)")+ 
  ylab("NDVI")+
  #ggtitle("AngavoKely(loess)")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank(),
                   #axis.title.x = element_blank(),
                   axis.title.y = element_blank()
  )+
  scale_y_continuous(breaks=round(seq(.4,.9,.04),2))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));ne

## Combine all of the plot for AngavoKely

cowplot::plot_grid(r1+theme(axis.title.x = element_blank(),
                            axis.text.x = element_blank()),
                   b2+theme(axis.title.x = element_blank()),
                   hdm,hde,hnm,hne,tpm,tpe,nm,ne,nnrow = 5,ncol=2)
ggsave(paste0(homewd,"/Other-figures/Allclimate_batflies_by_month.png"),
       units = "mm",height = 90,width = 60, dpi = 1000,scale = 3)
