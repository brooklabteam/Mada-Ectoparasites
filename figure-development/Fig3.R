#### CROSS CORRELATION EUCAMPSIPODA AND THE CLIMATE VARIABLE IN MAROMIZAHA####
rm(list=ls())

library(tidyverse)
library(readr)
library(dplyr)
library(plyr)
library(mgcv)
library(mgcViz)
library(ggplot2)
library(lubridate) 
library(cowplot)
library(gratia)
library(patchwork)
library(reshape2)
# I create my home work directory 
homewd="/Users/carabrook/Developer/Mada-Ectoparasites"

# create the working directory
setwd(paste0(homewd))

# call the Ectoparasite data from the working directory
dat<-read.csv(paste0(homewd,"/data/ecto_dat_long.csv"))
head(dat)
names(dat)
data1 <- dplyr::select(dat, roost_site, processing_date,
                       bat_species, bat_sex, 
                       bat_age_class, bat_weight_g,
                       bat_forearm_mm, 
                       meglastreblidae,bat_flies,mass_forearm_residual)


data1$processing_date <- as.Date(data1$processing_date, format = "%m/%d/%y")

#check for NAs in dataset
nrow(data1[is.na(data1$collection_date),]) #0
nrow(data1[is.na(data1$bat_forearm_mm),]) #8
nrow(data1[is.na(data1$bat_weight_g),]) #16
# nrow(data1[is.na(data1$bat_tibia_mm),]) #0
# nrow(data1[is.na(data1$ear_length_mm),]) #0
nrow(data1[is.na(data1$meglastreblidae),]) #972
nrow(data1[is.na(data1$bat_flies),]) #131
# Make numeric
data1$meglastreblidae<-as.numeric(data1$meglastreblidae)
data1$bat_flies<-as.numeric(data1$bat_flies)
data1$bat_weight_g<-as.numeric(data1$ bat_weight_g)
#change the date formaat
data1$dmy<-ymd(data1$processing_date)
data1$month<-month(data1$dmy)


data1=subset(data1, !is.na(bat_age_class))

####### ROUSETTUS MAROMIZAHA #############################################
Rou<-subset(data1, bat_species=="Rousettus madagascariensis"&roost_site=="Maromizaha_Rmad" & bat_age_class!="J")
head(Rou)
unique(data1$roost_site)

# change the NA to 0 (zero)

Rou$bat_flies[is.na(Rou$bat_flies)]<-0 
unique(Rou$bat_sex)
Rou$bat_sex[Rou$bat_sex=="unknown"]<- "female"
Rou$bat_sex[Rou$bat_sex==""]<- NA
Rou$bat_sex[is.na(Rou$bat_sex)]<-"female" 

#### Mean and Error standard of the ectoparasites - by month and year
head(Rou)
names(Rou)

Rou$year <- year(Rou$processing_date)
#Rou$month <- as.character(Rou$month)
#Rou$month[Rou$month=="1" | Rou$month=="2" | Rou$month=="3"| Rou$month=="4"| Rou$month=="5" | Rou$month=="6"| Rou$month=="7"| Rou$month=="8"| Rou$month=="9"] <- paste0("0", Rou$month[Rou$month=="1" | Rou$month=="2" | Rou$month=="3"| Rou$month=="4"| Rou$month=="5" | Rou$month=="6"| Rou$month=="7"| Rou$month=="8"| Rou$month=="9"])
#Rou$month_year <- as.Date(paste0(Rou$year, "-", Rou$month, "-01"))
#Rou$month <- as.numeric(Rou$month)

Rou1<-aggregate(Rou$bat_flies, list(Rou$month), FUN=mean)
Rou2<-aggregate(Rou$bat_flies, list(Rou$month), FUN=sd)

colnames(Rou1)<-c("month","mean");colnames(Rou2)<-c("month","sd")
Rou1<-merge(Rou1,Rou2,by="month")

se_Rou<-c()

for (i in unique(Rou1$month)) {
  if(i== i){
    se_Rou<-c(se_Rou,Rou1$sd[Rou1$month==i]/sqrt(length(Rou$month[Rou$month==i])))
  }
  
}
se_Rou

Rou1$se<-se_Rou
## Calculation of the intervale of confidents
Rou1$er_lci <- Rou1$mean -1.96*Rou1$se
Rou1$er_uci <- Rou1$mean +1.96*Rou1$se


###DATA PREPARATION FOR EIDOLON AND IT'S ECTOPARASITES #######

# Make a subset of the data for Eidolon dupreanum  from AngavoKely
Eid<-subset(data1, bat_species=="Eidolon dupreanum"&roost_site=="Angavokely_Edup" &bat_age_class!="J"| bat_species=="Eidolon dupreanum"&roost_site=="Angavobe_Edup" &bat_age_class!="J")
head(Eid)
unique(data1$roost_site)

# change the NA to 0 (zero)
# here NA means we did not get any ectoparasites 
Eid$bat_flies[is.na(Eid$bat_flies)]<-0 

# Check the doublon 
unique(Eid$bat_sex)

#change the miss speling names
Eid$bat_sex[Eid$bat_sex=="unknown"]<- "female"
Eid$bat_sex[Eid$bat_sex==""]<- NA
Eid$bat_sex[is.na(Eid$bat_sex)]<-"female" 

# Calculate the Mean and standard deviation of the ectoparasites for each month
names(Eid)
Eid$year <- year(Eid$processing_date)


Eid1<-aggregate(Eid$bat_flies, list(Eid$month), FUN=mean) #mean
Eid2<-aggregate(Eid$bat_flies, list(Eid$month), FUN=sd) #standard deviation

colnames(Eid1)<-c("month","mean");colnames(Eid2)<-c("month","sd")
Eid1<-merge(Eid1,Eid2,by="month")


head(Eid1)
# Calculate the Standard error 
se_Eid<-c()

for (i in unique(Eid1$month)) {
  if(i== i){
    se_Eid<-c(se_Eid,Eid1$sd[Eid1$month==i]/sqrt(length(Eid$month[Eid$month==i])))
  }
  
}
se_Eid

Eid1$se_Eid<-se_Eid

## Calculate the Confident Intervales
Eid1$er_lci <- Eid1$mean -1.96*Eid1$se_Eid #lower CI
Eid1$er_uci <- Eid1$mean +1.96*Eid1$se_Eid # Uper CI


#### IMPORTATION OF CLIMATIC DATA ###########################################


clim.dat<-read.csv(paste0(homewd,"/data/climate/Climate_tables/data_climate_giov.csv"))
head(clim.dat)

#add month_year column
clim.dat$month <- month(clim.dat$date)
clim.dat$year <- year(clim.dat$date)
# clim.dat$month <- as.character(clim.dat$month)
# clim.dat$month[clim.dat$month=="1" | clim.dat$month=="2" | clim.dat$month=="3"| clim.dat$month=="4"| clim.dat$month=="5" | clim.dat$month=="6"| clim.dat$month=="7"| clim.dat$month=="8"| clim.dat$month=="9"] <- paste0("0", clim.dat$month[clim.dat$month=="1" | clim.dat$month=="2" | clim.dat$month=="3"| clim.dat$month=="4"| clim.dat$month=="5" | clim.dat$month=="6"| clim.dat$month=="7"| clim.dat$month=="8"| clim.dat$month=="9"])
# clim.dat$month_year <- as.Date(paste0(clim.dat$year, "-", clim.dat$month, "-01"))
# clim.dat$month <- as.numeric(clim.dat$month)


clim.dat$daty<-ymd(clim.dat$date)
clim.dat$volana<-month(clim.dat$daty)


## CLIMATIC DATA and CROSS CORRELATION FOR MAROMIZAHA ####

miz.clim<-subset(clim.dat,sites=="Maromizaha")
head(miz.clim)

N=unique(ddply(miz.clim,.(month), summarise, N=length(unique(year)))$N)
#calculer l'humidite moyenne au cours des annees

miz.clim <- ddply(miz.clim, .(month), summarise,
                  mean_Hday = mean(humidity_day), 
                  mean_Temp = mean(temperature),
                  mean_prec=mean(precipitation),
                  se_Hday = sd(humidity_day)/sqrt(N), 
                  se_Temp = sd(temperature)/sqrt(N),
                  se_prec=sd(precipitation)/sqrt(N))




miz.clim$temp_uci <- miz.clim$mean_Temp+1.96*miz.clim$se_Temp
miz.clim$temp_lci <- miz.clim$mean_Temp-1.96*miz.clim$se_Temp

miz.clim$Hday_uci <- miz.clim$mean_Hday+1.96*miz.clim$se_Hday
miz.clim$Hday_lci <- miz.clim$mean_Hday-1.96*miz.clim$se_Hday

miz.clim$prec_uci <- miz.clim$mean_prec+1.96*miz.clim$se_prec
miz.clim$prec_lci <- miz.clim$mean_prec-1.96*miz.clim$se_prec





## CLIMATIC DATA and CROSS CORRELATION FOR ANGAVOKELY####

kel.clim<-subset(clim.dat,sites=="AngavoKely")
N=unique(ddply(kel.clim,.(month), summarise, N=length(unique(year)))$N)
#calculate the mean of the Humidity for each month since 2013

kel.clim <- ddply(kel.clim, .(month), summarise,
                  mean_Hday = mean(humidity_day), 
                  mean_Temp = mean(temperature),
                  mean_prec=mean(precipitation),
                  se_Hday = sd(humidity_day)/sqrt(N), 
                  se_Temp = sd(temperature)/sqrt(N),
                  se_prec=sd(precipitation)/sqrt(N))

kel.clim$temp_uci <- kel.clim$mean_Temp+1.96*kel.clim$se_Temp
kel.clim$temp_lci <- kel.clim$mean_Temp-1.96*kel.clim$se_Temp

kel.clim$Hday_uci <- kel.clim$mean_Hday+1.96*kel.clim$se_Hday
kel.clim$Hday_lci <- kel.clim$mean_Hday-1.96*kel.clim$se_Hday

kel.clim$prec_uci <- kel.clim$mean_prec+1.96*kel.clim$se_prec
kel.clim$prec_lci <- kel.clim$mean_prec-1.96*kel.clim$se_prec


#and plot

temp.miz<-ggplot(miz.clim)+
  geom_line(aes(x=month, y=mean),color="grey50",linewidth=1,data = Rou1)+
  geom_point(aes(x=month, y=mean),color="grey50",size=3,data = Rou1)+
  geom_errorbar(data = Rou1, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.5,width=.1 )+
  geom_line(aes(x=month,y=mean_Temp/2),color="red",linewidth=1)+
  geom_point(aes(x=month,y=mean_Temp/2),color="red",size=3)+
  geom_errorbar(aes(x= month,ymin=temp_lci/2, ymax=temp_uci/2), color="red",alpha=.5,width=.1 )+
  scale_color_manual(values = c("mean temperature"="red","mean"="black"))+
  scale_y_continuous(name = "mean temperature",limits = c(0,15),breaks = seq(0,14,2), labels = seq(0,14,2)*2,position = "right",sec.axis = sec_axis(~ ., name = "mean ectoparasites"))+
  theme_bw()+ theme(legend.position = 'none', panel.grid = element_blank(),
                    legend.title = element_blank(),
                    strip.text = element_text(size=14),
                    axis.title.y=element_text(size=14),
                    axis.title.y.right = element_text(size=14,angle = -90),
                    axis.title.x = element_blank(),
                    strip.background = element_rect(fill="white"),
                    legend.text = element_text(size=12),
                    plot.margin = unit(c(.2,.1,1.3,1.1), "lines"),
                    axis.line.y.right = element_line(color = "red"),
                    axis.line.y.left = element_line(color = "black"),
                    axis.text = element_text(size=13))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov"));temp.miz



hday.miz<-ggplot(miz.clim)+
  ggtitle("Eucampsipoda madagascariensis")+
  geom_line(aes(x=month, y=mean),color="grey50",linewidth=1,data = Rou1)+
  geom_point(aes(x=month, y=mean),color="grey50",size=3,data = Rou1)+
  geom_errorbar(data = Rou1, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.5,width=.1 )+
  geom_line(aes(x=month,y=mean_Hday/6),color="red",linewidth=1)+
  geom_point(aes(x=month,y=mean_Hday/6),color="red",size=3)+
  geom_errorbar(aes(x= month,ymin=Hday_lci/6, ymax=Hday_uci/6), color="red",alpha=.5,width=.1 )+
  scale_y_continuous(limits = c(0,15),name = "mean ectoparasites",sec.axis = sec_axis(~ .*6,name = "mean humidity"))+
  theme_bw() + theme(legend.position = "none", 
                     panel.grid = element_blank(),
                     legend.title = element_blank(),
                     #axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     plot.title = element_text(face="italic"),
                     axis.title.y.right = element_text(size=14,angle = -90),
                     strip.text = element_text(size=14),
                     axis.title.y=element_text(size=14), 
                     strip.background = element_rect(fill="white"),
                     axis.line.y.right = element_line(color = "red"),
                     axis.line.y.left = element_line(color = "black"),
                     legend.text = element_blank(),
                     plot.margin = unit(c(.2,.1,1.3,1.1), "lines"),
                     axis.text = element_text(size=12))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov")); hday.miz


prec.miz<-ggplot(miz.clim)+
  geom_line(aes(x=month, y=mean),color="grey50",linewidth=1,data = Rou1)+
  geom_point(aes(x=month, y=mean),color="grey50",size=3,data = Rou1)+
  geom_errorbar(data = Rou1, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.5,width=.1 )+
  geom_line(aes(x=month,y=mean_prec*10),color="red",linewidth=1)+
  geom_point(aes(x=month,y=mean_prec*10),color="red",size=3)+
  geom_errorbar(aes(x= month,ymin=prec_lci*10, ymax=prec_uci*10), color="red",alpha=.5,width=.1 )+
  scale_y_continuous(limits = c(0,15),"mean ectoparasites", sec.axis = sec_axis(~ ./10, name="mean precipitation rate"))+
  theme_bw() + theme(legend.position = "none", 
                     legend.text = element_blank(),
                     panel.grid = element_blank(),
                     strip.text = element_text(size=14),
                     strip.background = element_rect(fill="white"),
                     axis.title.y=element_text(size=13),
                     axis.title.y.right = element_text(size=14,angle=-90),
                     axis.title.x = element_blank(),
                     axis.text.y = element_text(size=13),
                     axis.text.x = element_blank(),
                     # axis.ticks.x = element_blank(),
                     axis.line.y.right = element_line(color = "red"),
                     axis.line.y.left = element_line(color = "black"),
                     plot.margin = unit(c(.2,.1,1.3,1.1), "lines"))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov"));prec.miz


library(cowplot)

plot_Rou<-cowplot::plot_grid(hday.miz, prec.miz, temp.miz, ncol=1, nrow=3, align = "v", axis = "lr");plot_Rou



### Plot the Climate data for Eidolon


temp.kel<-ggplot(kel.clim)+
  geom_line(aes(x=month, y=mean),color="grey50",linewidth=1,data = Eid1)+
  geom_point(aes(x=month, y=mean),color="grey50",size=3,data = Eid1)+
  geom_errorbar(data = Eid1, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.5,width=.1 )+
  geom_line(aes(x=month,y=mean_Temp/2),color="red",linewidth=1)+
  geom_point(aes(x=month,y=mean_Temp/2),color="red",size=3)+
  geom_errorbar(aes(x= month,ymin=temp_lci/2, ymax=temp_uci/2), color="red",alpha=.5,width=.1 )+
  scale_color_manual(values = c("mean temperature"="red","mean"="black"))+
  scale_y_continuous(name = "mean temperature",limits = c(0,15),breaks = seq(0,14,2), labels = seq(0,14,2)*2,position = "right",sec.axis = sec_axis(~ ., name = "mean ectoparasites"))+
  theme_bw()+ theme(legend.position = 'none', panel.grid = element_blank(),
                    legend.title = element_blank(),
                    strip.text = element_text(size=14),
                    axis.title.y=element_text(size=14),
                    axis.title.y.right = element_text(size=14,angle = -90),
                    axis.title.x = element_blank(),
                    strip.background = element_rect(fill="white"),
                    legend.text = element_text(size=12),
                    plot.margin = unit(c(.2,.1,1.3,1.1), "lines"),
                    axis.line.y.right = element_line(color = "red"),
                    axis.line.y.left = element_line(color = "black"),
                    axis.text = element_text(size=13))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov"));temp.kel



hday.kel<-ggplot(kel.clim)+
  ggtitle("Cyclopodia dubia")+
  geom_line(aes(x=month, y=mean),color="grey50",linewidth=1,data = Eid1)+
  geom_point(aes(x=month, y=mean),color="grey50",size=3,data = Eid1)+
  geom_errorbar(data = Eid1, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.5,width=.1 )+
  geom_line(aes(x=month,y=mean_Hday/6),color="red",linewidth=1)+
  geom_point(aes(x=month,y=mean_Hday/6),color="red",size=3)+
  geom_errorbar(aes(x= month,ymin=Hday_lci/6, ymax=Hday_uci/6), color="red",alpha=.5,width=.1 )+
  scale_y_continuous(limits = c(0,15),name = "mean ectoparasites",sec.axis = sec_axis(~ .*6,name = "mean humidity"))+
  theme_bw() + theme(legend.position = "none", 
                     panel.grid = element_blank(),
                     legend.title = element_blank(),
                     #axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     plot.title = element_text(face="italic"),
                     axis.title.y.right = element_text(size=14,angle = -90),
                     strip.text = element_text(size=14),
                     axis.title.y=element_text(size=14), 
                     strip.background = element_rect(fill="white"),
                     axis.line.y.right = element_line(color = "red"),
                     axis.line.y.left = element_line(color = "black"),
                     legend.text = element_blank(),
                     plot.margin = unit(c(.2,.1,1.3,1.1), "lines"),
                     axis.text = element_text(size=12))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov")); hday.kel


prec.kel<-ggplot(kel.clim)+
  geom_line(aes(x=month, y=mean),color="grey50",linewidth=1,data = Eid1)+
  geom_point(aes(x=month, y=mean),color="grey50",size=3,data = Eid1)+
  geom_errorbar(data = Eid1, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.5,width=.1 )+
  geom_line(aes(x=month,y=mean_prec*10),color="red",linewidth=1)+
  geom_point(aes(x=month,y=mean_prec*10),color="red",size=3)+
  geom_errorbar(aes(x= month,ymin=prec_lci*10, ymax=prec_uci*10), color="red",alpha=.5,width=.1 )+
  scale_y_continuous(limits = c(0,15),"mean ectoparasites", sec.axis = sec_axis(~ ./10, name="mean precipitation rate"))+
  theme_bw() + theme(legend.position = "none", 
                     legend.text = element_blank(),
                     panel.grid = element_blank(),
                     strip.text = element_text(size=14),
                     strip.background = element_rect(fill="white"),
                     axis.title.y=element_text(size=13),
                     axis.title.y.right = element_text(size=14,angle=-90),
                     axis.title.x = element_blank(),
                     axis.text.y = element_text(size=13),
                     axis.text.x = element_blank(),
                     # axis.ticks.x = element_blank(),
                     axis.line.y.right = element_line(color = "red"),
                     axis.line.y.left = element_line(color = "black"),
                     plot.margin = unit(c(.2,.1,1.3,1.1), "lines"))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov"));prec.kel



plot_Eid<-cowplot::plot_grid(hday.kel, prec.kel, temp.kel, ncol=1, nrow=3, align = "v", axis = "lr");plot_Eid



#Fig.miz_male <- cowplot::plot_grid(m, BM, rel_widths = c(1,1.1), nrow = 1, ncol = 2, labels = c("A", "B"), label_size = 22);Fig.miz_male
FIG3<-cowplot::plot_grid(plot_Eid, plot_Rou, ncol=2, nrow = 1,align = "h", axis="tb", labels = c("A", "B"), label_size = 22);FIG3


ggsave(file = paste0(homewd, "/final-figures/Fig3.png"),
       plot = FIG3,
       units="mm",  
       width=90, 
       height=80, 
       scale=3, 
       dpi=300)






