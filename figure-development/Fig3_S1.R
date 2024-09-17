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
                       body_length_cm, 
                       bat_forearm_mm, 
                       meglastreblidae,bat_flies,mass_forearm_residual)


data1$processing_date <- as.Date(data1$processing_date, format = "%m/%d/%y")

#check for NAs in dataset
nrow(data1[is.na(data1$collection_date),]) #0
nrow(data1[is.na(data1$bat_forearm_mm),]) #8
nrow(data1[is.na(data1$bat_weight_g),]) #16
nrow(data1[is.na(data1$bat_tibia_mm),]) #0
nrow(data1[is.na(data1$ear_length_mm),]) #0
nrow(data1[is.na(data1$meglastreblidae),]) #1018
nrow(data1[is.na(data1$bat_flies),]) #131
# Make numeric
data1$meglastreblidae<-as.numeric(data1$meglastreblidae)
data1$bat_flies<-as.numeric(data1$bat_flies)
data1$bat_weight_g<-as.numeric(data1$ bat_weight_g)
#change the date formaat
data1$dmy<-ymd(data1$processing_date)
data1$month<-month(data1$dmy)


####### ROUSETTUS MAROMIZAHA #############################################
Rou<-subset(data1, bat_species=="Rousettus madagascariensis"&roost_site=="Maromizaha_Rmad")
head(Rou)
unique(data1$roost_site)

# change the NA to 0 (zero)

Rou$bat_flies[is.na(Rou$bat_flies)]<-0 
unique(Rou$bat_sex)
Rou$bat_sex[Rou$bat_sex=="unknown"]<- "female"
Rou$bat_sex[Rou$bat_sex==""]<- NA
Rou$bat_sex[is.na(Rou$bat_sex)]<-"female" 

#### Mean and Error standard of the ectoparasites
names(Rou)

Rou1<-aggregate(Rou$bat_flies, list(Rou$month), FUN=mean)
Rou2<-aggregate(Rou$bat_flies, list(Rou$month), FUN=sd)

colnames(Rou1)<-c("month","mean");colnames(Rou2)<-c("month","sd")
Rou1<-merge(Rou1,Rou2,by="month")

Rou<-merge(Rou,Rou1,by="month")
length(Rou$month)

se_Rou<-c()

for (i in unique(Rou$month)) {
  if(i== i){
    se_Rou<-c(se_Rou,Rou$sd[Rou$month==i]/sqrt(length(Rou$month[Rou$month==i])))
  }
  
}
se_Rou

Rou$se<-se_Rou
## Calculation of the intervale of confidents
Rou$er_lci <- Rou$mean -1.96*Rou$se
Rou$er_uci <- Rou$mean +1.96*Rou$se


###DATA PREPARATION FOR EIDOLON AND IT'S ECTOPARASITES #######

# Make a subset of the data for Eidolon dupreanum  from AngavoKely
Eid<-subset(data1, bat_species=="Eidolon dupreanum"&roost_site=="Angavokely_Edup")
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
Eid1<-aggregate(Eid$bat_flies, list(Eid$month), FUN=mean) #mean
Eid2<-aggregate(Eid$bat_flies, list(Eid$month), FUN=sd) #standard deviation

colnames(Eid1)<-c("month","mean");colnames(Eid2)<-c("month","sd")
Eid1<-merge(Eid1,Eid2,by="month")
Eid<-merge(Eid,Eid1,by="month")
length(Eid$month)

# Calculate the Standard error 
se_Eid<-c()

for (i in unique(Eid$month)) {
  if(i== i){
    se_Eid<-c(se_Eid,Eid$sd[Eid$month==i]/sqrt(length(Eid$month[Eid$month==i])))
  }
  
}
se_Eid

Eid$se_Eid<-se_Eid

## Calculate the Confident Intervales
Eid$er_lci <- Eid$mean -1.96*Eid$se_Eid #lower CI
Eid$er_uci <- Eid$mean +1.96*Eid$se_Eid # Uper CI


#### IMPORTATION OF CLIMATIC DATA ###########################################


clim.dat<-read.csv(paste0(homewd,"/data/climate/Climate_tables/data_climate_giov.csv"))
head(clim.dat)

clim.dat$daty<-ymd(clim.dat$date)
clim.dat$volana<-month(clim.dat$daty)


## CLIMATIC DATA and CROSS CORRELATION FOR MAROMIZAHA ####

miz.clim<-subset(clim.dat,sites=="Maromizaha")
head(miz.clim)
#calculer l'humidite moyenne au cours des annees

miz.clim <- ddply(miz.clim, .(volana), summarise,
                  mean_Hday = mean(humidity_day), 
                  mean_Hnight=mean(Humidity.nigth), 
                  mean_Temp = mean(temperature),
                  mean_prec=mean(precipitation),
                  mean_NDVI = mean(NDVI))





#now merge with your case data
merge.dat.miz <- merge(Rou, miz.clim, by.x = "month",by.y = "volana")

attach(merge.dat.miz)
names(merge.dat.miz)
merge.dat.miz<-unique(data.frame(month,mean,sd,mean_Hday,mean_Hnight,mean_Temp,mean_NDVI,mean_prec))
detach(merge.dat.miz)

merge.melt.miz <- melt(merge.dat.miz, id.vars = c("month"))

head (merge.melt.miz)

# and plot these together - temp and cases
Ar.dat.miz1 = subset(merge.melt.miz, variable=="mean")
Ar.dat.miz1$variable <- "mean_Hday"
Ar.dat.miz2 = subset(merge.melt.miz, variable=="mean")
Ar.dat.miz2$variable <- "mean_Hnight"
Ar.dat.miz3 = subset(merge.melt.miz, variable=="mean")
Ar.dat.miz3$variable <- "mean_Temp"
Ar.dat.miz4 = subset(merge.melt.miz, variable=="mean")
Ar.dat.miz4$variable <- "mean_NDVI"
Ar.dat.miz5 = subset(merge.melt.miz, variable=="mean")
Ar.dat.miz5$variable <- "mean_prec"

Ar.dat.miz <- rbind(Ar.dat.miz1, Ar.dat.miz3,Ar.dat.miz5)
head(Ar.dat.miz)
unique(Ar.dat.miz$variable)



#save as data
lag.miz1 <- cbind.data.frame(lag = print(ccf(merge.dat.miz$mean_Hday, merge.dat.miz$mean))$lag, acf=print(ccf(merge.dat.miz$mean_Hday, merge.dat.miz$mean))$acf)
lag.miz1$variable <- "mean humidity"
#determine the optimal lag (here=0)
lag.miz1$lag[lag.miz1$acf==max(lag.miz1$acf)]
#plot
print(ccf(merge.dat.miz$mean_Hday, merge.dat.miz$mean))

# Lag_temperture
lag.miz2 = cbind.data.frame(lag = print(ccf(merge.dat.miz$mean_Temp, merge.dat.miz$mean))$lag, acf=print(ccf(merge.dat.miz$mean_Temp, merge.dat.miz$mean))$acf)
lag.miz2$variable <- "mean temperature"
lag.miz2$lag[lag.miz2$acf==max(lag.miz2$acf)]
# -1 is maximized cross correlation
# pred follow temp by 1
# 95% CI at 0.09


#and humidity
lag.miz3 = cbind.data.frame(lag = print(ccf(merge.dat.miz$mean_Hnight, merge.dat.miz$mean))$lag,
                            acf=print(ccf(merge.dat.miz$mean_Hnight, merge.dat.miz$mean))$acf)
lag.miz3$variable <- "mean_Hnight"
lag.miz3$lag[lag.miz3$acf==max(lag.miz3$acf)]
# 1 only
# cases follow mean H2M by 1 week
# 95% CI at 0.09
#and humidity
lag.miz4 = cbind.data.frame(lag = print(ccf(merge.dat.miz$mean_NDVI, merge.dat.miz$mean))$lag,
                            acf=print(ccf(merge.dat.miz$mean_NDVI, merge.dat.miz$mean))$acf)
lag.miz4$variable <- "mean_NDVI"
lag.miz4$lag[lag.miz4$acf==max(lag.miz4$acf)]
#and precipitation
lag.miz5 = cbind.data.frame(lag = print(ccf(merge.dat.miz$mean_prec, merge.dat.miz$mean))$lag,
                            acf=print(ccf(merge.dat.miz$mean_prec, merge.dat.miz$mean))$acf)
lag.miz5$variable <- "mean precipitation"
lag.miz5$lag[lag.miz5$acf==max(lag.miz5$acf)]
#save together
dat.lag.miz <- rbind(lag.miz1,lag.miz2, lag.miz5)

write.csv(dat.lag.miz, file=paste0(homewd, "/data/lag_output_rou.csv"), row.names = F)
andrana<-read.csv(paste0(homewd, "/data/lag_output_rou.csv"))
head(andrana)

#include the optimal lag on plot
max.lag <- dlply(dat.lag.miz, .(variable))
get.lag <- function(df){
  lag = df$lag[df$acf==max(df$acf)]
  df.out = cbind.data.frame(variable=unique(df$variable), lag=lag)
  return(df.out)
} 
max.lag <- data.table::rbindlist(lapply(max.lag, get.lag))
max.lag$label = paste0("lag=", max.lag$lag, " month")

dat.lag.miz$variable[dat.lag.miz$variable=="mean_prec"]<-"mean precipitation"
dat.lag.miz$variable[dat.lag.miz$variable=="mean_Hday"]<-"mean humidity"
dat.lag.miz$variable[dat.lag.miz$variable=="mean temperature"]<-"mean temperature"

unique(dat.lag.miz$variable)

BM.Miz<-ggplot(dat.lag.miz) + 
  geom_label(data=max.lag, aes(x=6,y=.4, label=label), label.size = 0) +
  geom_bar(aes(x=lag, y=acf), stat = "identity") + 
  geom_hline(aes(yintercept=0.09), color="blue", linetype=2) +
  geom_hline(aes(yintercept=-0.09), color="blue", linetype=2) +
  ggtitle("Eucampsipoda madagascariensis")+
  facet_grid(variable~.) + theme_bw() + theme(legend.position = c(.2,1), panel.grid = element_blank(),
                                              legend.title = element_blank(),
                                              axis.title = element_text(size=16),
                                              plot.title = element_text(face="italic"),
                                              strip.background = element_rect(fill="white"),
                                              strip.text = element_text(size=14,angle = -90),
                                              legend.text = element_text(size=12),
                                              plot.margin = unit(c(.2,.1,.1,1.1), "lines"),
                                              axis.text = element_text(size=14));BM.Miz




## CLIMATIC DATA and CROSS CORRELATION FOR ANGAVOKELY####

kel.clim<-subset(clim.dat,sites=="AngavoKely")

#calculate the mean of the Humidity for each month since 2013

kel.clim <- ddply(kel.clim, .(volana), summarise,
                  mean_Hday = mean(humidity_day), 
                  mean_Hnight=mean(Humidity.nigth), 
                  mean_Temp = mean(temperature),
                  mean_prec=mean(precipitation),
                  mean_NDVI = mean(NDVI))





#Now merge with our field data
#there is no month = 5 for Eid field data, so add as a dummy
dummy.eid <- Eid[1,]
dummy.eid$month <- 5
dummy.eid$processing_date <- NA
dummy.eid[,5:ncol(dummy.eid)] <- NA
merge.dat <- merge(Eid, kel.clim, by.x = "month",by.y = "volana", all.y = T)

attach(merge.dat)
names(merge.dat)
merge.dat<-unique(data.frame(month,mean,sd,mean_Hday,mean_Hnight,mean_Temp,mean_NDVI,mean_prec))
detach(merge.dat)

#Melt the new merged data
merge.melt <- melt(merge.dat, id.vars = c("month"))
head (merge.melt)

#Arrange and edit the new melted data
Ar.dat1 = subset(merge.melt, variable=="mean")
Ar.dat1$variable <- "mean_Hday"
Ar.dat2 = subset(merge.melt, variable=="mean")
Ar.dat2$variable <- "mean_Hnight"
Ar.dat3 = subset(merge.melt, variable=="mean")
Ar.dat3$variable <- "mean temperature"
Ar.dat4 = subset(merge.melt, variable=="mean")
Ar.dat4$variable <- "mean_NDVI"
Ar.dat5 = subset(merge.melt, variable=="mean")
Ar.dat5$variable <- "mean_prec"

Ar.dat <- rbind(Ar.dat1, Ar.dat3,Ar.dat5)
head(Ar.dat)
unique(Ar.dat$variable)

# check the lag between the mean abundance of ectoparasite and the climate variable and save as data

#Lag_humidity_day
lag1 <- cbind.data.frame(lag = print(ccf(merge.dat$mean_Hday, merge.dat$mean, na.action = na.pass))$lag, acf=print(ccf(merge.dat$mean_Hday, merge.dat$mean, na.action = na.pass))$acf)
lag1$variable <- "mean humidity"
#determine the optimal lag (here=0)
lag1$lag[lag1$acf==max(lag1$acf)]
#plot
print(ccf(merge.dat$mean_Hday, merge.dat$mean, na.action = na.pass))

# Lag_temperture
lag2 = cbind.data.frame(lag = print(ccf(merge.dat$mean_Temp, merge.dat$mean, na.action = na.pass))$lag, acf=print(ccf(merge.dat$mean_Temp, merge.dat$mean, na.action = na.pass))$acf)
lag2$variable <- "mean temperature"
lag2$lag[lag2$acf==max(lag2$acf)]
# -1 is maximized cross correlation
# pred follow temp by 1
# 95% CI at 0.09


#Humidity_Nigth
lag3 = cbind.data.frame(lag = print(ccf(merge.dat$mean_Hnight, merge.dat$mean, na.action = na.pass))$lag,
                        acf=print(ccf(merge.dat$mean_Hnight, merge.dat$mean, na.action = na.pass))$acf)
lag3$variable <- "mean_Hnight"
lag3$lag[lag3$acf==max(lag3$acf)]
# 1 only

#and Mean_NDVI
lag4 = cbind.data.frame(lag = print(ccf(merge.dat$mean_NDVI, merge.dat$mean, na.action = na.pass))$lag,
                        acf=print(ccf(merge.dat$mean_NDVI, merge.dat$mean, na.action = na.pass))$acf)
lag4$variable <- "mean_NDVI"
lag4$lag[lag4$acf==max(lag4$acf)]

#and Precipitation
lag5 = cbind.data.frame(lag = print(ccf(merge.dat$mean_prec, merge.dat$mean, na.action = na.pass))$lag,
                        acf=print(ccf(merge.dat$mean_prec, merge.dat$mean, na.action = na.pass))$acf)
lag5$variable <- "mean precipitation"
lag5$lag[lag5$acf==max(lag5$acf)]

#Put all of them together
dat.lag <- rbind(lag1,lag2, lag5) # here we only use the hDay, Temp, Precipitation

# Save the new data as CSV
write.csv(dat.lag, file=paste0(homewd, "/data/lag_output_eidolon.csv"), row.names = F)

# now lets import the new data 
andrana<-read.csv(paste0(homewd, "/data/lag_output_eidolon.csv"))
head(andrana)

#include the optimal lag on plot
max.lag <- dlply(dat.lag, .(variable))
get.lag <- function(df){
  lag = df$lag[df$acf==max(df$acf)]
  df.out = cbind.data.frame(variable=unique(df$variable), lag=lag)
  return(df.out)
} 

max.lag.Eid <- data.table::rbindlist(lapply(max.lag, get.lag))
max.lag.Eid$label = paste0("lag=", max.lag.Eid$lag, " month")

dat.lag$variable[dat.lag$variable=="mean_prec"]<-"mean precipitation"
dat.lag$variable[dat.lag$variable=="mean_Hday"]<-"mean humidity"
dat.lag$variable[dat.lag$variable=="mean temperature"]<-"mean temperature"

unique(dat.lag$variable)

#Plot the lagged data

BM.Ed<-ggplot(dat.lag) + 
  geom_label(data=max.lag.Eid, aes(x=6,y=.6, label=label), label.size = 0) +
  geom_bar(aes(x=lag, y=acf), stat = "identity") +
  ggtitle("Cyclopodia dubia")+
  geom_hline(aes(yintercept=0.09), color="blue", linetype=2) +
  geom_hline(aes(yintercept=-0.09), color="blue", linetype=2) +
  ylim(c(-.9,.9))+
  facet_grid(variable~.) + theme_bw() + theme(legend.position = c(.4,.9), panel.grid = element_blank(),
                                              legend.title = element_blank(),
                                              plot.title = element_text(face="italic"),
                                              axis.title.x = element_text(size=16),
                                              axis.title.y = element_blank(),
                                              strip.background = element_rect(fill="white"),
                                              strip.text = element_text(size=14,angle = -90),
                                              legend.text = element_text(size=12),
                                              plot.margin = unit(c(.2,.1,.1,1.1), "lines"),
                                              axis.text = element_text(size=14));BM.Ed


# FIGS1-Plot side by side the lag between the climate variable and the abundance of ectoparasites (CROSS CORRELATION)
ggdraw()+
  draw_plot(BM.Miz,x=0,y=0,width = .5,height = 1)+
  draw_plot(BM.Ed,x=0.5,y=0,width = .5,height = 1)



ggsave(file = paste0(homewd, "/final-figures/FigS1.png"),
       units="mm",  
       width=90, 
       height=80, 
       scale=3, 
       dpi=300)




### FIG2-Plot the cross correlation between the abundance of ectoparasite and the climate #

#plot all of Rousettus 
dat_temp.miz<-subset(merge.melt.miz,variable%in%c("mean_Temp","mean"))

temp.miz<-ggplot()+
  geom_line(aes(x=month,y=value,color=variable),color="grey50",linewidth=1,data = subset(dat_temp.miz,variable=="mean"))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_temp.miz,variable=="mean"))+
  geom_errorbar(data = Rou, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.3,width=.1 )+
  geom_line(aes(x=month,y=value/2,color=variable),color="red",linewidth=1,data = subset(dat_temp.miz,variable=="mean_Temp"))+
  geom_point(aes(x=month,y=value/2,color=variable),color="red",size=3,data = subset(dat_temp.miz,variable=="mean_Temp"))+
  scale_color_manual(values = c("mean temperature"="red","mean"="black"))+
  scale_y_continuous(name = "mean temperature",limits = c(0,14),breaks = seq(0,14,2), labels = seq(0,14,2)*2,position = "right",sec.axis = sec_axis(~ ., name = "mean ectoparasites"))+
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


dat_Hday.miz<-subset(merge.melt.miz,variable%in%c("mean_Hday","mean"))

hday.miz<-ggplot()+
  ggtitle("Eucampsipoda madagascariensis")+
  geom_line(aes(x=month,y=value,color=variable),color="grey50",linewidth=1,data = subset(dat_Hday.miz,variable=="mean"))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_Hday.miz,variable=="mean"))+
  geom_errorbar(data = Rou, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.3,width=.1 )+
  geom_line(aes(x=month,y=value/5,color=variable),color="red",linewidth=1,data = subset(dat_Hday.miz,variable=="mean_Hday"))+
  geom_point(aes(x=month,y=value/5,color=variable),color="red",size=3,data = subset(dat_Hday.miz,variable=="mean_Hday"))+
  #scale_color_manual(values = c("mean_Hday"="red","predicted_count"="grey50"))+
  scale_y_continuous(limits = c(0,14),name = "mean ectoparasites",sec.axis = sec_axis(~ .*5,name = "mean humidity"))+
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


dat_prec.miz<-subset(merge.melt.miz,variable%in%c("mean_prec","mean"))
dat_prec.miz$value[dat_prec.miz$variable=="mean_prec"]
min(dat_prec.miz$value[dat_prec.miz$variable=="mean_prec"])*10


prec.miz<-ggplot()+
  geom_line(aes(x=month,y=value,color=variable), color="grey50",linewidth=1,data = subset(dat_Hday.miz,variable=="mean"))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_Hday.miz,variable=="mean"))+
  geom_errorbar(data = Rou, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.3,width=.1 )+
  geom_point(aes(x=month,y=value*20,color=variable),color="red",size=3,data = subset(dat_prec.miz,variable=="mean_prec"))+
  geom_line(aes(x=month,y=value*20,color=variable),color="red",linewidth=1,data = subset(dat_prec.miz,variable=="mean_prec"))+
  scale_color_manual(values = c("mean_prec"="red","predicted_count"="blue"))+
  scale_y_continuous(limits = c(0,14),"mean ectoparasites", sec.axis = sec_axis(~ .*0.25, name="mean precipitation"))+
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

plot_Rou<-ggdraw()+
  draw_plot(hday.miz,x=0,y=.655,width = .5,height = .345)+
  draw_plot(prec.miz,x=0.002,y=.34,width = .49,height = .3425)+
  draw_plot(temp.miz,x=0,y=.0,width = .5,height = .368);plot_Rou

#Fig.miz_male <- cowplot::plot_grid(m, BM, rel_widths = c(1,1.1), nrow = 1, ncol = 2, labels = c("A", "B"), label_size = 22);Fig.miz_male

### Plot the Climate data for Eidolon

dat_temp<-subset(merge.melt,variable%in%c("mean_Temp","mean"))



temp<-ggplot()+
  geom_line(aes(x=month,y=value,color=variable),color="grey50",linewidth=1,data = subset(dat_temp,variable=="mean" & !is.na(value)))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_temp,variable=="mean"& !is.na(value)))+
  geom_errorbar(data = Eid, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.3,width=.1 )+
  geom_line(aes(x=month,y=value*.25,color=variable),color="red",linewidth=1,data = subset(dat_temp,variable=="mean_Temp"))+
  geom_point(aes(x=month,y=value*.25,color=variable),color="red",size=3,data = subset(dat_temp,variable=="mean_Temp"))+
  scale_color_manual(values = c("mean temperature"="red","mean"="black"))+
  scale_y_continuous(name = "mean temperature",position = "right",breaks = seq(0,5,1),labels = seq(0,5,1)*4,sec.axis = sec_axis(~ ., name = "mean ectoparasites"))+
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
                     labels = c("jan","mar","may","jul","sep","nov"));temp


dat_Hday<-subset(merge.melt,variable%in%c("mean_Hday","mean"))

hday<-ggplot()+
  ggtitle("Cyclopodia dubia")+
  geom_line(aes(x=month,y=value,color=variable),color="grey50",linewidth=1,data = subset(dat_Hday,variable=="mean"& !is.na(value)))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_Hday,variable=="mean"& !is.na(value)))+
  geom_errorbar(data = Eid, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.3,width=.1 )+
  #geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci*10 , ymax=predicted_count_uci *10), fill="black",alpha=.3 )+
  geom_line(aes(x=month,y=value/15,color=variable),color="red",linewidth=1,data = subset(dat_Hday,variable=="mean_Hday"))+
  geom_point(aes(x=month,y=value/15,color=variable),color="red",size=3,data = subset(dat_Hday,variable=="mean_Hday"))+
  #scale_color_manual(values = c("mean_Hday"="red","predicted_count"="grey50"))+
  scale_y_continuous(name = "mean ectoparasites",sec.axis = sec_axis(~ .*15,name = "mean humidity"))+
  theme_bw() + theme(legend.position = "none", 
                     panel.grid = element_blank(),
                     legend.title = element_blank(),
                     plot.title = element_text(face="italic"),
                     #axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
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
                     labels = c("jan","mar","may","jul","sep","nov")); hday


dat_prec<-subset(merge.melt,variable%in%c("mean_prec","mean"))
dat_prec$value[dat_prec$variable=="mean_prec"]
min(dat_prec$value[dat_prec$variable=="mean_prec"])*10




prec<-ggplot()+
  geom_line(aes(x=month,y=value,color=variable), color="grey50",linewidth=1,data = subset(dat_Hday,variable=="mean"& !is.na(value)))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_Hday,variable=="mean"& !is.na(value)))+
  geom_errorbar(data = Eid, aes(x= month, ymin=er_lci , ymax=er_uci ), color="grey50",alpha=.3,width=.1 )+
  #geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 )+
  geom_point(aes(x=month,y=value*10,color=variable),color="red",size=3,data = subset(dat_prec,variable=="mean_prec"))+
  geom_line(aes(x=month,y=value*10,color=variable),color="red",linewidth=1,data = subset(dat_prec,variable=="mean_prec"))+
  scale_color_manual(values = c("mean_prec"="red","predicted_count"="blue"))+
  scale_y_continuous("mean ectoparasites", sec.axis = sec_axis(~ .*0.25, name="mean precipitation"))+
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
                     labels = c("jan","mar","may","jul","sep","nov"));prec


plot_Eid<-ggdraw()+
  draw_plot(hday,x=0,y=.655,width = .508,height = .345)+
  draw_plot(prec,x=0.002,y=.34,width = .505,height = .3425)+
  draw_plot(temp,x=0,y=.0,width = .5,height = .368);plot_Eid

#Fig.miz_male <- cowplot::plot_grid(m, BM, rel_widths = c(1,1.1), nrow = 1, ncol = 2, labels = c("A", "B"), label_size = 22);Fig.miz_male
FIG3<-ggdraw()+
  draw_plot(plot_Eid,x=0,y=0,width = 1,height = 1)+
  draw_plot(plot_Rou,x=0.5,y=0,width = 1,height = 1);FIG3


ggsave(file = paste0(homewd, "/final-figures/Fig3.png"),
       plot = FIG3,
       units="mm",  
       width=90, 
       height=80, 
       scale=3, 
       dpi=300)





#make a new dataset with the lagged versions of these climate variables
# we can't start cases at timestep 1 because they are only predicted by all three variables starting
merge.dat # this is currently eidolon

#Rousettus first - precipitation should be lagged by 2 months, temperature by 1 month

#Precipitation first (at timestep 3, because lag is 2 months)
merge.shift <- merge.dat.miz
merge.shift$mean_precLag <- NA
for (i in 1:2){
  merge.shift$mean_precLag[merge.shift$month==i] <- merge.shift$mean_prec[merge.shift$month==12-i]   
}

for (i in 3:12){
#  print(i)
 # print(i-3)
  merge.shift$mean_precLag[merge.shift$month==i] <- merge.shift$mean_prec[merge.shift$month==i-2]   
}


#and lag the temp by one month
merge.shift$mean_tempLag <- NA
merge.shift$mean_tempLag[merge.shift$month==1] <- merge.shift$mean_Temp[merge.shift$month==12]   

for (i in 2:12){
  merge.shift$mean_tempLag[merge.shift$month==i] <- merge.shift$mean_Temp[merge.shift$month==i-1]   
}


#and merge with the old data

new_Rou<-merge(merge.shift,Rou,by="month")

#save new lagged data as a csv file
write.csv(new_Rou, file = paste0(homewd, "/data/Rousettus_lagged_data.csv"), row.names = F)

#and do Eidolon
#precip by 4 months, temp by 3
merge.shift <- merge.dat

merge.shift$mean_precLag <- NA
for (i in 1:4){
  merge.shift$mean_precLag[merge.shift$month==i] <- merge.shift$mean_prec[merge.shift$month==10-i]   
}

for (i in 5:12){
  #  print(i)
  # print(i-3)
  merge.shift$mean_precLag[merge.shift$month==i] <- merge.shift$mean_prec[merge.shift$month==i-4]   
}


#and lag the temp by three months
merge.shift$mean_tempLag <- NA
for (i in 1:3){
  merge.shift$mean_tempLag[merge.shift$month==i] <- merge.shift$mean_Temp[merge.shift$month==11-i]   
}


for (i in 4:12){
  merge.shift$mean_tempLag[merge.shift$month==i] <- merge.shift$mean_Temp[merge.shift$month==i-3]   
}


#and merge with the old data
new_ED<-merge(merge.shift,Eid,by="month")


head(new_ED)

nrow(new_ED[is.na(new_ED$mass_forearm_residual),]) #52
nrow(new_ED[is.na(new_ED$mean_Hday),])
nrow(new_ED[is.na(new_ED$mean_precLag),])

#save new lagged data
write.csv(new_ED, file = paste0(homewd, "/data/Eidolon_lagged_data.csv"), row.names = F)






