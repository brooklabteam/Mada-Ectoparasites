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
library(sour)
# I create my home work directory 
homewd="/Users/carabrook/Developer/Mada-Ectoparasites"

# create the working directory
setwd(paste0(homewd))

# call the Ectoparasite data from the working directory
dat<-read.csv(paste0(homewd,"/data/ecto_dat_long.csv"))
head(dat)
names(dat)
data1 <- dplyr::select(dat, sampleid, roost_site, processing_date,
                       bat_species, bat_sex, 
                       bat_age_class, bat_weight_g,
                       bat_forearm_mm, 
                       meglastreblidae,bat_flies,mass_forearm_residual)

data1 = subset(data1, !is.na(bat_age_class))
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
Rou$month <- as.character(Rou$month)
Rou$month[Rou$month=="1" | Rou$month=="2" | Rou$month=="3"| Rou$month=="4"| Rou$month=="5" | Rou$month=="6"| Rou$month=="7"| Rou$month=="8"| Rou$month=="9"] <- paste0("0", Rou$month[Rou$month=="1" | Rou$month=="2" | Rou$month=="3"| Rou$month=="4"| Rou$month=="5" | Rou$month=="6"| Rou$month=="7"| Rou$month=="8"| Rou$month=="9"])
Rou$month_year <- as.Date(paste0(Rou$year, "-", Rou$month, "-01"))
Rou$month <- as.numeric(Rou$month)

Rou1 <- ddply(Rou,.(bat_sex, month_year), summarise, mean = mean(bat_flies), sd=sd(bat_flies), N=length(sampleid))
Rou1$se <- Rou1$sd/sqrt(Rou1$N)

Rou2 <- ddply(Rou,.(month_year), summarise, mean = mean(bat_flies), sd=sd(bat_flies), N=length(sampleid))
Rou2$se <- Rou2$sd/sqrt(Rou2$N)

## Calculation of the intervale of confidents
Rou1$er_lci <- Rou1$mean -1.96*Rou1$se
Rou1$er_uci <- Rou1$mean +1.96*Rou1$se

Rou2$er_lci <- Rou2$mean -1.96*Rou2$se
Rou2$er_uci <- Rou2$mean +1.96*Rou2$se

Rou2$bat_sex <- "composite"
Rou2 <- dplyr::select(Rou2, names(Rou1))
Rou1 <- rbind(Rou1,Rou2)

###DATA PREPARATION FOR EIDOLON AND IT'S ECTOPARASITES #######

# Make a subset of the data for Eidolon dupreanum  from AngavoKely
Eid<-subset(data1, bat_species=="Eidolon dupreanum"&roost_site=="Angavokely_Edup" & bat_age_class!="J"|bat_species=="Eidolon dupreanum"&roost_site=="Angavobe_Edup" & bat_age_class!="J")
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
Eid$month <- as.character(Eid$month)
Eid$month[Eid$month=="1" | Eid$month=="2" | Eid$month=="3"| Eid$month=="4"| Eid$month=="5" | Eid$month=="6"| Eid$month=="7"| Eid$month=="8"| Eid$month=="9"] <- paste0("0", Eid$month[Eid$month=="1" | Eid$month=="2" | Eid$month=="3"| Eid$month=="4"| Eid$month=="5" | Eid$month=="6"| Eid$month=="7"| Eid$month=="8"| Eid$month=="9"])
Eid$month_year <- as.Date(paste0(Eid$year, "-", Eid$month, "-01"))
Eid$month <- as.numeric(Eid$month)



Eid1 <- ddply(Eid,.(bat_sex, month_year), summarise, mean = mean(bat_flies), sd=sd(bat_flies), N=length(sampleid))
Eid1$se <- Eid1$sd/sqrt(Eid1$N)

Eid2 <- ddply(Eid,.(month_year), summarise, mean = mean(bat_flies), sd=sd(bat_flies), N=length(sampleid))
Eid2$se <- Eid2$sd/sqrt(Eid2$N)

## Calculation of the intervale of confidents
Eid1$er_lci <- Eid1$mean -1.96*Eid1$se
Eid1$er_uci <- Eid1$mean +1.96*Eid1$se

Eid2$er_lci <- Eid2$mean -1.96*Eid2$se
Eid2$er_uci <- Eid2$mean +1.96*Eid2$se

Eid2$bat_sex <- "composite"
Eid2 <- dplyr::select(Eid2, names(Eid1))
Eid1 <- rbind(Eid1,Eid2)




head(Eid1)




#### IMPORTATION OF CLIMATIC DATA ###########################################


clim.dat<-read.csv(paste0(homewd,"/data/climate/Climate_tables/data_climate_giov.csv"))
head(clim.dat)

#add month_year column
clim.dat$month <- month(clim.dat$date)
clim.dat$year <- year(clim.dat$date)
clim.dat$month <- as.character(clim.dat$month)
clim.dat$month[clim.dat$month=="1" | clim.dat$month=="2" | clim.dat$month=="3"| clim.dat$month=="4"| clim.dat$month=="5" | clim.dat$month=="6"| clim.dat$month=="7"| clim.dat$month=="8"| clim.dat$month=="9"] <- paste0("0", clim.dat$month[clim.dat$month=="1" | clim.dat$month=="2" | clim.dat$month=="3"| clim.dat$month=="4"| clim.dat$month=="5" | clim.dat$month=="6"| clim.dat$month=="7"| clim.dat$month=="8"| clim.dat$month=="9"])
clim.dat$month_year <- as.Date(paste0(clim.dat$year, "-", clim.dat$month, "-01"))
clim.dat$month <- as.numeric(clim.dat$month)


clim.dat$daty<-ymd(clim.dat$date)
clim.dat$volana<-month(clim.dat$daty)


## CLIMATIC DATA and CROSS CORRELATION FOR MAROMIZAHA ####

miz.clim<-subset(clim.dat,sites=="Maromizaha")
head(miz.clim)
#calculer l'humidite moyenne au cours des annees

miz.clim <- ddply(miz.clim, .(month_year), summarise,
                  mean_Hday = mean(humidity_day), 
                  mean_Hnight=mean(Humidity.nigth), 
                  mean_Temp = mean(temperature),
                  mean_prec=mean(precipitation),
                  mean_NDVI = mean(NDVI))


#now make miz time series for all the climate variables

#females

#now look for cross correlation between mean ecto count per bat per month-year
#and mean climate variables per month-year
miz.hum <- dplyr::select(miz.clim, month_year, mean_Hday)
names(miz.hum) <- c("t", "y")
miz.hum$t <- year(miz.hum$t) + month(miz.hum$t)/12
Rou.test <- dplyr::select(subset(Rou1, bat_sex=="female"), month_year, mean)
Rou.test <- Rou.test [!duplicated(Rou.test),]
names(Rou.test) <- c("t", "y")
Rou.test$t <- year(Rou.test$t) + month(Rou.test$t)/12
out.miz.hum <- cross_correlate(miz.hum, Rou.test, dtau = (1/12), max.lag = 1)


#and repeat for temp 
miz.temp <- dplyr::select(miz.clim, month_year, mean_Temp)
names(miz.temp) <- c("t", "y")
miz.temp$t <- year(miz.temp$t) + month(miz.temp$t)/12
out.miz.temp <- cross_correlate(miz.temp, Rou.test, dtau = (1/12), max.lag = 1)



#and precip
miz.prec <- dplyr::select(miz.clim, month_year, mean_prec)
names(miz.prec) <- c("t", "y")
miz.prec$t <- year(miz.prec$t) + month(miz.prec$t)/12
out.miz.prec <- cross_correlate(miz.prec, Rou.test, dtau = (1/12), max.lag = 1)


#compile and save lag data

lag.miz1 <- cbind.data.frame(lag = out.miz.hum$tau[1:13]*12, 
                             ccf= out.miz.hum$ccf[1:13])
lag.miz1$variable <- "mean humidity"

#determine the optimal lag (here=0)
lag.miz1$lag[lag.miz1$ccf==max(lag.miz1$ccf)] #0

#plot

# Lag_temperature
lag.miz2 <- cbind.data.frame(lag = out.miz.temp$tau[1:13]*12, 
                             ccf= out.miz.temp$ccf[1:13])
lag.miz2$variable <- "mean temperature"
lag.miz2$lag[lag.miz2$ccf==max(lag.miz2$ccf)] #-1

# -1 is maximized cross correlation
# pred follow temp by 1



#and humidity
lag.miz3 <- cbind.data.frame(lag = out.miz.prec$tau[1:13]*12, 
                             ccf= out.miz.prec$ccf[1:13])
lag.miz3$variable <- "mean precipitation"
lag.miz3$lag[lag.miz3$ccf==max(lag.miz3$ccf)] #-1

# -1 is maximized cross correlation
# pred follow temp by 1


#save together
dat.lag.miz <- rbind(lag.miz1,lag.miz2, lag.miz3)

dat.lag.miz$bat_sex = "female"



#and male

#now look for cross correlation between mean ecto count per bat per month-year
#and mean climate variables per month-year
miz.hum <- dplyr::select(miz.clim, month_year, mean_Hday)
names(miz.hum) <- c("t", "y")
miz.hum$t <- year(miz.hum$t) + month(miz.hum$t)/12
Rou.test <- dplyr::select(subset(Rou1, bat_sex=="male"), month_year, mean)
Rou.test <- Rou.test [!duplicated(Rou.test),]
names(Rou.test) <- c("t", "y")
Rou.test$t <- year(Rou.test$t) + month(Rou.test$t)/12
out.miz.hum <- cross_correlate(miz.hum, Rou.test, dtau = (1/12), max.lag = 1)


#and repeat for temp 
miz.temp <- dplyr::select(miz.clim, month_year, mean_Temp)
names(miz.temp) <- c("t", "y")
miz.temp$t <- year(miz.temp$t) + month(miz.temp$t)/12
out.miz.temp <- cross_correlate(miz.temp, Rou.test, dtau = (1/12), max.lag = 1)



#and precip
miz.prec <- dplyr::select(miz.clim, month_year, mean_prec)
names(miz.prec) <- c("t", "y")
miz.prec$t <- year(miz.prec$t) + month(miz.prec$t)/12
out.miz.prec <- cross_correlate(miz.prec, Rou.test, dtau = (1/12), max.lag = 1)


#compile and save lag data

lag.miz1 <- cbind.data.frame(lag = out.miz.hum$tau[1:13]*12, 
                             ccf= out.miz.hum$ccf[1:13])
lag.miz1$variable <- "mean humidity"

#determine the optimal lag (here=0)
lag.miz1$lag[lag.miz1$ccf==max(lag.miz1$ccf)] #0

#plot

# Lag_temperature
lag.miz2 <- cbind.data.frame(lag = out.miz.temp$tau[1:13]*12, 
                             ccf= out.miz.temp$ccf[1:13])
lag.miz2$variable <- "mean temperature"
lag.miz2$lag[lag.miz2$ccf==max(lag.miz2$ccf)] #-1

# -1 is maximized cross correlation
# pred follow temp by 1



#and humidity
lag.miz3 <- cbind.data.frame(lag = out.miz.prec$tau[1:13]*12, 
                             ccf= out.miz.prec$ccf[1:13])
lag.miz3$variable <- "mean precipitation"
lag.miz3$lag[lag.miz3$ccf==max(lag.miz3$ccf)] #-1

# -1 is maximized cross correlation
# pred follow temp by 1


#save together
dat.lag.miz.male <- rbind(lag.miz1,lag.miz2, lag.miz3)
dat.lag.miz.male$bat_sex="male"



#and composite
#now look for cross correlation between mean ecto count per bat per month-year
#and mean climate variables per month-year
miz.hum <- dplyr::select(miz.clim, month_year, mean_Hday)
names(miz.hum) <- c("t", "y")
miz.hum$t <- year(miz.hum$t) + month(miz.hum$t)/12
Rou.test <- dplyr::select(subset(Rou1, bat_sex=="composite"), month_year, mean)
Rou.test <- Rou.test [!duplicated(Rou.test),]
names(Rou.test) <- c("t", "y")
Rou.test$t <- year(Rou.test$t) + month(Rou.test$t)/12
out.miz.hum <- cross_correlate(miz.hum, Rou.test, dtau = (1/12), max.lag = 1)


#and repeat for temp 
miz.temp <- dplyr::select(miz.clim, month_year, mean_Temp)
names(miz.temp) <- c("t", "y")
miz.temp$t <- year(miz.temp$t) + month(miz.temp$t)/12
out.miz.temp <- cross_correlate(miz.temp, Rou.test, dtau = (1/12), max.lag = 1)



#and precip
miz.prec <- dplyr::select(miz.clim, month_year, mean_prec)
names(miz.prec) <- c("t", "y")
miz.prec$t <- year(miz.prec$t) + month(miz.prec$t)/12
out.miz.prec <- cross_correlate(miz.prec, Rou.test, dtau = (1/12), max.lag = 1)


#compile and save lag data

lag.miz1 <- cbind.data.frame(lag = out.miz.hum$tau[1:13]*12, 
                             ccf= out.miz.hum$ccf[1:13])
lag.miz1$variable <- "mean humidity"

#determine the optimal lag (here=0)
lag.miz1$lag[lag.miz1$ccf==max(lag.miz1$ccf)] #0

#plot

# Lag_temperature
lag.miz2 <- cbind.data.frame(lag = out.miz.temp$tau[1:13]*12, 
                             ccf= out.miz.temp$ccf[1:13])
lag.miz2$variable <- "mean temperature"
lag.miz2$lag[lag.miz2$ccf==max(lag.miz2$ccf)] #-1

# -1 is maximized cross correlation
# pred follow temp by 1



#and humidity
lag.miz3 <- cbind.data.frame(lag = out.miz.prec$tau[1:13]*12, 
                             ccf= out.miz.prec$ccf[1:13])
lag.miz3$variable <- "mean precipitation"
lag.miz3$lag[lag.miz3$ccf==max(lag.miz3$ccf)] #-1

# -1 is maximized cross correlation
# pred follow temp by 1


#save together
dat.lag.miz.comp <- rbind(lag.miz1,lag.miz2, lag.miz3)
dat.lag.miz.comp$bat_sex="composite"

dat.lag.miz <- rbind(dat.lag.miz, dat.lag.miz.male, dat.lag.miz.comp)


write.csv(dat.lag.miz, file=paste0(homewd, "/data/lag_output_rou.csv"), row.names = F)
andrana<-read.csv(paste0(homewd, "/data/lag_output_rou.csv"))
head(andrana)

#include the optimal lag on plot
max.lag <- dlply(subset(dat.lag.miz, bat_sex=="composite"), .(variable))
get.lag <- function(df){
  lag = df$lag[df$ccf==max(df$ccf)]
  df.out = cbind.data.frame(variable=unique(df$variable), lag=lag)
  return(df.out)
} 
max.lag <- data.table::rbindlist(lapply(max.lag, get.lag))
max.lag$label = paste0("lag=", max.lag$lag, " month")

dat.lag.miz$variable[dat.lag.miz$variable=="mean_prec"]<-"mean precipitation"
dat.lag.miz$variable[dat.lag.miz$variable=="mean_Hday"]<-"mean humidity"
dat.lag.miz$variable[dat.lag.miz$variable=="mean temperature"]<-"mean temperature"

unique(dat.lag.miz$variable)

dat.lag.miz$facet = "male / female"
max.lag$facet = "male / female"

BM.Miz<-ggplot(subset(dat.lag.miz, bat_sex=="composite")) + 
  geom_label(data=max.lag, aes(x=-9.5,y=.47, label=label), label.size = 0) +
  geom_bar(aes(x=lag, y=ccf), stat = "identity") + 
  geom_hline(aes(yintercept=0.09), color="blue", linetype=2) +
  geom_hline(aes(yintercept=-0.09), color="blue", linetype=2) +
  ggtitle("Eucampsipoda madagascariensis")+ xlab("lag (months)") + ylab("ccf") +
  facet_grid(variable~facet) + theme_bw() + theme(legend.position = c(.2,1), panel.grid = element_blank(),
                                              legend.title = element_blank(),
                                              axis.title = element_text(size=16),
                                              plot.title = element_text(face="italic"),
                                              strip.background = element_rect(fill="white"),
                                              strip.text.y = element_text(size=14,angle = -90),
                                              strip.text.x = element_text(size=14),
                                              legend.text = element_text(size=12),
                                              plot.margin = unit(c(.2,.1,.1,1.1), "lines"),
                                              axis.text = element_text(size=14));BM.Miz




## CLIMATIC DATA and CROSS CORRELATION FOR ANGAVOKELY####

kel.clim<-subset(clim.dat,sites=="AngavoKely")

#calculate the mean of the Humidity for each month since 2013

kel.clim <- ddply(kel.clim, .(month_year), summarise,
                  mean_Hday = mean(humidity_day), 
                  mean_Hnight=mean(Humidity.nigth), 
                  mean_Temp = mean(temperature),
                  mean_prec=mean(precipitation),
                  mean_NDVI = mean(NDVI))


#now make kel time series for all the climate variables
#now look for cross correlation

#female
kel.hum <- dplyr::select(kel.clim, month_year, mean_Hday)
names(kel.hum) <- c("t", "y")
kel.hum$t <- year(kel.hum$t) + month(kel.hum$t)/12
Eid.test <- dplyr::select(subset(Eid1, bat_sex=="female"), month_year, mean)
Eid.test <- Eid.test [!duplicated(Eid.test),]
names(Eid.test) <- c("t", "y")
Eid.test$t <- year(Eid.test$t) + month(Eid.test$t)/12
out.kel.hum <- cross_correlate(kel.hum, Eid.test, dtau = (1/12), max.lag = 1)


#and repeat for temp 
kel.temp <- dplyr::select(kel.clim, month_year, mean_Temp)
names(kel.temp) <- c("t", "y")
kel.temp$t <- year(kel.temp$t) + month(kel.temp$t)/12
out.kel.temp <- cross_correlate(kel.temp, Eid.test, dtau = (1/12), max.lag = 1)



#and precip
kel.prec <- dplyr::select(kel.clim, month_year, mean_prec)
names(kel.prec) <- c("t", "y")
kel.prec$t <- year(kel.prec$t) + month(kel.prec$t)/12
out.kel.prec <- cross_correlate(kel.prec, Eid.test, dtau = (1/12), max.lag = 1)


#compile and save lag data

lag.kel1 <- cbind.data.frame(lag = out.kel.hum$tau[1:13]*12, 
                             ccf= out.kel.hum$ccf[1:13])
lag.kel1$variable <- "mean humidity"

#determine the optimal lag (here=0)
lag.kel1$lag[lag.kel1$ccf==max(lag.kel1$ccf)] #-5

#plot

# Lag_temperature
lag.kel2 <- cbind.data.frame(lag = out.kel.temp$tau[1:13]*12, 
                             ccf= out.kel.temp$ccf[1:13])
lag.kel2$variable <- "mean temperature"
lag.kel2$lag[lag.kel2$ccf==max(lag.kel2$ccf)] #-6



#and precip
lag.kel3 <- cbind.data.frame(lag = out.kel.prec$tau[1:13]*12, 
                             ccf= out.kel.prec$ccf[1:13])
lag.kel3$variable <- "mean precipitation"
lag.kel3$lag[lag.kel3$ccf==max(lag.kel3$ccf)] #-5


#save together
dat.lag.kel <- rbind(lag.kel1,lag.kel2, lag.kel3)
dat.lag.kel$bat_sex="female"

#male
kel.hum <- dplyr::select(kel.clim, month_year, mean_Hday)
names(kel.hum) <- c("t", "y")
kel.hum$t <- year(kel.hum$t) + month(kel.hum$t)/12
Eid.test <- dplyr::select(subset(Eid1, bat_sex=="male"), month_year, mean)
Eid.test <- Eid.test [!duplicated(Eid.test),]
names(Eid.test) <- c("t", "y")
Eid.test$t <- year(Eid.test$t) + month(Eid.test$t)/12
out.kel.hum <- cross_correlate(kel.hum, Eid.test, dtau = (1/12), max.lag = 1)


#and repeat for temp 
kel.temp <- dplyr::select(kel.clim, month_year, mean_Temp)
names(kel.temp) <- c("t", "y")
kel.temp$t <- year(kel.temp$t) + month(kel.temp$t)/12
out.kel.temp <- cross_correlate(kel.temp, Eid.test, dtau = (1/12), max.lag = 1)



#and precip
kel.prec <- dplyr::select(kel.clim, month_year, mean_prec)
names(kel.prec) <- c("t", "y")
kel.prec$t <- year(kel.prec$t) + month(kel.prec$t)/12
out.kel.prec <- cross_correlate(kel.prec, Eid.test, dtau = (1/12), max.lag = 1)


#compile and save lag data

lag.kel1 <- cbind.data.frame(lag = out.kel.hum$tau[1:13]*12, 
                             ccf= out.kel.hum$ccf[1:13])
lag.kel1$variable <- "mean humidity"

#determine the optimal lag (here=0)
lag.kel1$lag[lag.kel1$ccf==max(lag.kel1$ccf)] #0

#plot

# Lag_temperature
lag.kel2 <- cbind.data.frame(lag = out.kel.temp$tau[1:13]*12, 
                             ccf= out.kel.temp$ccf[1:13])
lag.kel2$variable <- "mean temperature"
lag.kel2$lag[lag.kel2$ccf==max(lag.kel2$ccf)] #-3


#and humidity
lag.kel3 <- cbind.data.frame(lag = out.kel.prec$tau[1:13]*12, 
                             ccf= out.kel.prec$ccf[1:13])
lag.kel3$variable <- "mean precipitation"
lag.kel3$lag[lag.kel3$ccf==max(lag.kel3$ccf)] #-4


#save together
dat.lag.kel.male <- rbind(lag.kel1,lag.kel2, lag.kel3)
dat.lag.kel.male$bat_sex="male"


#composite
kel.hum <- dplyr::select(kel.clim, month_year, mean_Hday)
names(kel.hum) <- c("t", "y")
kel.hum$t <- year(kel.hum$t) + month(kel.hum$t)/12
Eid.test <- dplyr::select(subset(Eid1, bat_sex=="composite"), month_year, mean)
Eid.test <- Eid.test [!duplicated(Eid.test),]
names(Eid.test) <- c("t", "y")
Eid.test$t <- year(Eid.test$t) + month(Eid.test$t)/12
out.kel.hum <- cross_correlate(kel.hum, Eid.test, dtau = (1/12), max.lag = 1)


#and repeat for temp 
kel.temp <- dplyr::select(kel.clim, month_year, mean_Temp)
names(kel.temp) <- c("t", "y")
kel.temp$t <- year(kel.temp$t) + month(kel.temp$t)/12
out.kel.temp <- cross_correlate(kel.temp, Eid.test, dtau = (1/12), max.lag = 1)



#and precip
kel.prec <- dplyr::select(kel.clim, month_year, mean_prec)
names(kel.prec) <- c("t", "y")
kel.prec$t <- year(kel.prec$t) + month(kel.prec$t)/12
out.kel.prec <- cross_correlate(kel.prec, Eid.test, dtau = (1/12), max.lag = 1)


#compile and save lag data

lag.kel1 <- cbind.data.frame(lag = out.kel.hum$tau[1:13]*12, 
                             ccf= out.kel.hum$ccf[1:13])
lag.kel1$variable <- "mean humidity"

#determine the optimal lag (here=0)
lag.kel1$lag[lag.kel1$ccf==max(lag.kel1$ccf)] #0

#plot

# Lag_temperature
lag.kel2 <- cbind.data.frame(lag = out.kel.temp$tau[1:13]*12, 
                             ccf= out.kel.temp$ccf[1:13])
lag.kel2$variable <- "mean temperature"
lag.kel2$lag[lag.kel2$ccf==max(lag.kel2$ccf)] #-3




#and humidity
lag.kel3 <- cbind.data.frame(lag = out.kel.prec$tau[1:13]*12, 
                             ccf= out.kel.prec$ccf[1:13])
lag.kel3$variable <- "mean precipitation"
lag.kel3$lag[lag.kel3$ccf==max(lag.kel3$ccf)] #-4


#save together
dat.lag.kel.comp <- rbind(lag.kel1,lag.kel2, lag.kel3)
dat.lag.kel.comp$bat_sex="composite"

dat.lag.kel <- rbind(dat.lag.kel, dat.lag.kel.male, dat.lag.kel.comp)

write.csv(dat.lag.kel, file=paste0(homewd, "/data/lag_output_eidolon.csv"), row.names = F)


# now lets import the new data 
andrana<-read.csv(paste0(homewd, "/data/lag_output_eidolon.csv"))
head(andrana)

#include the optimal lag on plot
max.lag.M <- dlply(subset(dat.lag.kel, bat_sex=="male"), .(variable))
max.lag.F <- dlply(subset(dat.lag.kel, bat_sex=="female"), .(variable))
get.lag <- function(df){
  lag = df$lag[df$ccf==max(df$ccf)]
  df.out = cbind.data.frame(variable=unique(df$variable), lag=lag)
  return(df.out)
} 

max.lag.Eid.M <- data.table::rbindlist(lapply(max.lag.M, get.lag))
max.lag.Eid.F <- data.table::rbindlist(lapply(max.lag.F, get.lag))
max.lag.Eid.M$label = paste0("lag=", max.lag.Eid.M$lag, " month")
max.lag.Eid.F$label = paste0("lag=", max.lag.Eid.F$lag, " month")
max.lag.Eid.F$bat_sex = "female"
max.lag.Eid.M$bat_sex = "male"
max.lag.Eid <- rbind(max.lag.Eid.M, max.lag.Eid.F)

dat.lag.kel$variable[dat.lag.kel$variable=="mean_prec"]<-"mean precipitation"
dat.lag.kel$variable[dat.lag.kel$variable=="mean_Hday"]<-"mean humidity"
dat.lag.kel$variable[dat.lag.kel$variable=="mean temperature"]<-"mean temperature"

unique(dat.lag.kel$variable)

#Plot the lagged data
dat.lag.kel$bat_sex <- factor(dat.lag.kel$bat_sex, levels=c("composite", "male", "female"))
max.lag.Eid$bat_sex <- factor(max.lag.Eid$bat_sex, levels=c("male", "female"))

BM.Ed<-ggplot(subset(dat.lag.kel, bat_sex!="composite")) + 
  geom_label(data=max.lag.Eid, aes(x=-9.5,y=.47, label=label), label.size = 0) + theme_bw() +
  geom_bar(aes(x=lag, y=ccf), stat = "identity") +
  ggtitle("Cyclopodia dubia")+
  geom_hline(aes(yintercept=0.09), color="blue", linetype=2) +
  geom_hline(aes(yintercept=-0.09), color="blue", linetype=2) +
  facet_grid(variable~bat_sex)  + xlab("lag (months)") + ylab("ccf") +
  scale_y_continuous(limits = c(-.5,.5), breaks=c(-.5,-.25,0,.25,.5), labels = c("-0.50", "-0.25", "0.00","0.25", "0.50")) +
    theme(legend.position = c(.4,.9), panel.grid = element_blank(),
                                              legend.title = element_blank(),
                                              plot.title = element_text(face="italic"),
                                              axis.title = element_text(size=16),
                                              strip.background = element_rect(fill="white"),
                                              strip.text.y = element_text(size=14,angle = -90),
                                              strip.text.x = element_text(size=14),
                                              legend.text = element_text(size=12),
                                              plot.margin = unit(c(.2,.1,.1,1.1), "lines"),
                                              axis.text = element_text(size=14));BM.Ed


# FIGS2-Plot side by side the lag between the climate variable and the abundance of ectoparasites (CROSS CORRELATION)

FigS3 <- cowplot::plot_grid(BM.Ed, BM.Miz, ncol = 2, nrow = 1, rel_widths = c(1.75,1), labels = c("A", "B"), label_size = 22)



ggsave(file = paste0(homewd, "/final-figures/FigS3.png"),
       plot = FigS3,
       units="mm",  
       width=110, 
       height=80, 
       scale=3, 
       dpi=300)


#and now shift the data by the appropriate lag
head(kel.clim)
head(miz.clim)


#miz data will shift by one month for temp and precip and stay the same for humidity
#miz is composite the same for all
miz.clim.shift <- dplyr::select(miz.clim, month_year, mean_Hday)
miz.clim.shift$lag_temp <- c(NA,miz.clim$mean_Temp[1:length(miz.clim$mean_Temp)-1])
miz.clim.shift$lag_precip <- c(NA,miz.clim$mean_prec[1:length(miz.clim$mean_prec)-1])

#and the eidolon data will shift by 2 months for humidity and 4 months for precip and 3 months for temp for males
#females should be 5 months for humidity and prceip and 6 months for temp
kel.clim.shift.male <- dplyr::select(kel.clim, month_year, mean_Hday)
kel.clim.shift.male$lag_temp <- c(NA,NA,NA, kel.clim$mean_Temp[1:(length(kel.clim$month_year)-3)])
kel.clim.shift.male$lag_precip <- c(NA,NA, NA,NA, kel.clim$mean_prec[1:(length(kel.clim$month_year)-4)])


kel.clim.shift.female <- dplyr::select(kel.clim, month_year)
kel.clim.shift.female$mean_Hday <- c(NA,NA,NA,NA,NA, kel.clim$mean_Hday[1:(length(kel.clim$month_year)-5)])
kel.clim.shift.female$lag_temp <- c(NA,NA,NA,NA,NA,NA, kel.clim$mean_Temp[1:(length(kel.clim$month_year)-6)])
kel.clim.shift.female$lag_precip <- c(NA,NA, NA,NA,NA, kel.clim$mean_prec[1:(length(kel.clim$month_year)-5)])

kel.clim.shift.male$bat_sex = "male"
kel.clim.shift.female$bat_sex = "female"
kel.clim.shift <- rbind(kel.clim.shift.male,kel.clim.shift.female)
#now, merge with the infection data for the two sites:
# Rou first

Rou.merge <- dplyr::select(Rou, 1:11, "month_year")
Rou.merge <- merge(Rou.merge, miz.clim.shift, by="month_year", all.x = T)
head(Rou.merge)


#save new lagged data as a csv file
write.csv(Rou.merge, file = paste0(homewd, "/data/Rousettus_lagged_data.csv"), row.names = F)

#and do Eidolon
head(Eid)


Eid.merge <- dplyr::select(Eid, 1:11, "month_year")
Eid.merge <- merge(Eid.merge, kel.clim.shift, by=c("bat_sex", "month_year"), all.x = T)
head(Eid.merge)



#save new lagged data
write.csv(Eid.merge, file = paste0(homewd, "/data/Eidolon_lagged_data.csv"), row.names = F)






