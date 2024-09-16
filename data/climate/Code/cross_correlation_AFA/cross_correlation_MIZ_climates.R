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
# I creat my home work directory 
homewd="D:/EKIPA_FANIHY/Ekipa fanihy praper project/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"

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

#Rou<-subset(Rou,bat_sex=="male")

gamRou <- gam(bat_flies~ s(month, k=7, bs = "cc"), data = Rou)

plot(gamRou)

Rou$predicted_count <- predict.gam(gamRou,type="response")

#and to add in the confidence intervals
Rou$predicted_count_SE <- predict.gam(gamRou,type="response", se.fit = T)$se.fit
Rou$predicted_count_lci <- Rou$predicted_count -1.96*Rou$predicted_count_SE
Rou$predicted_count_uci <- Rou$predicted_count +1.96*Rou$predicted_count_SE
Rou$predicted_count_lci[Rou$predicted_count_lci<0] <- 0#now add the predictions to each dataframe
#get epiweek for the climate data
Rou$mensuelle <- as.Date(cut.Date(Rou$dmy, breaks="month"))





ggplot(data = Rou) + 
  geom_point(aes(x=month, y= bat_flies), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Month of the year")+ 
  ylab("Count of ectoparasite")+
  ggtitle("E.madagascariensis (Maromizaha)")+
  geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = Rou, aes(x=month, y=predicted_count),color="red", linewidth=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_text(),
        axis.title.y = element_text())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"))


#### CALCULATION MEAN OF THE ECTOPARASITES
names(Rou)
View(Rou1)

library(dplyr)

Rou1<-aggregate(Rou$bat_flies, list(Rou$month), FUN=mean)


colnames(Rou1)<-c("month","mean")

afx<-merge(Rou,Rou1,by="month")
View(afx)

afx<-Rou%>%
  group_by(month)%>%
  mutate(mean_BF=mean(bat_flies),sd_BF=sd(bat_flies))


mean(Rou$bat_flies)

Rou1x <- group_by(Rou, month) # group data by party *and* congress
summarize(Rou1x,
          mean_bf = mean(bat_flies), # calculate the mean for each group
          sd_bf = sd(bat_flies)) # calculate the standard deviation for each group

Rou<-Rou%>%
  group_by(month)%>%mutate(SE=sd_BF/sqrt(sum(bat_flies)),lci=mean_BF-1.96*SE,uci=mean_BF+1.96*SE)



a<-ggplot(data = Rou) + 
  geom_point(aes(x=month, y= mean_BF), alpha=.3, show.legend = F);a

a
View(Rou)


Rou_M<-Rou[16:20]
#### CLIMATIC DATA ###########################################
## HUMIDITY DAY NIGHT ####


clim.dat<-read.csv("D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/Climate-data/Climate_tables/data_climate_giov.csv")
View(clim.dat)

clim.dat$daty<-ymd(clim.dat$date)
clim.dat$volana<-month(clim.dat$daty)

miz.clim<-subset(clim.dat,sites=="Maromizaha")
View(miz.clim)
#calculer l'humidite moyenne au cours des annees

miz.clim <- ddply(miz.clim, .(volana), summarise,
                  mean_Hday = mean(humidity_day), 
                  mean_Hnight=mean(Humidity.nigth), 
                  mean_Temp = mean(temperature),
                  mean_prec=mean(precipitation),
                  mean_NDVI = mean(NDVI))


#Plot of the  Humidity at day
ggplot(data = miz.clim)+
  geom_point(aes(x=volana,y=mean_Hday),size=2.5,shape=16,color="forestgreen")+
  geom_smooth(aes(x=volana,y=mean_Hday),method = "gam",formula =y ~ s(x, bs = "cc",k=-1)  )+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Day time humidity")+
  xlim(c(1,12))+
  theme_bw()


#now merge with your case data
merge.dat <- merge(Rou_M, miz.clim, by.x = "month",by.y = "volana")
View(merge.dat)
attach(merge.dat)
names(merge.dat)
merge.dat<-unique(data.frame(month,predicted_count,mean_Hday,mean_Hnight,mean_Temp,mean_NDVI,mean_prec))
detach(merge.dat)

merge.melt <- melt(merge.dat, id.vars = c("month"))

head (merge.melt)
View(merge.melt)

# and plot these together - temp and cases
Ar.dat1 = subset(merge.melt, variable=="predicted_count")
Ar.dat1$variable <- "mean_Hday"
Ar.dat2 = subset(merge.melt, variable=="predicted_count")
Ar.dat2$variable <- "mean_Hnight"
Ar.dat3 = subset(merge.melt, variable=="predicted_count")
Ar.dat3$variable <- "mean_Temp"
Ar.dat4 = subset(merge.melt, variable=="predicted_count")
Ar.dat4$variable <- "mean_NDVI"
Ar.dat5 = subset(merge.melt, variable=="predicted_count")
Ar.dat5$variable <- "mean_prec"

Ar.dat <- rbind(Ar.dat1, Ar.dat3,Ar.dat5)
head(Ar.dat)
unique(Ar.dat$variable)
scale=10

ggplot()+
  geom_line(data=Ar.dat, aes(x=month, y=value),  size=1, alpha=.2)+
  geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 )+
  geom_point(data=Rou,aes(x=month,y=bat_flies),alpha=.3,size=1)+
  geom_point(data=Ar.dat, aes(x=month, y=value), size=3)+
  geom_line(aes(x=month, y=value, color=variable),  size=3, show.legend = F,
            data = subset(merge.melt, variable=="mean_prec"|variable=="mean_Hday"|variable=="mean_Temp"))+
  theme_bw()+
  theme(axis.line.y.right = element_line(colour = "red"))+
  facet_wrap(.~variable, scales = "free_y")







#save as data
lag1 <- cbind.data.frame(lag = print(ccf(merge.dat$mean_Hday, merge.dat$predicted_count))$lag, acf=print(ccf(merge.dat$mean_Hday, merge.dat$predicted_count))$acf)
lag1$variable <- "mean_Hday"
#determine the optimal lag (here=0)
lag1$lag[lag1$acf==max(lag1$acf)]
#plot
print(ccf(merge.dat$mean_Hday, merge.dat$predicted_count))

# Lag_temperture
lag2 = cbind.data.frame(lag = print(ccf(merge.dat$mean_Temp, merge.dat$predicted_count))$lag, acf=print(ccf(merge.dat$mean_Temp, merge.dat$predicted_count))$acf)
lag2$variable <- "mean_Temp"
lag2$lag[lag2$acf==max(lag2$acf)]
# -1 is maximized cross correlation
# pred follow temp by 1
# 95% CI at 0.09


#and humidity
lag3 = cbind.data.frame(lag = print(ccf(merge.dat$mean_Hnight, merge.dat$predicted_count))$lag,
                        acf=print(ccf(merge.dat$mean_Hnight, merge.dat$predicted_count))$acf)
lag3$variable <- "mean_Hnight"
lag3$lag[lag3$acf==max(lag3$acf)]
# 1 only
# cases follow mean H2M by 1 week
# 95% CI at 0.09
#and humidity
lag4 = cbind.data.frame(lag = print(ccf(merge.dat$mean_NDVI, merge.dat$predicted_count))$lag,
                        acf=print(ccf(merge.dat$mean_NDVI, merge.dat$predicted_count))$acf)
lag4$variable <- "mean_NDVI"
lag4$lag[lag4$acf==max(lag4$acf)]
#and precipitation
lag5 = cbind.data.frame(lag = print(ccf(merge.dat$mean_prec, merge.dat$predicted_count))$lag,
                        acf=print(ccf(merge.dat$mean_prec, merge.dat$predicted_count))$acf)
lag5$variable <- "mean_prec"
lag5$lag[lag5$acf==max(lag5$acf)]
#save together
dat.lag <- rbind(lag1,lag2, lag5)

write.csv(dat.lag, file=paste0(homewd, "/Data-and-Code/lag_output.csv"), row.names = F)
andrana<-read.csv(paste0(homewd, "/Data-and-Code/lag_output.csv"))
View(andrana)

#include the optimal lag on plot
max.lag <- dlply(dat.lag, .(variable))
get.lag <- function(df){
  lag = df$lag[df$acf==max(df$acf)]
  df.out = cbind.data.frame(variable=unique(df$variable), lag=lag)
  return(df.out)
}
max.lag <- data.table::rbindlist(lapply(max.lag, get.lag))
max.lag$label = paste0("lag=", max.lag$lag, " month")

BM<-ggplot(dat.lag) + 
  geom_label(data=max.lag, aes(x=6,y=.4, label=label), label.size = 0) +
  geom_bar(aes(x=lag, y=acf), stat = "identity") + 
  geom_hline(aes(yintercept=0.09), color="blue", linetype=2) +
  geom_hline(aes(yintercept=-0.09), color="blue", linetype=2) +
  facet_grid(variable~.) + theme_bw() + theme(legend.position = c(.2,.87), panel.grid = element_blank(),
                                              legend.title = element_blank(),
                                              axis.title = element_text(size=16),
                                              strip.background = element_rect(fill="white"),
                                              strip.text = element_text(size=14),
                                              legend.text = element_text(size=12),
                                              plot.margin = unit(c(.2,.1,.1,1.1), "lines"),
                                              axis.text = element_text(size=14));BM

dat_temp<-subset(merge.melt,variable%in%c("mean_Temp","predicted_count"))

temp<-ggplot()+
  geom_line(aes(x=month,y=value,color=variable),color="grey50",linewidth=1,data = subset(dat_temp,variable=="predicted_count"))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_temp,variable=="predicted_count"))+
  geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 )+
  geom_line(aes(x=month,y=value/2,color=variable),color="red",linewidth=1,data = subset(dat_temp,variable=="mean_Temp"))+
  geom_point(aes(x=month,y=value/2,color=variable),color="red",size=3,data = subset(dat_temp,variable=="mean_Temp"))+
  scale_color_manual(values = c("mean_Temp"="red","predicted_count"="black"))+
  scale_y_continuous("Temperature",limits = c(0,13.9,2),breaks = seq(0,14,2), labels = seq(0,14,2)*2,sec.axis = sec_axis(~ ., name = "Abundance of Ectoparasites"))+
  theme_bw()+ theme(legend.position = 'none', panel.grid = element_blank(),
                    legend.title = element_blank(),
                    strip.text = element_text(size=14),
                    axis.title.y=element_text(size=14),
                    axis.title.y.right = element_text(size=14,angle = 90),
                    axis.title.x = element_blank(),
                    strip.background = element_rect(fill="white"),
                    legend.text = element_text(size=12),
                    plot.margin = unit(c(.2,.1,1.3,1.1), "lines"),
                    axis.line.y.right = element_line(color = "black"),
                    axis.line.y.left = element_line(color = "red"),
                    axis.text = element_text(size=14))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov"));temp


dat_Hday<-subset(merge.melt,variable%in%c("mean_Hday","predicted_count"))

hday<-ggplot()+
  geom_line(aes(x=month,y=value*10,color=variable),color="grey50",linewidth=1,data = subset(dat_Hday,variable=="predicted_count"))+
  geom_point(aes(x=month,y=value*10,color=variable),color="grey50",size=3,data = subset(dat_Hday,variable=="predicted_count"))+
  geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci*10 , ymax=predicted_count_uci *10), fill="black",alpha=.3 )+
  geom_line(aes(x=month,y=value*2,color=variable),color="red",linewidth=1,data = subset(dat_Hday,variable=="mean_Hday"))+
  geom_point(aes(x=month,y=value*2,color=variable),color="red",size=3,data = subset(dat_Hday,variable=="mean_Hday"))+
  scale_color_manual(values = c("mean_Hday"="red","predicted_count"="grey50"))+
  scale_y_continuous("Average humidity",limits = c(0,139),breaks = seq(0,150,50),labels = seq(0,150,50)/2, sec.axis = sec_axis(~ ./10, name = "Abundance of Ectoparasites"))+
  theme_bw() + theme(legend.position = "none", 
                     panel.grid = element_blank(),
                     legend.title = element_blank(),
                     #axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y.right = element_text(size=14,angle = 90),
                     strip.text = element_text(size=14),
                     axis.title.y=element_text(size=14), 
                     strip.background = element_rect(fill="white"),
                     axis.line.y.right = element_line(color = "black"),
                     axis.line.y.left = element_line(color = "red"),
                     legend.text = element_blank(),
                     plot.margin = unit(c(.2,.1,1.3,1.1), "lines"),
                     axis.text = element_text(size=14))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov")); hday


dat_prec<-subset(merge.melt,variable%in%c("mean_prec","predicted_count"))
dat_prec$value[dat_prec$variable=="mean_prec"]
min(dat_prec$value[dat_prec$variable=="mean_prec"])*10


prec<-ggplot()+
  geom_line(aes(x=month,y=value,color=variable), color="grey50",linewidth=1,data = subset(dat_Hday,variable=="predicted_count"))+
  geom_point(aes(x=month,y=value,color=variable),color="grey50",size=3,data = subset(dat_Hday,variable=="predicted_count"))+
  geom_ribbon(data = Rou, aes(x= month, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 )+
  geom_point(aes(x=month,y=value*20,color=variable),color="red",size=3,data = subset(dat_prec,variable=="mean_prec"))+
  geom_line(aes(x=month,y=value*20,color=variable),color="red",linewidth=1,data = subset(dat_prec,variable=="mean_prec"))+
  scale_color_manual(values = c("mean_prec"="red","predicted_count"="blue"))+
  scale_y_continuous("Average Precipitation",breaks = seq(0,12,2), labels = c(0,.1,.2,.3,.4,.5,.6), sec.axis = sec_axis(~ ., name = "Abundance of Ectoparasites"))+
  theme_bw() + theme(legend.position = "none", 
                     legend.text = element_blank(),
                     panel.grid = element_blank(),
                     strip.text = element_text(size=14),
                     strip.background = element_rect(fill="white"),
                     axis.title.y=element_text(size=13),
                     axis.title.y.right = element_text(size=14,angle = 90),
                     axis.title.x = element_blank(),
                     axis.text.y = element_text(size=13),
                     axis.text.x = element_blank(),
                    # axis.ticks.x = element_blank(),
                     axis.line.y.right = element_line(color = "black"),
                     axis.line.y.left = element_line(color = "red"),
                     plot.margin = unit(c(.2,.1,1.3,1.1), "lines"))+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","mar","may","jul","sep","nov"));prec




library(cowplot)

m<-ggdraw()+
  draw_plot(hday,x=-.002,y=.655,width = .503,height = .345)+
  draw_plot(prec,x=-.0035,y=.34,width = .505,height = .3425)+
  draw_plot(temp,x=0,y=.0,width = .502,height = .368);m

#Fig.miz_male <- cowplot::plot_grid(m, BM, rel_widths = c(1,1.1), nrow = 1, ncol = 2, labels = c("A", "B"), label_size = 22);Fig.miz_male

ggdraw()+
  draw_plot(m,x=0,y=0,width = 1,height = 1)+
  draw_plot(BM,x=0.5,y=0,width = .5,height = 1)

ggsave(file = paste0(homewd, "/Other-figures/Fig_lag_rousettus_withtransformation.png"),
       units="mm",  
       width=90, 
       height=80, 
       scale=3, 
       dpi=300)
