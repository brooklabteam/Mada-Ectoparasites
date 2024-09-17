rm(list=ls())
### 
#import data
# Packages to use 
library(tidyverse)
library(readr)
library(mgcv)
library(mgcViz)
library(ggplot2)
library(gratia)
library(lubridate)

# Data importation
homewd = "/Users/carabrook/Developer/Mada-Ectoparasites"
setwd(homewd)

cap <- read.csv(paste0(homewd,"/data/ecto_dat_long.csv"), header = T, stringsAsFactors = F)

names(cap)
# Data CHOICE
# Count is the number of Cyclopodia male+female
# Male and Female= numbers of the cyclopodia Male and Female
data1 <- dplyr::select(cap, roost_site, processing_date, #collection_date,
                       bat_species, bat_sex,sampleid, 
                       bat_age_class, bat_weight_g,
                       body_length_cm, bat_sex,bat_age_class,
                       bat_forearm_mm, mass_forearm_residual, #bat_tibia_mm,
                       #ear_length_mm, gonad_length_mm, 
                       #gonad_width_mm,
                       bat_flies,meglastreblidae,
                       fleas,mites,ticks)

data1$processing_date <- as.Date(data1$processing_date, format="%m/%d/%y")
#data1$collection_date <- as.Date(data1$collection_date, format = "%d-%m-%y")
#data1$collection_date<-ymd(data1$collection_date) # change the format of the collection date as Date using the function ymd (lubridate)
#class(data1$collection_date)

data1$yday<-yday(data1$processing_date)
#data1$yday<-yday(data1$collection_date)
#data1$Daty<-data1$collection_date # I make a copy of the collection date and rename the colume as "Daty"
data1$Daty<-data1$processing_date
# then I separate the colume date into year,month and day (we may need it later)
data1<-separate(data1, col = Daty, into = c("year", "month","day"), sep = "-")

#Check if every thing is goiing well
head(data1)
head(data1,2)
tail(data1)
str(data1)


# Now I change the class of each variable
data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
data1$ body_length_cm<-as.numeric(data1$ body_length_cm)
#data1$ ear_length_mm<-as.numeric(data1$ ear_length_mm)
#data1$ gonad_length_mm<-as.numeric(data1$ gonad_length_mm)
#data1$ gonad_width_mm<-as.numeric(data1$ gonad_width_mm)
data1$month<-as.numeric(data1$month)
data1$month<-as.factor(data1$ month)
data1$bat_sex<-as.factor(data1$bat_sex)
data1$bat_age_class<-as.factor(data1$bat_age_class)
data1$year<-as.factor(data1$year)
data1$day<-as.numeric(data1$day)

data1$bat_flies<-as.numeric(data1$bat_flies)
data1$meglastreblidae<-as.numeric(data1$meglastreblidae)
data1$meglastreblidae[is.na(data1$meglastreblidae)]<-0
data1$bat_flies[is.na(data1$bat_flies)]<-0



############################EIDOLON DUPREANUM##########################################
# Now i Am working on EIdolon only
# so I make a subset of the data 
unique(data1$bat_species)
unique(data1$bat_age_class)
#look at seasonality in adults
EID<-subset(data1,bat_species=="Eidolon dupreanum" & bat_sex!="unknown", bat_age_class!="J" & !is.na(bat_age_class))
unique(EID$bat_sex)
EID$bat_sex <- droplevels(EID$bat_sex)

unique(EID$roost_site)

# ## I Change the name of the three sites
# EID$roost_site[EID$roost_site%in%c("AngavoBe","AngavoKely","Lakato")]<-"east"
# EID$roost_site[EID$roost_site%in%c("Ankarana_Cathedral","Ankarana_Canyon" )]<-"north"

EID$roost_site[EID$roost_site%in%c("Angavokely_Edup","Angavobe_Edup","Lakato_Edup")]<-"east"
EID$roost_site[EID$roost_site%in%c("Ankarana_Cathedral_Edup","Ankarana_Canyon_Edup", "Ankarana_Chauves_Souris_Edup" )]<-"north"
EID$roost_site[EID$roost_site%in%c("Mahabo_Edup" )]<-"west"
unique(EID$roost_site)
# 
# 
# #for seasonality, we will focus just on the eastern sites
# EIDM<-subset(EID,bat_sex=="male"&roost_site=="east")
# EIDF<-subset(EID,bat_sex=="female"&roost_site=="east")
# 
# ##### GAM 
# gamEIDM <- gam(bat_flies~ s(yday, k=7, bs = "cc"), data = EIDM)
# gamEIDF <- gam(bat_flies~ s(yday, k=7, bs = "cc"), data = EIDF)

#try joint
EID.east = subset(EID, roost_site=="east")

#compare seasonality by sex, and lumped together. also include predictors of mass:forearm residual

#here by sex
gamEID1 <- gam(bat_flies~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = EID.east)
summary(gamEID1)
#here lumped
gamEID2 <- gam(bat_flies~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = EID.east)
summary(gamEID2)
#here without MFR
gamEID3 <- gam(bat_flies~ s(yday, by=bat_sex, k=7, bs = "cc")  + s(bat_sex, bs="re"), data = EID.east)
summary(gamEID3)
#here lumped
gamEID4 <- gam(bat_flies~ s(yday, k=7, bs = "cc") + s(bat_sex, bs="re"), data = EID.east)
summary(gamEID4)
#here without sex as RE
gamEID5 <- gam(bat_flies~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = EID.east)
summary(gamEID5)
#here lumped
gamEID6 <- gam(bat_flies~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = EID.east)
summary(gamEID6)

#model comparison
AIC(gamEID1, gamEID2, gamEID3, gamEID4, gamEID5, gamEID6)
#model 1 and model 5 are the best - 5 has fewer variables so we'll go with that. includes MFR as predictor (though not a significant one) and also includes seasonality by sex


#look at the seasonality by sex now
#first just a glance
par(mfrow=c(3,1))
plot(gamEID5)


#and now the actual plot


EID.east.sub <- cbind.data.frame(yday=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
EID.east.sub$mass_forearm_residual = 0
EID.east.sub$predicted_count <- predict.gam(gamEID5,type="response", newdata = EID.east.sub, exclude=c("mass_forearm_residual"))
EID.east.sub$predicted_count_SE <- predict.gam(gamEID5,type="response", newdata = EID.east.sub, exclude=c("mass_forearm_residual"), se.fit = T)$se.fit
EID.east.sub$predicted_count_lci <- EID.east.sub$predicted_count -1.96*EID.east.sub$predicted_count_SE
EID.east.sub$predicted_count_uci <- EID.east.sub$predicted_count +1.96*EID.east.sub$predicted_count_SE
EID.east.sub$predicted_count_lci[EID.east.sub$predicted_count_lci<0] <- 0#now add the predictions to each dataframe
EID.east.sub$predicted_count[EID.east.sub$predicted_count<0] <- 0

seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))
seas.dat$bat_sex="male"
preg.dat <- cbind.data.frame(x = c(yday("2014-07-07"), yday("2019-11-16")))
preg.dat$xlab = "F"
preg.dat$bat_sex="female"
seas.dat <- rbind(seas.dat, preg.dat)



Eall<-ggplot(data = EID.east) + facet_grid(~bat_sex) +
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf, fill=bat_sex),alpha=0.3, show.legend = F)+
  geom_point(aes(x= as.numeric(yday), y= bat_flies), alpha=.3, show.legend = F)+
  scale_fill_manual(values=c("pink", "cornflowerblue")) +
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  ylab(bquote('Count of'~italic('Cyclopodia dubia')))+
  #ggtitle("host male")+ 
  geom_ribbon(data = EID.east.sub, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = EID.east.sub, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        strip.text = element_text(size=16),
        plot.margin = unit(c(.3,.2,0,.3), "cm"),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank())+
  scale_x_continuous(breaks=c(60,152,244,335), labels=c("Mar", "Jun", "Sep", "Dec"));Eall



#now do the same for the Rousettus bats
# both eucampsipoda and meglastrebla (latter might not be enough data)


####### ROUSETTUS MADAGASSCARIENSIS ######
seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))
preg.dat <- cbind.data.frame(x = c(yday("2014-09-11"), yday("2019-12-11")))
preg.dat$xlab = "F"
seas.dat$bat_sex = "male"
preg.dat$bat_sex="female"
seas.dat <- rbind(seas.dat, preg.dat)
#####EUCAMPSIPODA MADAGASCARIENSIS #####

unique(data1$bat_species)
unique(data1$bat_age_class)
ROU<-subset(data1,bat_species=="Rousettus madagascariensis" & bat_age_class!="J" & !is.na(bat_age_class))
ROU$bat_sex <- droplevels(ROU$bat_sex)
unique(ROU$bat_sex)
unique(ROU$roost_site)

ROU$roost_site[ROU$roost_site=="Maromizaha_Rmad"] <- "east"
ROU$roost_site[ROU$roost_site%in%c("Ankarana_Cathedral_Rmad","Ankarana_Canyon_Rmad" )]<-"north"
ROU$roost_site[ROU$roost_site=="Makira_Rmad"] <- "far_east"

ROU.east = subset(ROU, roost_site=="east")
### GAMS


#compare seasonality by sex, and lumped together. also include predictors of mass:forearm residual

#here by sex
gamROU1 <- gam(bat_flies~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROU1)
#here lumped
gamROU2 <- gam(bat_flies~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROU2)
#here without MFR
gamROU3 <- gam(bat_flies~ s(yday, by=bat_sex, k=7, bs = "cc")  + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROU3)
#here lumped
gamROU4 <- gam(bat_flies~ s(yday, k=7, bs = "cc") + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROU4)
#here without sex as RE
gamROU5 <- gam(bat_flies~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = ROU.east)
summary(gamROU5)
#here lumped
gamROU6 <- gam(bat_flies~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = ROU.east)
summary(gamROU6)

#model comparison
AIC(gamROU1, gamROU2, gamROU3, gamROU4, gamROU5, gamROU6)
#model 1,2, and model 5 are the best but  1 is the best. includes random effect of bat sex in addition to the factors in the eidolon plot



#and plot same as above
par(mfrow=c(4,1))
plot(gamROU1)

ROU.east.sub <- cbind.data.frame(yday=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
ROU.east.sub$mass_forearm_residual = 0
ROU.east.sub$predicted_count <- predict.gam(gamROU1,type="response", newdata = ROU.east.sub, exclude = c("mass_forearm_residual"))
ROU.east.sub$predicted_count_SE <- predict.gam(gamROU1,type="response", newdata = ROU.east.sub, exclude = c("mass_forearm_residual"), se.fit = T)$se.fit
ROU.east.sub$predicted_count_lci <- ROU.east.sub$predicted_count -1.96*ROU.east.sub$predicted_count_SE 
ROU.east.sub$predicted_count_uci <- ROU.east.sub$predicted_count +1.96*ROU.east.sub$predicted_count_SE 
ROU.east.sub$predicted_count_lci[ROU.east.sub$predicted_count_lci<0] <- 0
# #and now the actual plot
# ROU.east$predicted_count[!is.na(ROU.east$mass_forearm_residual)] <- predict.gam(gamROU1,type="response", exclude = c("bat_sex", "mass_forearm_residual"))
# ROU.east$predicted_count_SE[!is.na(ROU.east$mass_forearm_residual)] <- predict.gam(gamROU5,type="response", se.fit = T)$se.fit
# ROU.east$predicted_count_lci[!is.na(ROU.east$mass_forearm_residual)] <- ROU.east$predicted_count[!is.na(ROU.east$mass_forearm_residual)] -1.96*ROU.east$predicted_count_SE[!is.na(ROU.east$mass_forearm_residual)]
# ROU.east$predicted_count_uci[!is.na(ROU.east$mass_forearm_residual)] <- ROU.east$predicted_count[!is.na(ROU.east$mass_forearm_residual)] +1.96*ROU.east$predicted_count_SE[!is.na(ROU.east$mass_forearm_residual)]
# ROU.east$predicted_count_lci[ROU.east$predicted_count_lci<0& !is.na(ROU.east$mass_forearm_residual)] <- 0#now add the predictions to each dataframe



seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))
preg.dat <- cbind.data.frame(x = c(yday("2014-09-11"), yday("2019-12-12")))
preg.dat$xlab = "F"
seas.dat$bat_sex="male"
preg.dat$bat_sex="female"
seas.dat <- rbind(seas.dat, preg.dat)

#ROU.east.sub = subset(ROU.east, !is.na(mass_forearm_residual & !is.na(bat_sex)))


Rall<-ggplot(data = ROU.east) + facet_grid(~bat_sex) +
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf, fill=bat_sex),alpha=0.3, show.legend = F)+
  geom_point(aes(x= as.numeric(yday), y= bat_flies), alpha=.3, show.legend = F)+
  scale_fill_manual(values=c("pink", "cornflowerblue")) +
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  ylab(bquote('Count of'~italic('Eucampsipoda madagascariensis')))+
  #ggtitle("host male")+ 
  geom_ribbon(data = ROU.east.sub, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROU.east.sub, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        strip.text = element_text(size=16),
        plot.margin = unit(c(.2,.2,.3,.3), "cm"),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks=c(60,152,244,335), labels=c("Mar", "Jun", "Sep", "Dec"));Rall


######MEGASTREBLIDAE #######
##### GAM 


#compare seasonality by sex, and lumped together. also include predictors of mass:forearm residual

#here by sex
gamROUmeg1 <- gam(meglastreblidae~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROUmeg1)
#here lumped
gamROUmeg2 <- gam(meglastreblidae~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROUmeg2)
#here without MFR
gamROUmeg3 <- gam(meglastreblidae~ s(yday, by=bat_sex, k=7, bs = "cc")  + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROUmeg3)
#here lumped
gamROUmeg4 <- gam(meglastreblidae~ s(yday, k=7, bs = "cc") + s(bat_sex, bs="re"), data = ROU.east)
summary(gamROUmeg4)
#here without sex as RE
gamROUmeg5 <- gam(meglastreblidae~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = ROU.east)
summary(gamROUmeg5)
#here lumped
gamROUmeg6 <- gam(meglastreblidae~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = ROU.east)
summary(gamROUmeg6)

#model comparison
AIC(gamROUmeg1, gamROUmeg2, gamROUmeg3, gamROUmeg4, gamROUmeg5, gamROUmeg6)
#model 1,2, and model 5 are the best but  1 is the best. includes random effect of bat sex in addition to the factors in the eidolon plot

summary(gamROUmeg1) 
#interesting... no significant seasonality for males and females
#but there is a significance of sex and of MFR
#high MFI is positively associated with infection (probably a mass effect)
#very few females
#might mean this plot is silly


#and plot same as above
par(mfrow=c(4,1))
plot(gamROUmeg1) 

ROU.east.sub.meg <- cbind.data.frame(yday=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
ROU.east.sub.meg$mass_forearm_residual = 0
ROU.east.sub.meg$predicted_count <- predict.gam(gamROUmeg1,type="response", newdata = ROU.east.sub.meg, exclude = c("mass_forearm_residual"))
ROU.east.sub.meg$predicted_count_SE <- predict.gam(gamROUmeg1,type="response", newdata = ROU.east.sub.meg, exclude = c("mass_forearm_residual"), se.fit = T)$se.fit
ROU.east.sub.meg$predicted_count_lci <- ROU.east.sub.meg$predicted_count -1.96*ROU.east.sub.meg$predicted_count_SE 
ROU.east.sub.meg$predicted_count_uci <- ROU.east.sub.meg$predicted_count +1.96*ROU.east.sub.meg$predicted_count_SE 
ROU.east.sub.meg$predicted_count_lci[ROU.east.sub.meg$predicted_count_lci<0] <- 0


#this plot is probably not needed since we showed statistically that
#meglastrebla abundance was not significantly seasonal.
Rallmeg<-ggplot(data = ROU.east) + facet_grid(~bat_sex) +
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf, fill=bat_sex),alpha=0.3, show.legend = F)+
  geom_point(aes(x= as.numeric(yday), y= meglastreblidae), alpha=.3, show.legend = F)+
  scale_fill_manual(values=c("pink", "cornflowerblue")) +
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  ylab(bquote('Count of'~italic('Megastreblia wenzeli')))+
  #ggtitle("host male")+ 
  geom_ribbon(data = ROU.east.sub.meg, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROU.east.sub.meg, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,.2,.2,.2), "cm"),
        strip.text = element_text(size=16),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank());Rallmeg


#the plot then will just have the top two panels
pFig2 <- cowplot::plot_grid(Eall, Rall, ncol=1, nrow = 2, labels=c("A","B"), label_size = 22,label_y = c(1,1.07))
ggsave(file =  paste0(homewd,"/final-figures/Fig2.png"),
       plot = pFig2,
       units="mm",  
       width=90, 
       height=75, 
       scale=3, 
       dpi=300)


#partial effects will just be presented as stats tables, not as plots, so we are done now.
