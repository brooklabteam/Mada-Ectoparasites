rm(list=ls())
### 
#import data
# Packages to use 
library(tidyverse)
library(plyr)
library(dplyr)
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
data1 <- dplyr::select(cap, roost_site, processing_date, sampling_session,#collection_date,
                       latitude_s,           
                       longitude_e,
                       bat_species, bat_sex,sampleid, 
                       bat_age_class, bat_weight_g,
                       bat_sex,bat_age_class,
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

#summarise bat captures by sampling sesson
tables1 <- ddply(data1,.(bat_species, sampling_session, bat_sex, roost_site, latitude_s, longitude_e), summarise, sampling_start_date = min(processing_date), N_captured = length(sampleid))
tables1$roost_site <- sapply(strsplit(tables1$roost_site, "_"), "[", 1)
unique(tables1$roost_site)
unique(tables1$bat_species)
write.csv(tables1, paste0(homewd, "/final-tables/tableS1.csv"), row.names = F)

# Now I change the class of each variable
data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
#data1$ body_length_cm<-as.numeric(data1$ body_length_cm)
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

EID$roost_site[EID$roost_site%in%c("Angavokely_Edup","Angavobe_Edup")]<-"east"
EID$roost_site[EID$roost_site%in%c("Ankarana_Cathedral_Edup","Ankarana_Canyon_Edup", "Ankarana_Chauves_Souris_Edup" )]<-"north"
#EID$roost_site[EID$roost_site%in%c("Mahabo_Edup" )]<-"west"
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
gamEID1 <- gam(bat_flies~ s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re") +s(yday, by=bat_sex, k=7, bs = "cc") +s(year, bs="re"), data = EID.east, family = poisson)
summary(gamEID1)

#here lumped seasonality
gamEID2 <- gam(bat_flies~ s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re")  + s(yday, k=7, bs = "cc")+s(year, bs="re"), data = EID.east, family = poisson)
summary(gamEID2)


#here without MFR
gamEID3 <- gam(bat_flies~  s(bat_sex, bs="re")  + s(yday, by=bat_sex, k=7, bs = "cc")+s(year, bs="re")  , data = EID.east, family = poisson)
summary(gamEID3)

#here without MFR + lumped seasonality
gamEID4 <- gam(bat_flies~ s(bat_sex, bs="re") + s(yday, k=7, bs = "cc")+s(year, bs="re"), data = EID.east, family = poisson)
summary(gamEID4)

#here without sex as RE
gamEID5 <-  gam(bat_flies~ s(mass_forearm_residual, bs="tp") + s(yday, by=bat_sex, k=7, bs = "cc")+s(year, bs="re"), data = EID.east, family = poisson)
summary(gamEID5)

#here without sex as RE + lumped seasonality
gamEID6 <- gam(bat_flies~ s(mass_forearm_residual, bs="tp") + s(yday, k=7, bs = "cc")+s(year, bs="re"), data = EID.east, family = poisson)
summary(gamEID6)


#model comparison
AIC(gamEID1, gamEID2, gamEID3, gamEID4, gamEID5, gamEID6)
#model 1 and 5 are the best but 5 has fewer variables


#look at the seasonality by sex now
#first just a glance
par(mfrow=c(4,1))
plot(gamEID5)
summary(gamEID5)

#and now the actual plot for the main text
EID.east.sub <- cbind.data.frame(yday=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
EID.east.sub$year = 2019
EID.east.sub$mass_forearm_residual = 0
EID.east.sub$predicted_count <- predict.gam(gamEID5,type="response", newdata = EID.east.sub, exclude=c("mass_forearm_residual", "year"))
EID.east.sub$predicted_count_SE <- predict.gam(gamEID5,type="response", newdata = EID.east.sub, exclude=c("mass_forearm_residual", "year"), se.fit = T)$se.fit
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
  #xlab ("Day of the Year")+ 
  ylab(bquote('count of'~italic('Cyclopodia dubia')))+
  #ggtitle("host male")+ 
  geom_ribbon(data = EID.east.sub, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = EID.east.sub, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        strip.text = element_text(size=16),
        plot.margin = unit(c(.3,.2,.6,.7), "cm"),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank())+
  scale_x_continuous(breaks=c(60,152,244,335), labels=c("Mar", "Jun", "Sep", "Dec"));Eall



#and evaluate relationship with MFR
source(paste0(homewd,"/figure-development/prep-files/mollentze-streicker-2020-functions.R"))
plot.partial.cont <- function(df, log, var, response_var, alt_var, legend.on){
  df1 = df$effects
  df2= df$partialResiduals
  #head(df2)
  
  #head(df1)
  names(df1)[names(df1)==var] <- "var"
  names(df2)[names(df2)==var] <- "var"
  
  fillz = c("No"="gray70", "Yes" = "skyblue3")
  
  
  #p2 <- ggplot(data=df2, aes(var,  Residual)) +
  #     geom_boxplot(aes(var~Residual))
  if(legend.on==TRUE){
    if(log==F){
      
      p1 <- ggplot(data=df1, aes(var, y)) + 
        geom_line(aes(color=IsSignificant), size=3)+
        geom_ribbon(aes(ymin=ylower, ymax=yupper, fill=IsSignificant), 
                    alpha=.4, show.legend = F) +
        #geom_point(aes(x=var, y=y, color=var), size=5) +
        #geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
        scale_fill_manual(values = fillz) +
        scale_color_manual(values = fillz) +
        scale_x_continuous(labels=scales::comma) +
        geom_hline(aes(yintercept=0), linetype=2) + theme_bw() +
        theme(panel.grid = element_blank(),
              axis.title = element_text(size=16),
              legend.text = element_text(size=12),
              axis.text = element_text(size=14),
              #plot.margin = unit(c(.5,.1,.1,1), "cm"),
              plot.margin = unit(c(.2,.2,0,.1), "cm"),
              legend.position = c(.85,.15))+
        ylab(paste0("partial effect on ", response_var)) + xlab(alt_var)
      
      
      
    }else{
      df1$var <- 10^(df1$var)
      
      p1 <- ggplot(data=df1, aes(var, y)) + 
        geom_line(aes(color=IsSignificant), size=3)+
        geom_ribbon(aes(ymin=ylower, ymax=yupper, fill=IsSignificant), 
                    alpha=.4, show.legend = F) +
        #geom_point(aes(x=var, y=y, color=var), size=5) +
        #geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
        scale_fill_manual(values = fillz) +
        scale_color_manual(values = fillz) +
        scale_x_continuous(labels=scales::comma) +
        geom_hline(aes(yintercept=0), linetype=2) + theme_bw() +
        theme(panel.grid = element_blank(), #axis.title.x = element_blank(),
              #axis.text.x = element_text(size=8, angle = 45),
              plot.margin = unit(c(.5,.1,.1,1), "cm"),
              legend.position = c(.85,.15))+
        ylab(paste0("partial effect on ", response_var)) + xlab(alt_var)
    }}else{
      if(log==F){
        
        p1 <- ggplot(data=df1, aes(var, y)) + 
          geom_line(aes(color=IsSignificant), size=3, show.legend = F)+
          geom_ribbon(aes(ymin=ylower, ymax=yupper, fill=IsSignificant), 
                      alpha=.4, show.legend = F) +
          #geom_point(aes(x=var, y=y, color=var), size=5) +
          #geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
          scale_fill_manual(values = fillz) +
          scale_color_manual(values = fillz) +
          scale_x_continuous(labels=scales::comma) +
          geom_hline(aes(yintercept=0), linetype=2) + theme_bw() +
          theme(panel.grid = element_blank(),
                axis.title = element_text(size=16),
                axis.text = element_text(size=14),
                #plot.margin = unit(c(.5,.1,.1,1), "cm"),
                plot.margin = unit(c(.2,.2,0,.1), "cm"),
                legend.position = c(.15,.15))+
          ylab(paste0("partial effect on ", response_var)) + xlab(alt_var)
        
        
        
      }else{
        df1$var <- 10^(df1$var)
        
        p1 <- ggplot(data=df1, aes(var, y)) + 
          geom_line(aes(color=IsSignificant), size=3, show.legend = F)+
          geom_ribbon(aes(ymin=ylower, ymax=yupper, fill=IsSignificant), 
                      alpha=.4, show.legend = F) +
          #geom_point(aes(x=var, y=y, color=var), size=5) +
          #geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
          scale_fill_manual(values = fillz) +
          scale_color_manual(values = fillz) +
          scale_x_continuous(labels=scales::comma) +
          geom_hline(aes(yintercept=0), linetype=2) + theme_bw() +
          theme(panel.grid = element_blank(), #axis.title.x = element_blank(),
                #axis.text.x = element_text(size=8, angle = 45),
                plot.margin = unit(c(.5,.1,.1,1), "cm"), 
                legend.position = c(.15,.15))+
          ylab(paste0("partial effect on ", response_var)) + xlab(alt_var)
      }
    }
  
  print(p1)
  
  return(p1)
}
detach("package:plyr", unload = TRUE)
library(dplyr)
MFR.dat <- get_partial_effects_continuous(gamFit = gamEID5, var="mass_forearm_residual")
EidB <- plot.partial.cont(df = MFR.dat, log=F, var="mass_forearm_residual", response_var = "count", alt_var = "mass: forearm residual", legend.on = F)

pEid <- cowplot::plot_grid(Eall, EidB, ncol = 2, nrow=1, labels = c("A", "B"), label_size = 22, rel_widths = c(2,1), label_x = c(-.010,0));pEid



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
#ROU$roost_site[ROU$roost_site=="Makira_Rmad"] <- "far_east"

ROU.east = subset(ROU, roost_site=="east")

### GAMS


#compare seasonality by sex, and lumped together. also include predictors of mass:forearm residual

#here by sex
#summary(gamROU1)

gamROU1 <- gam(bat_flies~  s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re") + s(yday, by=bat_sex, k=7, bs = "cc")+s(year, bs="re"), data = ROU.east, family = poisson)
summary(gamROU1)


#here lumped seasonality
gamROU2 <- gam(bat_flies~  s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re") + s(yday, k=7, bs = "cc")+s(year, bs="re"), data = ROU.east, family = poisson)
summary(gamROU2)

#here without MFR 
gamROU3 <- gam(bat_flies~   s(bat_sex, bs="re")  + s(yday, by=bat_sex, k=7, bs = "cc")+s(year, bs="re"), data = ROU.east, family = poisson)
summary(gamROU3)

#here without MFR + lumped seasonality
gamROU4 <- gam(bat_flies~   s(bat_sex, bs="re") + s(yday, k=7, bs = "cc")+s(year, bs="re"), data = ROU.east, family = poisson)
summary(gamROU4)


#here without sex as RE
gamROU5 <- gam(bat_flies~  s(mass_forearm_residual, bs="tp") + s(yday, by=bat_sex, k=7, bs = "cc") +s(year, bs="re"), data = ROU.east, family = poisson)
summary(gamROU5)

#here without sex plus lumped seasonality
gamROU6 <- gam(bat_flies~  s(mass_forearm_residual, bs="tp")  + s(yday, k=7, bs = "cc") +s(year, bs="re"), data = ROU.east, family = poisson)
summary(gamROU6)

#model comparison
AIC(gamROU1, gamROU2, gamROU3, gamROU4, gamROU5, gamROU6)
#model 1 and 5 are  best but 5 has fewer variables. 




#and plot same as above
par(mfrow=c(4,1))
plot(gamROU5)

ROU.east.sub <- cbind.data.frame(yday=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
ROU.east.sub$year = 2019
ROU.east.sub$mass_forearm_residual = 0
ROU.east.sub$predicted_count <- predict.gam(gamROU5,type="response", newdata = ROU.east.sub, exclude = c("mass_forearm_residual", "year"))
ROU.east.sub$predicted_count_SE <- predict.gam(gamROU5,type="response", newdata = ROU.east.sub, exclude = c("mass_forearm_residual", "year"), se.fit = T)$se.fit
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
  #xlab ("Day of the Year")+ 
  ylab(bquote('count of'~italic('Eucampsipoda madagascariensis')))+
  #ggtitle("host male")+ 
  geom_ribbon(data = ROU.east.sub, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROU.east.sub, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        strip.text = element_text(size=16),
        plot.margin = unit(c(.2,.2,.6,.7), "cm"),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks=c(60,152,244,335), labels=c("Mar", "Jun", "Sep", "Dec"));Rall



#and relationship with MFR
MFR.dat.Rou <- get_partial_effects_continuous(gamFit = gamROU5, var="mass_forearm_residual")
add <- MFR.dat.Rou$effects[nrow(MFR.dat.Rou$effects),]
add$y <- add$se <- add$ylower <- add$yupper<- add$NotSignificant <- add$mass_forearm_residual<- add$yday<- add$bat_sex<- add$year <-  NA
add$IsSignificant <- "No"
MFR.dat.Rou$effects <- rbind(MFR.dat.Rou$effects, add)

RouB <- plot.partial.cont(df = MFR.dat.Rou, log=F, var="mass_forearm_residual", response_var = "count", alt_var = "mass: forearm residual", legend.on = T)
pRou <- cowplot::plot_grid(Rall, RouB, ncol = 2, nrow=1, labels = c("C", "D"), label_size = 22, rel_widths = c(2,1), label_x = c(-.010,0), label_y = c(1.04,1.04));pRou

pFig2 <- cowplot::plot_grid(pEid, pRou, ncol = 1, nrow = 2);pFig2

ggsave(file =  paste0(homewd,"/final-figures/Fig2.png"),
       plot = pFig2,
       units="mm",  
       width=110, 
       height=75, 
       scale=3, 
       dpi=300)








# 
# 
# 
# #partial effects will just be presented as stats tables, not as plots, so we are done now.
# 
# #playing with megastrebla but don't use it:
# 
# ######MEGASTREBLIDAE #######
# ##### GAM 
# 
# 
# #compare seasonality by sex, and lumped together. also include predictors of mass:forearm residual
# 
# #here by sex
# gamROUmeg1 <- gam(meglastreblidae~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = ROU.east)
# summary(gamROUmeg1)
# #here lumped
# gamROUmeg2 <- gam(meglastreblidae~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp") + s(bat_sex, bs="re"), data = ROU.east)
# summary(gamROUmeg2)
# #here without MFR
# gamROUmeg3 <- gam(meglastreblidae~ s(yday, by=bat_sex, k=7, bs = "cc")  + s(bat_sex, bs="re"), data = ROU.east)
# summary(gamROUmeg3)
# #here lumped
# gamROUmeg4 <- gam(meglastreblidae~ s(yday, k=7, bs = "cc") + s(bat_sex, bs="re"), data = ROU.east)
# summary(gamROUmeg4)
# #here without sex as RE
# gamROUmeg5 <- gam(meglastreblidae~ s(yday, by=bat_sex, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = ROU.east)
# summary(gamROUmeg5)
# #here lumped
# gamROUmeg6 <- gam(meglastreblidae~ s(yday, k=7, bs = "cc") + s(mass_forearm_residual, bs="tp"), data = ROU.east)
# summary(gamROUmeg6)
# 
# #model comparison
# AIC(gamROUmeg1, gamROUmeg2, gamROUmeg3, gamROUmeg4, gamROUmeg5, gamROUmeg6)
# #model 1,2, and model 5 are the best but  1 is the best. includes random effect of bat sex in addition to the factors in the eidolon plot
# 
# summary(gamROUmeg1) 
# #interesting... no significant seasonality for males and females
# #but there is a significance of sex and of MFR
# #high MFI is positively associated with infection (probably a mass effect)
# #very few females
# #might mean this plot is silly
# 
# 
# #and plot same as above
# par(mfrow=c(4,1))
# plot(gamROUmeg1) 
# 
# ROU.east.sub.meg <- cbind.data.frame(yday=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
# ROU.east.sub.meg$mass_forearm_residual = 0
# ROU.east.sub.meg$predicted_count <- predict.gam(gamROUmeg1,type="response", newdata = ROU.east.sub.meg, exclude = c("mass_forearm_residual"))
# ROU.east.sub.meg$predicted_count_SE <- predict.gam(gamROUmeg1,type="response", newdata = ROU.east.sub.meg, exclude = c("mass_forearm_residual"), se.fit = T)$se.fit
# ROU.east.sub.meg$predicted_count_lci <- ROU.east.sub.meg$predicted_count -1.96*ROU.east.sub.meg$predicted_count_SE 
# ROU.east.sub.meg$predicted_count_uci <- ROU.east.sub.meg$predicted_count +1.96*ROU.east.sub.meg$predicted_count_SE 
# ROU.east.sub.meg$predicted_count_lci[ROU.east.sub.meg$predicted_count_lci<0] <- 0
# 
# 
# #this plot is probably not needed since we showed statistically that
# #meglastrebla abundance was not significantly seasonal.
# Rallmeg<-ggplot(data = ROU.east) + facet_grid(~bat_sex) +
#   geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf, fill=bat_sex),alpha=0.3, show.legend = F)+
#   geom_point(aes(x= as.numeric(yday), y= meglastreblidae), alpha=.3, show.legend = F)+
#   scale_fill_manual(values=c("pink", "cornflowerblue")) +
#   #scale_color_manual(values=ColM)+ 
#   #scale_fill_manual(values=ColM)+ 
#   #geom_hline(aes(yintercept=0), color="gray50") +
#   xlab ("Day of the Year")+ 
#   ylab(bquote('Count of'~italic('Megastreblia wenzeli')))+
#   #ggtitle("host male")+ 
#   geom_ribbon(data = ROU.east.sub.meg, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
#   geom_line(data = ROU.east.sub.meg, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
#   theme_bw()+
#   theme(strip.background= element_rect(fill="white"), 
#         strip.text.y = element_text(face="italic"),
#         panel.grid = element_blank(),
#         plot.margin = unit(c(0,.2,.2,.2), "cm"),
#         strip.text = element_text(size=16),
#         axis.text = element_text(size = 14),
#         axis.title.y = element_text(size = 16),
#         axis.title.x = element_blank());Rallmeg
# 
