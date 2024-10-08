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
names(ectos)[names(ectos)=="Bat_SampleID"] <- c("sampleid")
length(unique(ectos$sampleid)) #840

unique(ectos$Ecto_type)

bf = subset(ectos, Ecto_type=="Bat fly" | Ecto_type=="Batflies")
head(bf)

#and plot by sex and species an date

cap <- read.csv(file= paste0(homewd, "/data/ecto_dat_long.csv"), header = T, stringsAsFactors = F)
head(cap,2)

cap$bat_flies <-as.numeric(cap$bat_flies)
cap$meglastreblidae <-as.numeric(cap$meglastreblidae)
cap$fleas <-as.numeric(cap$fleas)
cap$mites <-as.numeric(cap$mites)
cap$ticks <-as.numeric(cap$ticks)
cap$t_star <-as.numeric(cap$t_star)

cap$total_ectos =rowSums(cbind(cap$bat_flies, cap$meglastreblidae, cap$fleas,cap$mites, cap$ticks,cap$t_star), na.rm = T)

setdiff(ectos$sampleid, cap$sampleid) #"KELunlabeld1" "MIZ305X"      "MIZ306X"      "MIZ307X"      "MIZ388"      "MIZ478"    
#these will get discarded

ectos = subset(ectos, sampleid!="KELunlabeld1" & sampleid!= "MIZ305X"& sampleid!="MIZ306X"  & sampleid!="MIZ307X"& sampleid!= "MIZ388"& sampleid!="MIZ478")
ectos=subset(ectos, !is.na(Ecto_type))

setdiff(ectos$sampleid, cap$sampleid)
length(unique(intersect(ectos$sampleid, cap$sampleid))) #818
length(unique(ectos$sampleid)) #818
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

afa<-merge(bf, cap, by = "sampleid",all=T)
head(afa)
afa$processing_date <- as.Date(afa$processing_date, format = "%m/%d/%y")
afa = subset(afa, !is.na(processing_date))
afa = subset(afa, roost_site=="Angavobe_Edup" | roost_site=="Angavokely_Edup" | roost_site =="Maromizaha_Rmad")

afa <- dplyr::select(afa, sampleid, bat_species, bat_sex, bat_age_class, processing_date, mass_forearm_residual, Male, Female)

head(afa)
afa <- afa[complete.cases(afa),]

#and melt the data
afa.M <- dplyr::select(afa, names(afa)[1:7])
afa.F <- dplyr::select(afa, c(names(afa)[1:6], "Female"))

names(afa.M)[7] <- "count"
names(afa.F)[7] <- "count"

afa.M$fly_sex <- "male"
afa.F$fly_sex <- "female"

afa <- rbind(afa.F, afa.M)

head(afa)

ggplot(afa) + geom_point(aes(x=processing_date, y=count, color=fly_sex))+theme_bw() +
              facet_grid(bat_sex~bat_species) + scale_color_manual(values=c("hotpink2", "blue"))

#is there a difference in abundance of male vs. female bat flies? controlling by season?

afa$doy <- yday(afa$processing_date)
afa$year <- year(afa$processing_date)
unique(afa$bat_age_class)
afa$mass_forearm_residual <- as.numeric(afa$mass_forearm_residual)
afa$bat_sex <- as.factor(afa$bat_sex)
afa$fly_sex <- as.factor(afa$fly_sex)
afa$year <- as.factor(afa$year)

afa = subset(afa, bat_age_class!="J")



gamEdup1 <- gam(count~s(bat_sex, bs="re") + 
                 s(fly_sex, bs="re") + 
                 s(mass_forearm_residual, bs="tp")+
                 s(doy, bs="cc", k=5, by=bat_sex) + s(year, bs="re"), 
               data = subset(afa, bat_species == "Eidolon dupreanum"), family=poisson)
summary(gamEdup1)

#plot(gamEdup1)
#only seasonality of bat sex significant

source(paste0(homewd,"/figure-development/prep-files/mollentze-streicker-2020-functions.R"))
fly.sex <- get_partial_effects(fit=gamEdup1, var="fly_sex")
bat.sex <- get_partial_effects(fit=gamEdup1, var="bat_sex")
MFR.dat <- get_partial_effects_continuous(gamFit = gamEdup1, var="mass_forearm_residual")

plot.partial <- function(df, var, response_var){
  df1 = df$effects
  df2= df$partialResiduals
  #head(df2)
  
  #head(df1)
  names(df1)[names(df1)==var] <- "var"
  names(df2)[names(df2)==var] <- "var"
  
  fillz = c("No"="gray70", "Yes" = "skyblue3")
  
  
  p2 <- ggplot(data=df2, aes(var,  Residual)) +
    geom_boxplot(aes(var~Residual))
  
  p1 <- ggplot(data=df1, aes(var, y)) + 
    geom_crossbar(aes(ymin=ylower, ymax=yupper, fill=IsSignificant), 
                  alpha=.4, show.legend = F) +
    #geom_point(aes(x=var, y=y, color=var), size=5) +
    #geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
    scale_fill_manual(values = fillz) +
    geom_hline(aes(yintercept=0), linetype=2) + theme_bw() +
    theme(panel.grid = element_blank(), axis.title.x = element_blank(),
          axis.text = element_text(size=14, angle = 90),
          axis.title.y = element_text(size=18, angle = 90),
          legend.text = element_text(size=10),
          plot.margin = unit(c(.1,.1,.5,1), "cm"))+
    ylab(paste0("partial effect on ", response_var)) 
  
  #print(p1)
  
  return(p1)
}
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
FigS2B <- plot.partial(fly.sex, var = "fly_sex", response_var = "count")
FigS2C <- plot.partial(bat.sex, var = "bat_sex", response_var = "count")
FigS3D <- plot.partial.cont(df = MFR.dat, log=F, var="mass_forearm_residual", response_var = "count", alt_var = "mass: forearm residual", legend.on = F)


gamRmad1 <- gam(count~s(bat_sex, bs="re") + 
                  s(fly_sex, bs="re") + 
                  s(mass_forearm_residual, bs="tp")+
                  s(doy, bs="cc", k=7, by=bat_sex) + s(year, bs="re"), 
                data = subset(afa, bat_species == "Rousettus madagascariensis"), family = poisson)
summary(gamRmad1)
#only seasonality of bat sex significant

fly.sexR <- get_partial_effects(fit=gamRmad1, var="fly_sex")
bat.sexR <- get_partial_effects(fit=gamRmad1, var="bat_sex")
MFR.datR <- get_partial_effects_continuous(gamFit = gamRmad1, var="mass_forearm_residual")

FigS2F <- plot.partial(fly.sexR, var = "fly_sex", response_var = "count")
FigS2G <- plot.partial(bat.sexR, var = "bat_sex", response_var = "count")
FigS3H <- plot.partial.cont(df = MFR.datR, log=F, var="mass_forearm_residual", response_var = "count", alt_var = "mass: forearm residual", legend.on = F)

# and the seasonal plots


#and now the actual plot for the main text
EID.east.sub <- cbind.data.frame(doy=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
EID.east.sub$year = 2019
EID.east.sub$mass_forearm_residual = 0
EID.east.sub$fly_sex = "male"
EID.east.sub$predicted_count <- predict.gam(gamEdup1,type="response", newdata = EID.east.sub, exclude=c("mass_forearm_residual", "year", "bat_sex", "fly_sex"))
EID.east.sub$predicted_count_SE <- predict.gam(gamEdup1,type="response", newdata = EID.east.sub, exclude=c("mass_forearm_residual", "year", "bat_sex", "fly_sex"), se.fit = T)$se.fit
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
seas.dat$fly_sex = "male"
seas.dat2 <- seas.dat
seas.dat2$fly_sex = "female"
seas.dat <- rbind(seas.dat, seas.dat2)

Eall<-ggplot(data = subset(afa, bat_species == "Eidolon dupreanum")) + facet_grid(~bat_sex) +
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf, fill=bat_sex),alpha=0.3, show.legend = F)+
  geom_point(aes(x= as.numeric(doy), y=count, color=fly_sex), alpha=.3, show.legend = F)+
  scale_fill_manual(values=c("pink", "cornflowerblue")) +
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  #xlab ("Day of the Year")+ 
  ylab(bquote('count of'~italic('Cyclopodia dubia')))+
  #ggtitle("host male")+ 
  geom_ribbon(data = EID.east.sub, aes(x=doy, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = EID.east.sub, aes(x=doy, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        strip.text = element_text(size=16),
        plot.margin = unit(c(.2,.2,.6,.6), "cm"),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank())+
  scale_x_continuous(breaks=c(60,152,244,335), labels=c("Mar", "Jun", "Sep", "Dec"));Eall


pEid <- cowplot::plot_grid(Eall, FigS3D, ncol=2, nrow=1, labels = c("A", "B"), label_size = 22, label_x = c(-0.01,0), rel_widths = c(2,1))


#and Rousettus
ROU.east.sub <- cbind.data.frame(doy=rep(1:365, 2), bat_sex = rep(c("female", "male"), each=365))
ROU.east.sub$year = 2019
ROU.east.sub$mass_forearm_residual = 0
ROU.east.sub$fly_sex = "male"
ROU.east.sub$predicted_count <- predict.gam(gamRmad1,type="response", newdata = ROU.east.sub, exclude=c("mass_forearm_residual", "year", "bat_sex", "fly_sex"))
ROU.east.sub$predicted_count_SE <- predict.gam(gamRmad1,type="response", newdata = ROU.east.sub, exclude=c("mass_forearm_residual", "year", "bat_sex", "fly_sex"), se.fit = T)$se.fit
ROU.east.sub$predicted_count_lci <- ROU.east.sub$predicted_count -1.96*ROU.east.sub$predicted_count_SE
ROU.east.sub$predicted_count_uci <- ROU.east.sub$predicted_count +1.96*ROU.east.sub$predicted_count_SE
ROU.east.sub$predicted_count_lci[ROU.east.sub$predicted_count_lci<0] <- 0#now add the predictions to each dataframe
ROU.east.sub$predicted_count[ROU.east.sub$predicted_count<0] <- 0

seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))
seas.dat$bat_sex="male"
preg.dat <- cbind.data.frame(x = c(yday("2014-09-11"), yday("2019-12-12")))
preg.dat$xlab = "F"
preg.dat$bat_sex="female"
seas.dat <- rbind(seas.dat, preg.dat)
seas.dat$fly_sex = "male"
seas.dat2 <- seas.dat
seas.dat2$fly_sex = "female"
seas.dat <- rbind(seas.dat, seas.dat2)


Rall<-ggplot(data = subset(afa, bat_species == "Rousettus madagascariensis")) + facet_grid(~bat_sex) +
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf, fill=bat_sex),alpha=0.3, show.legend = F)+
  geom_point(aes(x= as.numeric(doy), y=count, color=fly_sex), alpha=.3, show.legend = F)+
  scale_fill_manual(values=c("pink", "cornflowerblue")) +
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  #xlab ("Day of the Year")+ 
  ylab(bquote('count of'~italic('Eucampsipoda madagascariensis')))+
  #ggtitle("host male")+ 
  geom_ribbon(data = ROU.east.sub, aes(x=doy, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROU.east.sub, aes(x=doy, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        strip.text = element_text(size=16),
        plot.margin = unit(c(.2,.2,.6,.6), "cm"),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank())+
  scale_x_continuous(breaks=c(60,152,244,335), labels=c("Mar", "Jun", "Sep", "Dec"));Rall


pRou <- cowplot::plot_grid(Rall, FigS3H,ncol=2, nrow=1, labels = c("C", "D"), label_size = 22, label_x = c(-0.01,0), label_y = c(1,03,1.03), rel_widths = c(2,1))

#and just A and B
pFigS2 <- cowplot::plot_grid(pEid, pRou, ncol=1, nrow = 2)

ggsave(file =  paste0(homewd,"/final-figures/FigS2.png"),
       plot = pFigS2,
       units="mm",  
       width=110, 
       height=75, 
       scale=3, 
       dpi=300)

