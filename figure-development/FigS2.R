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

gamEdup <- gam(count~bat_sex + mass_forearm_residual + fly_sex + 
                 s(doy, bs="cc", k=7, by=bat_sex) + s(year, bs="re"), 
               data = subset(afa, bat_species == "Eidolon dupreanum"))
summary(gamEdup)


gamEdup2 <- gam(count~s(bat_sex, bs="re") + s(fly_sex, bs="re") + 
                 s(doy, bs="cc", k=7, by=bat_sex) + s(year, bs="re"), 
               data = subset(afa, bat_species == "Eidolon dupreanum"))
summary(gamEdup2)
out <- get_partial_effects(fit=gamEdup2, var="fly_sex")
head(out)
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
plot.partial(out, var = "fly_sex", response_var = "count")
