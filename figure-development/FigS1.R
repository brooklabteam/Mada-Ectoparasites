######################################
#Clear work environment
rm(list=ls())

library(plyr)
library(dplyr)
library(ggplot2)

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

cap <- read.csv(file= paste0(homewd, "/data/ecto_dat_long.csv"), header = T, stringsAsFactors = F)
head(ectos)
head(cap,2)
names(ectos)
names(cap)

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

afa<-merge(ectos, cap, by = "sampleid",all=T)

head(afa)
names(afa)

afa = subset(afa, !is.na(bat_sex) & bat_sex!="unknown")
# 
# #what is the date range of those that were looked at under the scope?
# unique(afa$Ecto_type)
# afa$processing_date <- as.Date(afa$processing_date, format = "%m/%d/%y")
# min(afa$processing_date[!is.na(afa$Ecto_type)], na.rm = T) #2018-02-14
# max(afa$processing_date[!is.na(afa$Ecto_type)], na.rm = T) #2019-11-30
# length(unique(afa$sampleid[!is.na(afa$Ecto_type)])) #818
# 
# unique(afa$Ecto_type[afa$processing_date>="2018-02-14" & afa$processing_date<="2019-11-30"])
# missed <- subset(afa, is.na(Ecto_type) & processing_date>="2018-02-14" & processing_date<="2019-11-30")
# complete <- subset(afa, !is.na(Ecto_type) & processing_date>="2018-02-14" & processing_date<="2019-11-30")
# 
# length(missed$sampleid[missed$total_ectos==0]) #100 were 0s which is why they are not included in the subset
# length(missed$sampleid[missed$total_ectos!=0]) #99 were not assessed
# 
# never.assessed = subset(missed, total_ectos!=0)
# write.csv(never.assessed, file = paste0(homewd,"/data/never-assessed.csv"), row.names = F)
# unique(missed$bat_flies)
# unique(missed$roost_site)
# min(missed$processing_date) # "2018-02-19"
# max(missed$processing_date)# "2019-11-16"
# sort(missed$sampleid)
# 
# sort(unique(missed$sampling_session))
# sort(unique(complete$sampling_session))
# 
# setdiff(unique(missed$sampling_session), unique(complete$sampling_session)) #three sessions totally missed. otherwise ok
# 
# unique(afa$Ecto_type[afa$sampling_session==73])
# afa[is.na(afa$Ecto_type) & afa$sampling_session==73,]
# subset(afa, sampling_session==73)
# subset(afa, sampleid=="ROU106")
# Data CHOICE
# Count is the number of Cyclopodia male+female
# Male and Female= numbers of the cyclopodia Male and Female
data1 <- dplyr::select(afa, roost_site, processing_date,
                       bat_species, bat_sex, 
                       bat_age_class, bat_weight_g,
                       sampleid,
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
# how ever i should leave NA for the number of ectoparasites in each sex


data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
data1$ month<-as.numeric(data1$ month)
data1$year<-as.numeric(data1$year)
data1$day<-as.numeric(data1$day)
data1$Count<-as.numeric(data1$Count)
data1$bat_flies<-as.numeric(data1$bat_flies)
unique(data1$Ecto_species)

length(unique(data1$sampleid[!is.na(data1$Ecto_type) & data1$bat_species=="Eidolon dupreanum"]))#351
length(unique(data1$sampleid[!is.na(data1$Ecto_type) & data1$bat_species=="Eidolon dupreanum" & data1$bat_sex=="male"]))#137
length(unique(data1$sampleid[!is.na(data1$Ecto_type) & data1$bat_species=="Eidolon dupreanum" & data1$bat_sex=="female"]))#214

length(unique(data1$sampleid[!is.na(data1$Ecto_type) & data1$bat_species=="Rousettus madagascariensis"]))#467
length(unique(data1$sampleid[!is.na(data1$Ecto_type) & data1$bat_species=="Rousettus madagascariensis" & data1$bat_sex=="male"]))#241
length(unique(data1$sampleid[!is.na(data1$Ecto_type) & data1$bat_species=="Rousettus madagascariensis" & data1$bat_sex=="female"]))#232

#how many coinfected?
data2 =  dplyr::select(data1, sampleid, bat_species, bat_flies, meglastreblidae, fleas, mites, ticks)
data2[is.na(data2)] <- 0
library(reshape2)
data2 <- melt(data2)
head(data2)

data2 <- data2[!duplicated(data2),]
data2 = subset(data2, sampleid!="MIZ549") #repeat
data.split <- dlply(data2,.(sampleid))

get.coinf <- function(df){
  df1 <- dplyr::select(df[1,], sampleid, bat_species)
  df1$N_type <- length(df$sampleid[df$value>0])
  return(df1)
  
}

data2.df <- data.table::rbindlist(lapply(data.split, get.coinf))

out <- ddply(data2.df, .(bat_species), summarise, N= mean(N_type), sd=sd(N_type), sample = length(unique(sampleid)))
out$se <- out$sd/sqrt(out$sample)
out$lci = out$N-1.96*out$se
out$uci = out$N+1.96*out$se

ECD<-data1%>%
  filter(bat_species=="Eidolon dupreanum",
         !is.na(Ecto_species))


head(ECD)
unique(ECD$Ecto_genus)
unique(ECD$Ecto_species)
tail(ECD,2)


cor.test(ECD$Count[ECD$Ecto_genus=="Cyclopodia"],ECD$bat_flies[ECD$Ecto_genus=="Cyclopodia"])
p1 <- ggplot(data=subset(ECD,Ecto_genus=="Cyclopodia"))+
  geom_point(aes(x=Count,y=bat_flies),size=3, alpha=.2,color="red")+
  labs(title="Cyclopodia dubia",x="Recount in the lab",y= "Field count")+
  geom_text(aes(x=5, y=17, label="r=0.92,p<0.001***"),col="grey0", size=5)+
  theme_bw()+
  theme(axis.text = element_text(size=12),
        plot.title = element_text(face = "italic"),
        axis.title = element_text(size=14)); p1


REM<-data1%>%
  filter(bat_species=="Rousettus madagascariensis",
         !is.na(Ecto_species))

head(REM)
unique(REM$Ecto_genus)


cor.test(REM$Count[REM$Ecto_genus=="Eucampsipoda"],REM$bat_flies[REM$Ecto_genus=="Eucampsipoda"])
p2 <- ggplot(data=subset(REM,Ecto_genus=="Eucampsipoda"))+
  geom_point(aes(x=Count,y=bat_flies),size=3, alpha=.2,color="red")+
  geom_text(aes(x=8, y=36, label="r=0.91,p<0.001***"),col="grey0", size=5)+
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


ggsave(file = paste0(homewd, "/final-figures/FigS1.pdf"),
       plot = FigS1,
       units="mm",  
       width=90, 
       height=55, 
       scale=2.5, 
       dpi=300)



#Megastrebla is poorly retained from one to the other, so we do not examine its seasonality in the dataset
cor.test(REM$Count[REM$Ecto_genus=="Megastrebla"],REM$bat_flies[REM$Ecto_genus=="Megastrebla"])
p3 <- ggplot(data=subset(REM,Ecto_genus=="Megastrebla"))+
  geom_point(aes(x=Count,y=bat_flies),size=3, alpha=.2,color="red")+
  geom_text(aes(x=8, y=36, label="r=0"),col="grey0", size=5)+
  labs(title="Megastrebla wenzeli",x="Recount in the lab",y= "Field count")+
  theme_bw()+
  theme(axis.text = element_text(size=12),
        plot.title = element_text(face = "italic"),
        axis.title = element_text(size=14)); p3
