######################################
#Clear work environment
rm(list=ls())

#Set working directory - add your working directory here
homewd= "/Users/theresalaverty/Documents/R/R_repositories"
#Angelo-- add your working directory here and add a "#" before my working directory above

#Place individual log files within this folder
setwd(paste0(homewd,"/Mada-Ectoparasites/Data-and-Code/"))

# Packages to use 
library(tidyverse)
library(readr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(ggplot2)
library(gratia)

# Data importation
ectos <- read_csv("20200628_AA_SG_Ectoparasite_Species.csv")
cap <- read.csv(file= paste0(homewd, "/Madagascar-Bat-Data/Catching-Data/catching_data.csv"), header = T, stringsAsFactors = F)



head(ectos)
 head(cap,2)

#MERGE the two data sets from the "SampleID" colum

# 1) read in both datasets with:  datasetName <- read.csv("datasetName.csv")
# 2) check colnames of both with: colnames(datasetName)
# 3) if my ID column has different names, I change them to match with: 
  
  # colnames(Ectos)[IDcolumnNumber] <- c("IDcolumnName")
  names(ectos)[names(ectos)=="Bat_SampleID"] <- c("sampleid") 
  
# 4) once you have matching column names, you can use the merge function
  
afa<-merge(ectos, cap, by = "sampleid",all=T)

View(afa)

# Data CHOICE
# Count is the number of Cyclopodia male+female
# Male and Female= numbers of the cyclopodia Male and Female
data1 <- dplyr::select(afa, roost_site, processing_date,
                       bat_species, bat_sex, 
                       bat_age_class, bat_weight_g,
                       body_length_cm, 
                       bat_forearm_mm, bat_tibia_mm,
                       ear_length_mm, gonad_length_mm, 
                       gonad_width_mm, Ecto_species,
                       Count, Male, Female)

data1<-dplyr::mutate(afa,Daty=processing_date)
data1$processing_date <- as.Date(data1$processing_date, format ="%m/%d/%y")
data1$Daty <- as.Date(data1$Daty, format = "%m/%d/%y")
data1<-data_sep<-separate(data1, col = Daty, into = c("year", "month","day"), sep = "-")

View(data1)
head(data1,2)
tail(data1)
str(data1)

data1$Count[is.na(data1$Count)]<-0  # I change NA to O because that means there is no ectoparasite in the individual
# how ever i should let NA for the number of ectoparasites in each sex


data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
data1$ body_length_cm<-as.numeric(data1$ body_length_cm)
data1$ ear_length_mm<-as.numeric(data1$ ear_length_mm)
data1$ gonad_length_mm<-as.numeric(data1$ gonad_length_mm)
data1$ gonad_width_mm<-as.numeric(data1$ gonad_width_mm)
data1$ month<-as.numeric(data1$ month)
data1$year<-as.numeric(data1$year)
data1$day<-as.numeric(data1$day)

ECD<-data1%>%
  filter(bat_species=="Eidolon dupreanum",
         Ecto_species!="")


View(ECD)
str(ECD)
tail(ECD,2)


#




# GAMMMM
  
# library(mgcv)
# library(mgcViz)

#Model-1 Model fot the ectoparasite in general
modl<-gam(Count~s(month,k=9, bs="cs"),data=ECD)
summary(modl)
getViz(modl)
#HINT-CEB: Set up panel for 4 plots at once before instituting 'gam.check'
par(mfrow=c(2,2))
gam.check(modl)
dev.off()
print(plot(modl),ask=FALSE)



draw(modl)
predict(modl,type="terms")
#HINT-CEB: If you want to get the predicted values of the data instead of the model residuals, do the following,
#and attach to the original data set

ECD$predicted_count <- predict.gam(modl,type="response")

#and to add in the confidence intervals
ECD$predicted_count_SE <- predict.gam(modl,type="response", se.fit = T)$se.fit
ECD$predicted_count_lci <- ECD$predicted_count -1.96*ECD$predicted_count_SE
ECD$predicted_count_uci <- ECD$predicted_count +1.96*ECD$predicted_count_SE
ECD$predicted_count_lci[ECD$predicted_count_lci<0] <- 0


#HINT-CEB: Here are the raw data plotted with the fitted line
ggplot(data=ECD) + geom_point(aes(x=month, y=Count)) +
                  geom_line(aes(x=month, y=predicted_count)) + 
                  geom_ribbon(aes(x=month, ymin=predicted_count_lci, ymax=predicted_count_uci), alpha=.3)


#HINT-CEB: not sure what you are doing here???
p1<-ggplot(data = ECD) +
  geom_point(mapping = aes(x =month , y = Female)) +
  geom_smooth(mapping = aes(x = month , y = Female),method = "REML") +
  coord_cartesian( ylim = c(0,5 ))+labs(title="B",x="Month of collection", y=" Abundance of the ectoparasite")
p1+theme(panel.background= element_rect(fill = "white", colour = "black"),
                                           axis.text.x = element_text(size=12,colour = "black"),
                                           axis.title.x = element_text(size = 12),
                                           axis.text.y = element_text(size=12, colour = "black"),
                                           axis.title.y = element_text(size = 12),
                                           axis.line = element_line(colour = "black", 
                                                                    size = 0.7, linetype = "solid"))


p1

##HINT-CEB: your base plot looks nice, but remember it is NOT the "Abundance of the ectoparasites" 
#on the y-axis but instead the residual from the fitted gam. Abundance will only ever be 0 or higher

## Base plot
plot(modl,shade=T,residual=T,xaxt="none", yaxt="none", xlab="",ylab="", ylim=c(-5,10),xlim=c(1,12),pch=21)
## change axis labels
axis(1, seq(0,30, 1),las=1, font=1,cex.axis=1)
axis(2, seq(-5,10, 5),las=1, font=1,cex.axis=1)

## Xlab, ylab, main lab
mtext(side=1, line=2, "Month of the collection", col="black", font=1,cex=1.5)
mtext(side=2, line=3, "Abundance of the ectoparasites", col="black", font=1, cex=1.5)
mtext(side=3, line=0.5, "Ectoparasites in general", col="forestgreen", font=3, cex=1)
text(x=11, y=10, " p=0.0041**",col="grey70", font=2, cex=0.8)
## Add the shade for the seasons and the lines separators (abline)
rect(-10, -1000, 4, 1000, col=adjustcolor("green", 0.1))
rect(4, -1000, 10, 1000, col=adjustcolor("orange", 0.1))
rect(10, -1000, 13, 1000, col=adjustcolor("green", 0.1))

abline(h=0,v=c(4,10),lty=4, col="gray")
###################################################################################################

# CYCLOPODIA DUBIA

cd<-data1%>%
  filter(bat_species=="Eidolon dupreanum",
         Ecto_species=="Cyclopodia_dubia")

view(cd)
str(cd)

modcd <- gam(Count~s(month,k=7, bs="cs"), family="poisson",method = "REML", data=cd)
print(modcd)
summary(modcd)
par(mfrow=c(2,2))
gam.check(modcd)
dev.off()
plot(modcd,residual=T,pch=21,shade=T)
plot(modcd,shade=T,residual=T,xaxt="none", yaxt="none", xlab="",ylab="", ylim=c(-2,8),xlim=c(1,12),pch=21)

## change axis labels
axis(1, seq(0,30, 1),las=1, font=1,cex.axis=1)
axis(2, seq(-2,20, 2),las=1, font=1,cex.axis=1)

## Xlab, ylab, main lab
mtext(side=1, line=2, "Month of collection", col="black", font=1,cex=1.5)
mtext(side=2, line=3, "Abundance of the BF", col="black", font=1, cex=1.5)
mtext(side=3, line=0.5, "ED_Cyclopodia (k=6,bs=cc)", col="forestgreen", font=3, cex=1)
text(x=11, y=8, " p<0.001***",col="grey70", font=2, cex=0.8)

## Add the shade for the seasons and the lines separators (abline)
rect(-10, -1000, 4, 1000, col=adjustcolor("green", 0.1))
rect(4, -1000, 10, 1000, col=adjustcolor("orange", 0.1))
rect(10, -1000, 13, 1000, col=adjustcolor("green", 0.1))

abline(h=0,v=c(4,10),lty=4, col="grey")




############"

modcdm <- gam(Male~s(month,k=7, bs="cc"), family="poisson",method = "REML", data=cd)
print(modcdm)
summary(modcdm)
gam.check(modcdm)


plot(modcdm,residual=T,pch=21,shade=T)
plot(modcdm,shade=T,residual=T,xaxt="none", yaxt="none", xlab="",ylab="", ylim=c(-2,8),xlim=c(1,12),pch=21)
## change axis labels
axis(1, seq(0,30, 1),las=1, font=1,cex.axis=1)
axis(2, seq(-2,20, 2),las=1, font=1,cex.axis=1)

## Xlab, ylab, main lab
mtext(side=1, line=2, "Month of collection", col="black", font=1,cex=1.5)
mtext(side=2, line=3, "Abundance of the BF", col="black", font=1, cex=1.5)
mtext(side=3, line=0.5, "ED_Cyclopodia male (k=6,bs=cc)", col="forestgreen", font=3, cex=1)
text(x=11, y=8, " p<0.001***",col="grey70", font=2, cex=0.8)

## Add the shade for the seasons and the lines separators (abline)
rect(-10, -1000, 4, 1000, col=adjustcolor("green", 0.1))
rect(4, -1000, 10, 1000, col=adjustcolor("orange", 0.1))
rect(10, -1000, 13, 1000, col=adjustcolor("green", 0.1))

abline(h=0,v=c(4,10),lty=4, col="grey")


########################################################################################################################
########################################################################################################################

### Hey Angelo, I recommended trying to model the day of year instead of month. Let's do that here, starting from
## the raw data

#there are a handful of sampleIDs in the ecto data that don't match the cacthing. you'll need to edit them manually.
#here is a list right here:
setdiff(ectos$sampleid, cap$sampleid)

#get just cyc dubia on E. dup
unique(ectos$Bat_species)
afa$Bat_species[afa$Bat_species=="Eiodolon_dupreaneum"] <- "Eidolon_dupreanum"
ecto.plot = subset(afa, Ecto_species=="Cyclopodia_dubia" & Bat_species=="Eidolon_dupreanum")

#drop unnecessary columns 
ecto.plot <- dplyr::select(ecto.plot, -(Notes), -(Photo))
head(ecto.plot)
#add in important columns from catching data

#convert date

#ecto.plot$processing_date <- ecto.plot$bat_age_class <- ecto.plot$bat_sex <- ecto.plot$roost_site <- NA

for (i in 1:length(cap$sampleid)){
  ecto.plot$processing_date[ecto.plot$Bat_SampleID==cap$sampleid[i]] <- cap$processing_date[i]
  ecto.plot$bat_age_class[ecto.plot$Bat_SampleID==cap$sampleid[i]] <- cap$bat_age_class[i]
  ecto.plot$bat_sex[ecto.plot$Bat_SampleID==cap$sampleid[i]] <- cap$bat_sex[i]
  ecto.plot$roost_site[ecto.plot$Bat_SampleID==cap$sampleid[i]] <- cap$roost_site[i]
}


ecto.plot$processing_date = as.Date(ecto.plot$processing_date, format = "%m/%d/%y")

#there are a subset of sampleIDs that don't match up that cause NAs in the fill-in here
#I will leave you to figure them out -- they are the NAs in the data


#then, take only the adults
unique(ecto.plot$bat_age_class)
ecto.plot = subset(ecto.plot, bat_age_class!="J" & !is.na(bat_age_class))
#merge on sample 
##
head(ecto.plot)


#now get day of year
library(lubridate)
ecto.plot$yday <- yday(ecto.plot$processing_date)
head(ecto.plot)

#plot just the data
pnew <- ggplot(data=ecto.plot) + geom_point(aes(x=yday, y=Count, color =roost_site)) + xlab("Day of Year") +
        ylab("Cyclopodia dubia Count") +
        theme_bw()+ theme(panel.grid = element_blank())
print(pnew)

#you see why I want to complete the dataset -- 
#the vast majority of our entries from the end of the year are from the Ankarana sites. 
#We need a full year's worth data from one site



#now add in the model
gamnew <- gam(Count~s(yday,k=7, bs="cc"), family="poisson", data=ecto.plot)
print(gamnew)
plot(gamnew)
summary(gamnew)
par(mfrow=c(2,2))
gam.check(gamnew)
dev.off()

#add model projections to dataset
ecto.plot$predicted_count <- predict.gam(gamnew, type="response", se.fit = T)$fit
ecto.plot$predicted_count_SE <- predict.gam(gamnew, type="response", se.fit = T)$se.fit

ecto.plot$predicted_count_lci <- ecto.plot$predicted_count -1.96*ecto.plot$predicted_count_SE
ecto.plot$predicted_count_uci <- ecto.plot$predicted_count +1.96*ecto.plot$predicted_count_SE

ecto.plot$predicted_count_lci[ecto.plot$predicted_count_lci<0] <- 0

#now plot with data
pnew2 <- ggplot(data=ecto.plot) + geom_point(aes(x=yday, y=Count, color =roost_site)) + xlab("Day of Year") +
  ylab("Cyclopodia dubia Count") +
  theme_bw()+ theme(panel.grid = element_blank()) + 
  geom_line(aes(x=yday, y=predicted_count), color="red", size=1) +
  geom_ribbon(aes(x=yday, ymin=predicted_count_lci, ymax=predicted_count_uci), fill="red", size=1, alpha=.3) 
print(pnew2)


ggsave(path= "Figures", file = "CycloDub_Season.png",
       units="mm",  
       width=60, 
       height=40, 
       scale=3, 
       dpi=300)
