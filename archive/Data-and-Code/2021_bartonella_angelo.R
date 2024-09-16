######################################
#Clear work environment
rm(list=ls())

#Set working directory - add your working directory here
homewd= "/Users/carabrook/Developer/Mada-Ectoparasites"
#Angelo-- add your working directory here and add a "#" before my working directory above

#Place individual log files within this folder
setwd(homewd)

# Packages to use 
library(dplyr)  
library(mgcv)
library(lme4)
library(ggplot2)
library(lubridate)

# Data importation
data1<-read.csv(paste0(homewd,"/Data-and-Code/Angelo_catch_dat_bart_age_2_13_2021.csv"),header = T,sep = ",")
names(data1)[names(data1)=="collection_date"] <- "processing_date"


head(data1,2)


bart_dat<-select(data1,roost_site,processing_date,bat_species,sampleid,sample_type,bartonella_precense_absence,
                 age_by_tooth,bat_sex,bat_age_class,bat_weight_g,body_length_cm,bat_forearm_mm,
                 bat_tibia_mm,ear_length_mm,gonad_length_mm,gonad_width_mm,bat_flies,meglastreblidae,
                 fleas,mites,ticks)

str(bart_dat)


bart_dat$processing_date<-dmy(bart_dat$processing_date)
bart_dat$yday<-yday(bart_dat$processing_date)
class(bart_dat$processing_date)

str(bart_dat$yday)
bart_dat$bat_weight_g<-as.numeric(bart_dat$bat_weight_g)
bart_dat$body_length_cm<-as.numeric(bart_dat$body_length_cm)
bart_dat$ear_length_mm<-as.numeric(bart_dat$ear_length_mm)
bart_dat$gonad_length_mm<-as.numeric(bart_dat$gonad_length_mm)
bart_dat$gonad_width_mm<-as.numeric(bart_dat$gonad_width_mm)
bart_dat$bat_flies<-as.numeric(bart_dat$bat_flies)
bart_dat$meglastreblidae<-as.numeric(bart_dat$meglastreblidae)
bart_dat$fleas<-as.numeric(bart_dat$fleas)
bart_dat$mites<-as.numeric(bart_dat$mites)
bart_dat$ticks<-as.numeric(bart_dat$ticks)
bart_dat$yday<-yday(bart_dat$processing_date)

#'let's start with EIDOLON DUPREANUM

#'first step:: filtre the data and clean the one that we need

eid_bart_dat<-subset(bart_dat, bat_species=="Rousettus madagascariensis")
eid_bart_dat<-subset(eid_bart_dat, bartonella_precense_absence!="NA")

eid_bart_dat<-subset(eid_bart_dat,bat_sex!="unknown"&bat_weight_g!="NA")
eid_bart_dat<-subset(eid_bart_dat,bat_age_class!="J")
unique(eid_bart_dat$bat_species)
str(eid_bart_dat$yday)

head(eid_bart_dat)


plot(eid_bart_dat$yday,eid_bart_dat$bartonella_precense_absence)

eid_bart_dat$bartonella_precense_absence<-as.numeric(eid_bart_dat$bartonella_precense_absence)


p4<-ggplot(eid_bart_dat,aes(as.factor(bartonella_precense_absence),bat_forearm_mm,fill=as.factor(bartonella_precense_absence)))+
  geom_violin()+
  geom_point()+
  theme_bw()
  
p4

mod2<-gam(bartonella_precense_absence~s(bat_forearm_mm,k=7,bs="cc"), data = eid_bart_dat)
print(mod2)
plot(mod2)
summary(mod2)
par(mfrow=c(2,2))
gam.check(mod2)
dev.off()
coef(mod2)
# add model projections to dataset
eid_bart_dat$predicted_prf<- predict.gam(mod2, type="response", se.fit = T)$fit
eid_bart_dat$predicted_count_SE <- predict.gam(mod2, type="response", se.fit = T)$se.fit

eid_bart_dat$predicted_count_lci <- eid_bart_dat$predicted_prf -1.96*eid_bart_dat$predicted_count_SE

eid_bart_dat$predicted_count_uci <- eid_bart_dat$predicted_prf +1.96*eid_bart_dat$predicted_count_SE
eid_bart_dat$predicted_count_lci[eid_bart_dat$predicted_count_lci<0] <- 0


p2 <- ggplot(data=eid_bart_dat) + 
  geom_point ( aes(x=bat_forearm_mm, y=bartonella_precense_absence),shape=19,color="grey70",size=2) + xlab("Bat forearm length (mm)") +
  ylab("Bartonella infection") +
  ggtitle("R.M ") +
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=14, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12) ) + 
  geom_line(aes(x=bat_forearm_mm, y=predicted_prf), color="green", size=1) +
  geom_ribbon(aes(x=bat_forearm_mm, ymin=predicted_count_lci, ymax=predicted_count_uci), fill="green", size=1, alpha=.3)
print(p2)



##############################################



mod1<-glm(bartonella_precense_absence~as.numeric(bat_forearm_mm),data = eid_bart_dat,family = "binomial")
mod1
summary(mod1)
eid_bart_dat$predicted_prf<- predict.glm(mod1, type="response", se.fit = T)$fit
eid_bart_dat$predicted_count_SE <- predict.glm(mod1, type="response", se.fit = T)$se.fit
eid_bart_dat$predicted_count_lci <- eid_bart_dat$predicted_prf -1.96*eid_bart_dat$predicted_count_SE
eid_bart_dat$predicted_count_uci <- eid_bart_dat$predicted_prf +1.96*eid_bart_dat$predicted_count_SE
eid_bart_dat$predicted_count_lci[eid_bart_dat$predicted_count_lci<0] <- 0


p1 <- ggplot(data=eid_bart_dat) + 
  geom_point ( aes(x=as.numeric(bat_forearm_mm), y=bartonella_precense_absence),shape=19,color="grey70",size=2) + xlab("bat weight (g)") +
  ylab("Infection rate") +
  ggtitle("R. madagascariensis bartonella infection in both sex") +
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=14, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12) ) + 
  geom_line(aes(x=as.numeric(bat_forearm_mm), y=predicted_prf), color="green", size=1)+
  geom_ribbon(aes(x=as.numeric(bat_forearm_mm), ymin=predicted_count_lci, ymax=predicted_count_uci), fill="green", size=1, alpha=.3)

print(p1)


boxplot(eid_bart_dat$bartonella_precense_absence~eid_bart_dat$bat_sex)

### to see if the sex hove a relation with the presence or abscence of bartonella

chisq.test(eid_bart_dat$bartonella_precense_absence,eid_bart_dat$bat_sex)
  #'this test show that there is no interaction between the presence of bartonella and the sex of the host


############################################"
par(mfrow=c(2,2))

mod3<-glm(bartonella_precense_absence~bat_age_class+as.numeric(bat_weight_g)+bat_sex+bat_forearm_mm,data = eid_bart_dat,family = "binomial")
mod3
summary(mod3)

mod4<-glm(bartonella_precense_absence~bat_sex*as.numeric(bat_weight_g)*as.numeric(bat_forearm_mm),data = eid_bart_dat,family = "binomial")
summary(mod4)

mod5<-glm(bartonella_precense_absence~as.numeric(bat_forearm_mm),family = "binomial",data=eid_bart_dat)

summary(mod5)
plot(mod5)



#######################################""
anova(mod1,mod2,mod3,mod4)


#W(t)=W(1-exp(-k*(t-t0)))



########################################################################################################################################

#Data that I have
#' presence abcence of bartonella (binomial)
#' Bat weight
#' Bat forearm length
#' Bat sex
#' Bat ectoparasites
#' Ectoparasite abundance

#'Question: 
#'1-Does the bats length, bat weigth affect the infection of bartonella?
       #'bartonella~s(bat weight)+s(bat forearm)+ random=(1|sex)
       #'the season as well could be considerde as random effecte 
       

#'2-Does the bartonella infection change in both sex?
       #'comparaison of the presence of batronella in bof sex 
       #'Chi square
      
#'3-Does the abundance of the ectoparasite for its species affect the infection of bartonella?
      #'bartonella~abundance of cyclopodia+mite+ticks+others+ season

#############################################################################



#' LETS TRY TO ANSWER THOSE QUESTION ONE BY ONE AND PLOT THEM
#' CASE OF Eidolon dupreanum
eid_bart_dat # subset of the data that we need for 
str(eid_bart_dat)

eid_bart_dat_cor<-select(eid_bart_dat,bartonella_precense_absence,bat_weight_g,
                         bat_forearm_mm,bat_sex,bat_flies,yday)

pairs(eid_bart_dat_cor,panel = panel.smooth)

library(psych)
pairs.panels(eid_bart_dat_cor) # mampiseho ny sary na auto correlation entre ny variable

#'1-Does the bats length, bat weigth affect the infection of bartonella?
    #'bartonella~s(bat weight)+s(bat forearm)+ random=(1|sex)
    #'the season as well could be considerde as random effecte 
library(lme4)
bart_mod1<-glm(bartonella_precense_absence~bat_weight_g+bat_forearm_mm+bat_sex ,data=eid_bart_dat)
plot(bart_mod1)
summary(bart_mod1)
bart_mod2<-glm(bartonella_precense_absence~bat_weight_g+bat_forearm_mm ,data=eid_bart_dat)
summary(bart_mod2)
bart_mod3<-glm(bartonella_precense_absence~bat_weight_g,data=eid_bart_dat)
summary(bart_mod3)

#######################################################################
#######################################################################
#                 ECTOS MERGE WITH THE CAPTURED DATA                  #
#                                                                     #
#######################################################################
#ectos<-read.csv("ectos.csv")
ectos <- read.csv("20200628_AA_SG_Ectoparasite_Species.csv", header=T)
names(ectos)[names(ectos)=="Bat_SampleID"] <- c("sampleid") 
efa<-merge(ectos,data1,by="sampleid")
str(efa)
efa<-select(efa,roost_site,processing_date,bat_species,sampleid,sample_type,bartonella_precense_absence,
                 age_by_tooth,bat_sex,bat_age_class,bat_weight_g,body_length_cm,bat_forearm_mm,
                 bat_tibia_mm,ear_length_mm,gonad_length_mm,gonad_width_mm,
                 bat_flies,meglastreblidae,fleas,mites,ticks,
                Ecto_species,Ecto_ID,Count,Male,Female)


efa$processing_date<-dmy(efa$processing_date)
class(efa$processing_date)

str(efa$processing_date)
efa$bat_weight_g<-as.numeric(efa$bat_weight_g)
efa$body_length_cm<-as.numeric(efa$body_length_cm)
efa$ear_length_mm<-as.numeric(efa$ear_length_mm)
efa$gonad_length_mm<-as.numeric(efa$gonad_length_mm)
efa$gonad_width_mm<-as.numeric(efa$gonad_width_mm)
efa$Count<-as.numeric(efa$Count)
efa$Male<-as.numeric(efa$Male)
efa$Female<-as.numeric(efa$Female)
efa$yday<-yday(efa$processing_date)
unique(efa$roost_site)

unique(efa$Ecto_species)


efa_eid<-subset(efa, bat_species=="Rousettus madagascariensis" & Ecto_species=="Eucampsipoda_madagascariensis")
efa_eid$bat_flies<-as.numeric(efa_eid$bat_flies)

efa1<-select(efa_eid,Ecto_species,Count,bat_flies )
efa1<-na.omit(efa1)
mean_field<-mean(efa1$bat_flies[efa1$Ecto_species=="Eucampsipoda_madagascariensis"])
mean_lab<-mean(efa1$Count[efa1$Ecto_species=="Eucampsipoda_madagascariensis"])

sd_field<-sd(efa1$bat_flies[efa1$Ecto_species=="Eucampsipoda_madagascariensis"])
sd_lab<-sd(efa1$Count[efa1$Ecto_species=="Eucampsipoda_madagascariensis"])

mean_lab-mean_field
sd_dif<-sd_lab-sd_field

mydata<-data.frame(samples=c("Field","Lab"),
                   moyen=c(mean_field,mean_lab),
                   sd_lps=c(sd_field,sd_lab))


a<-c(2,2,4,6,6,8)
b<-c(2,2,4,6,6,8)

plot(abline(lm(a~b)))
mean(a)-mean(b)



sg<-ggplot(data.frame(mydata), aes(x=samples , y=moyen)) + 
  geom_errorbar(aes(ymin=moyen-sd_lps, ymax=moyen+sd_lps), width=.2) +
  geom_point(colour="green",shape=19,size=5)+     # shape=manamboatra ny paozin'ilay point
  theme(panel.background= element_rect(fill = "white", colour = "black"),
        axis.text.x = element_text(size=12,colour = "black"),  #mananboatra ny police  ny axec des "x"
        axis.title.x = element_text(size = 15),                #Manamboatra ny label ny axce des "x" 
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size = 15),
        axis.line = element_line(colour = "black", 
                                 size = 0.7, linetype = "solid"))+
  labs(title="Eucampsipoda: Field Vs LabID", # Mametraka ny Label (izay tiana hatao) sy ny titre
       x="",
       y="Mean of the number of the ectos")

sg

###MANAMBOAtRA NY DATA CORRELATION PLOT MIARAKA AMIN4NY ABLINE
# Tokony asiana poligone ny Sur estimate sy ny Under estimate
library(tidyverse)
library(dplyr)




ggplot(data.frame(efa1), aes(x= Count, y=bat_flies)) + 
  geom_abline(colour="green",size=1)+
  geom_point(colour="grey",shape=19,size=2)+
  theme(panel.background= element_rect(fill = "white", colour = "black"),
        axis.text.x = element_text(size=12,colour = "black"),  #mananboatra ny police  ny axec des "x"
        axis.title.x = element_text(size = 15),                #Manamboatra ny label ny axce des "x" 
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size = 15),
        axis.line = element_line(colour = "black", 
                                 size = 0.7, linetype = "solid"))+
  labs(title="Eucampsipoda: Field Vs LabID", # Mametraka ny Label (izay tiana hatao) sy ny titre
       x="Count in the Lab",
       y="Number from the Field")

cor.test(efa1$Count,efa1$bat_flies)


efa_eid<-subset(efa_eid,roost_site!="Mahabo"| roost_site!="Makira")

efa_eid$bartonella_precense_absence



###Develop the gam model
efa_eid_mod<-gam(bartonella_precense_absence~s(yday,k=7,bs="cc"),data=efa_eid)
summary(efa_eid_mod)
gam.check(efa_eid_mod)
plot(efa_eid_mod)

####Extract the fitied value and the SE (lower and upper) then I will add theme in the data set

efa_eid$prediction<-predict.gam(efa_eid_mod,type = "response",se.fit = T)$fit
efa_eid$predicted_count_SE <- predict.gam(efa_eid_mod, type="response", se.fit = T)$se.fit
efa_eid$predicted_count_lci <- efa_eid$prediction -1.96*efa_eid$predicted_count_SE
efa_eid$predicted_count_uci <- efa_eid$prediction +1.96*efa_eid$predicted_count_SE
efa_eid$predicted_count_lci[efa_eid$predicted_count_lci<0] <- 0

p2 <- ggplot(data=efa_eid) + 
  geom_point ( aes(x=yday, y=Count),shape=19,color="grey70",size=2) + xlab("Day of year from january 1st") +
  ylab("count") +
  ggtitle("E. dupreanum bf") +
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=14, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12) ) + 
  geom_line(aes(x=yday, y=prediction), color="orange", size=1) +
  geom_ribbon(aes(x=yday, ymin=predicted_count_lci, ymax=predicted_count_uci), fill="orange", size=1, alpha=.3)
print(p2)



#####################


dat_bart<-subset(data1, bartonella_precense_absence!="NA")
dat_bart$bartonella_precense_absence

