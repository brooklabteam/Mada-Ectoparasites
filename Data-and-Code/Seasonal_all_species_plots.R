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
setwd("D:/EKIPA_FANIHY/Ectoparasite_works/Ecto2")
#ectos <- read.csv("20200628_AA_SG_Ectoparasite_Species.xlsx - ID_Ectoparasites.csv")
cap <- read.csv("mada_catching_bat_AFA_7_9_2020.csv", header = T, stringsAsFactors = F)


# Data CHOICE
# Count is the number of Cyclopodia male+female
# Male and Female= numbers of the cyclopodia Male and Female
data1 <- dplyr::select(cap, roost_site, collection_date,
                       bat_species, bat_sex,sampleID, 
                       bat_age_class, bat_weight_g,
                       body_length_cm, bat_sex,bat_age_class,
                       bat_forearm_mm, bat_tibia_mm,
                       ear_length_mm, gonad_length_mm, 
                       gonad_width_mm,
                       bat_flies,meglastreblidae,
                       fleas,mites,ticks)

data1$collection_date<-mdy(data1$collection_date) # change the format of the collection date as Date using the function ymd (lubridate)
class(data1$collection_date)

data1$yday<-yday(data1$collection_date)
data1$Daty<-data1$collection_date # I make a copy of the collection date and rename the colume as "Daty"

# then I separate the colume date into year,month and day (we may need it later)
data1<-separate(data1, col = Daty, into = c("year", "month","day"), sep = "-")

#Check if every thing is goiing well
View(data1)
head(data1,2)
tail(data1)
str(data1)


# Now I change the class of each variable
data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
data1$ body_length_cm<-as.numeric(data1$ body_length_cm)
data1$ ear_length_mm<-as.numeric(data1$ ear_length_mm)
data1$ gonad_length_mm<-as.numeric(data1$ gonad_length_mm)
data1$ gonad_width_mm<-as.numeric(data1$ gonad_width_mm)
data1$ month<-as.numeric(data1$ month)
data1$ month<-as.factor(data1$ month)
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

EID<-subset(data1,bat_species=="Eidolon dupreanum")
unique(EID$bat_sex)


## I Change the name of the three sites
EID$roost_site[EID$roost_site%in%c("AngavoBe","AngavoKely","Lakato")]<-"east"
EID$roost_site[EID$roost_site%in%c("Ankarana_Cathedral","Ankarana_Canyon" )]<-"north"
unique(EID$roost_site)



EIDM<-subset(EID,bat_sex=="male"&roost_site=="east")
EIDF<-subset(EID,bat_sex=="female"&roost_site=="east")

##### GAM 
gamEIDM <- gam(bat_flies~ s(yday, k=7, bs = "cc"), data = EIDM)
gamEIDF <- gam(bat_flies~ s(yday, k=7, bs = "cc"), data = EIDF)

plot(gamEIDM)

EIDM$predicted_count <- predict.gam(gamEIDM,type="response")
EIDM$predicted_count_SE <- predict.gam(gamEIDM,type="response", se.fit = T)$se.fit
EIDM$predicted_count_lci <- EIDM$predicted_count -1.96*EIDM$predicted_count_SE
EIDM$predicted_count_uci <- EIDM$predicted_count +1.96*EIDM$predicted_count_SE
EIDM$predicted_count_lci[EIDM$predicted_count_lci<0] <- 0#now add the predictions to each dataframe

E2<-ggplot(data = EIDM) + 
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="cornflowerblue", alpha=0.3)+
  geom_point(aes(x= as.numeric(yday), y= bat_flies), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  ylab("Count of Cyclopodia dubia")+
  #ggtitle("host male")+ 
  geom_ribbon(data = EIDM, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = EIDM, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank());E2


EIDF$predicted_count <- predict.gam(gamEIDF,type="response")
#and to add in the confidence intervals
EIDF$predicted_count_SE <- predict.gam(gamEIDF,type="response", se.fit = T)$se.fit
EIDF$predicted_count_lci <- EIDF$predicted_count -1.96*EIDF$predicted_count_SE
EIDF$predicted_count_uci <- EIDF$predicted_count +1.96*EIDF$predicted_count_SE
EIDF$predicted_count_lci[EIDF$predicted_count_lci<0] <- 0#now add the predictions to each dataframe


seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))
preg.dat <- cbind.data.frame(x = c(yday("2014-07-07"), yday("2019-11-16")), bat_species= rep("Eidolon dupreanum", each=2))
preg.dat$xlab = "F"

E1<-ggplot(data = EIDF) + 
  geom_ribbon(data = preg.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="hotpink3", alpha=0.3)+
  geom_point(aes(x= as.numeric(yday), y= bat_flies), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  # ylab("Count of Cyclopodia")+
  #ggtitle("host female")+
  geom_ribbon(data = EIDF, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = EIDF, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  geom_vline(xintercept = 111, color="blue",linetype=2)+
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank());E1




####### ROUSETTUS MADAGASSCARIENSIS ######
seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))
preg.dat <- cbind.data.frame(x = c(yday("2014-09-11"), yday("2019-12-11")), bat_species= rep("Rousettus madagascariensis", each=2))
preg.dat$xlab = "F"


#####EUCAMPSIPODA MADAGASCARIENSIS #####

unique(data1$bat_species)

ROU<-subset(data1,bat_species=="Rousettus madagascariensis")
unique(ROU$bat_sex)




ROUM<-subset(ROU,bat_sex=="male"&roost_site=="Maromizaha")
ROUF<-subset(ROU,bat_sex=="female"&roost_site=="Maromizaha")

##### GAM 
gamROUM <- gam(bat_flies~ s(yday, k=7, bs = "cc"), data = ROUM)
gamROUF <- gam(bat_flies~ s(yday, k=7, bs = "cc"), data = ROUF)

plot(gamROUM)

ROUM$predicted_count <- predict.gam(gamROUM,type="response")
ROUM$predicted_count_SE <- predict.gam(gamROUM,type="response", se.fit = T)$se.fit
ROUM$predicted_count_lci <- ROUM$predicted_count -1.96*ROUM$predicted_count_SE
ROUM$predicted_count_uci <- ROUM$predicted_count +1.96*ROUM$predicted_count_SE
ROUM$predicted_count_lci[ROUM$predicted_count_lci<0] <- 0#now add the predictions to each dataframe

E4<-ggplot(data = ROUM) + 
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="cornflowerblue", alpha=0.3)+
  geom_point(aes(x= as.numeric(yday), y= bat_flies), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  ylab("Count of Eucampsipoda madagascariensis")+
  #ggtitle("host male")+ 
  geom_ribbon(data = ROUM, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROUM, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank());E4


ROUF$predicted_count <- predict.gam(gamROUF,type="response")
#and to add in the confidence intervals
ROUF$predicted_count_SE <- predict.gam(gamROUF,type="response", se.fit = T)$se.fit
ROUF$predicted_count_lci <- ROUF$predicted_count -1.96*ROUF$predicted_count_SE
ROUF$predicted_count_uci <- ROUF$predicted_count +1.96*ROUF$predicted_count_SE
ROUF$predicted_count_lci[ROUF$predicted_count_lci<0] <- 0#now add the predictions to each dataframe



E3<-ggplot(data = ROUF) + 
  geom_ribbon(data = preg.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="hotpink3", alpha=0.3)+
  geom_point(aes(x= as.numeric(yday), y= bat_flies), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  # ylab("Count of Cyclopodia")+
  #ggtitle("host female")+
  geom_ribbon(data = ROUF, aes(x= yday, ymin=predicted_count_lci , ymax=predicted_count_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROUF, aes(x=yday, y=predicted_count),color="red", size=1, show.legend = F)+ 
  geom_vline(xintercept = 111, color="blue",linetype=2)+
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank());E3

######MEGASTREBLIDAE #######
##### GAM 
gamROUMM <- gam(meglastreblidae~ s(yday, k=7, bs = "cc"), data = ROUM)
gamROUFM <- gam(meglastreblidae~ s(yday, k=7, bs = "cc"), data = ROUF)

plot(gamROUMM)

ROUM$predicted_countM <- predict.gam(gamROUMM,type="response")
ROUM$predicted_countM_SE <- predict.gam(gamROUMM,type="response", se.fit = T)$se.fit
ROUM$predicted_countM_lci <- ROUM$predicted_countM -1.96*ROUM$predicted_countM_SE
ROUM$predicted_countM_uci <- ROUM$predicted_countM +1.96*ROUM$predicted_countM_SE
ROUM$predicted_countM_lci[ROUM$predicted_countM_lci<0] <- 0#now add the predictions to each dataframe

E6<-ggplot(data = ROUM) + 
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="cornflowerblue", alpha=0.3)+
  geom_point(aes(x= as.numeric(yday), y= meglastreblidae), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  ylab("Count of Megastrebla wenzeli")+
  #ggtitle("host male")+ 
  geom_ribbon(data = ROUM, aes(x= yday, ymin=predicted_countM_lci , ymax=predicted_countM_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROUM, aes(x=yday, y=predicted_countM),color="red", size=1, show.legend = F)+ 
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank());E6


ROUF$predicted_countM <- predict.gam(gamROUFM,type="response")
#and to add in the confidence intervals
ROUF$predicted_countM_SE <- predict.gam(gamROUFM,type="response", se.fit = T)$se.fit
ROUF$predicted_countM_lci <- ROUF$predicted_countM -1.96*ROUF$predicted_countM_SE
ROUF$predicted_countM_uci <- ROUF$predicted_countM +1.96*ROUF$predicted_countM_SE
ROUF$predicted_countM_lci[ROUF$predicted_countM_lci<0] <- 0#now add the predictions to each dataframe


seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))
preg.dat <- cbind.data.frame(x = c(yday("2014-09-11"), yday("2019-12-12")), bat_species= rep("ROUolon dupreanum", each=2))
preg.dat$xlab = "F"

E5<-ggplot(data = ROUF) + 
  geom_ribbon(data = preg.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="hotpink3", alpha=0.3)+
  geom_point(aes(x= as.numeric(yday), y= meglastreblidae), alpha=.3, show.legend = F)+
  #scale_color_manual(values=ColM)+ 
  #scale_fill_manual(values=ColM)+ 
  #geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Day of the Year")+ 
  # ylab("Count of Cyclopodia")+
  #ggtitle("host female")+
  geom_ribbon(data = ROUF, aes(x= yday, ymin=predicted_countM_lci , ymax=predicted_countM_uci ), fill="black",alpha=.3 ) +
  geom_line(data = ROUF, aes(x=yday, y=predicted_countM),color="red", size=1, show.legend = F)+ 
  geom_vline(xintercept = 111, color="blue",linetype=2)+
  theme_bw()+
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank());E5



cowplot::plot_grid(E2,
          E1,
          E4+theme(axis.title.x = element_blank()),
          E3+theme(axis.title.x = element_blank()),
          E6,
          E5,
          ncol=2, nrow = 3, labels=c("A","B","C","D","E","F"), label_x = c(.1,.1))
ggsave(file =  "20221214__continuse_effect_all.png",
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)


######################### WITH PARTIALL EFFECT


get_partial_resids <- function(gamFit, terms, seWithMean) {
  predType <- ifelse(seWithMean, 'iterms', 'terms')  # Doesn't have much meaning here, but included for consistency with get_partial_preds
  
  linearTerm <- predict(gamFit, type = predType, terms = terms) %>% 
    rowSums()
  
  partialResids <- residuals(gamFit) + linearTerm     # TODO: unclear if using deviance residuals is the best idea (differs from plot.gam)
  
  partialResids
}
get_partial_preds <- function(gamFit, newdata, terms, seWithMean) {
  predType <- ifelse(seWithMean, 'iterms', 'terms')
  
  predict(gamFit, newdata = newdata, se.fit = T,
          type = predType, terms = terms) %>% 
    lapply(rowSums) %>% 
    as.data.frame() %>% 
    rename(y = fit, se = se.fit) %>% 
    mutate(ylower = y - 1.96*se,
           yupper = y + 1.96*se)
}
get_partial_effects_interaction <- function(gamFit, var1, var2, seWithMean = TRUE, fixedEffect = FALSE) {
  ## Term names: wrap in s():
  if (!is.null(var2)) {
    if (fixedEffect) stop('Non-smooth interactions not implemented')
    
    termnames <- c(paste0('s(', var1, ',', var2, ')'))
    
  } else {
    if (!fixedEffect) {
      termnames <- paste0('s(', var1, ')')
    } else {
      termnames <- var1
    }
  }
  
  
  ## Variables not part of the effect / interaction are kept constant:
  modelData <- gamFit$model
  responseIndex <- attr(modelData, 'terms') %>% attr('response')
  responseName <- colnames(modelData)[responseIndex]
  
  otherData <- modelData %>% 
    select(-one_of(responseName, var1, var2))
  
  numericData <- otherData %>% 
    summarise_if(is.numeric, ~ median(.))
  
  factorData <- otherData %>% 
    summarise_if(is.factor, ~ names(which.max(table(.))))		
  
  stopifnot(all(colnames(otherData) %in% c(colnames(numericData), colnames(factorData))))  # Would indicate unhandled column types
  
  
  ## Calculate partial residuals
  partialDat <- modelData %>% 
    data.frame() %>% 
    select(one_of(var1, var2))
  
  if (length(numericData) > 0) partialDat <- cbind(partialDat, numericData)
  if (length(factorData) > 0) partialDat <- cbind(partialDat, factorData)
  
  
  partialResids <- get_partial_resids(gamFit, termnames, seWithMean)
  partialResids <- cbind(partialDat,
                         Residual = partialResids)
  
  
  ## Predictions
  # - Make a prediction for each level of the interaction var (so for all interactions that occur)
  newData <- modelData %>% 
    data.frame() %>% 
    select(one_of(var1, var2)) %>% 
    unique()
  
  # - All other data get set to their median (or the most common factor level)
  if (length(numericData) > 0) newData <- cbind(newData, numericData)
  if (length(factorData) > 0) newData <- cbind(newData, factorData)
  
  
  # - Make predictions
  newPredictions <- get_partial_preds(gamFit, newData, termnames, seWithMean) %>% 
    mutate(IsSignificant = if_else(ylower <= 0 & yupper >= 0, 'No', 'Yes')) %>%    # Check if CI crosses zero
    cbind(newData)
  
  # Add significance to residuals (for plotting):
  partialResids <- newPredictions %>% 
    select(one_of(var1, var2), IsSignificant) %>% 
    right_join(partialResids)
  
  # Return:
  list(effects = newPredictions, partialResiduals = partialResids)
}
get_partial_effects <- function(fit, var, seWithMean = TRUE) {
  get_partial_effects_interaction(fit, var, NULL, seWithMean)
}
get_partial_effects_binary_single <- function(fit, var, seWithMean = TRUE, fixedEffect = TRUE, EIDPoveNegatives = TRUE) {
  plotData <- get_partial_effects_interaction(fit, var1 = var, NULL, seWithMean, fixedEffect)
  
  # EIDPove negatives
  if (EIDPoveNegatives) {
    plotData$effects <- plotData$effects[plotData$effects[[var]] == 1, ]
    plotData$partialResiduals <- plotData$partialResiduals[plotData$partialResiduals[[var]] == 1, ]
  }
  
  # Add a column containing var as a label
  plotData$effects$variable <- var
  plotData$partialResiduals$variable <- var
  
  # Return
  plotData
}
get_partial_effects_binary <- function(fit, vars, seWithMean = TRUE, fixedEffect = TRUE, EIDPoveNegatives = TRUE) {
  allData <- lapply(vars, get_partial_effects_binary_single, fit = fit, 
                    seWithMean = seWithMean, 
                    fixedEffect = fixedEffect, 
                    EIDPoveNegatives = EIDPoveNegatives)
  
  extract_by_name <- function(x, name) x[[name]]
  effects <- lapply(allData, extract_by_name, 'effects')
  partialResiduals <- lapply(allData, extract_by_name, 'partialResiduals')
  
  effects <- do.call(rbind, effects)
  partialResiduals <- do.call(rbind, partialResiduals)
  
  list(effects = effects, partialResiduals = partialResiduals)
}
get_partial_effects_continuous <- function(gamFit, var, resolution = 1, seWithMean = TRUE) {
  ## Term names: wrap in s():
  termnames <- paste0('s(', var, ')')
  
  
  ## Data not part of effect kept constant:
  modelData <- gamFit$model
  responseIndex <- attr(modelData, 'terms') %>% attr('response')
  responseName <- colnames(modelData)[responseIndex]
  
  otherData <- modelData %>% 
    select(-one_of(responseName, var))
  
  numericData <- otherData %>% 
    summarise_if(is.numeric, ~ median(.))
  
  factorData <- otherData %>% 
    summarise_if(is.factor, ~ names(which.max(table(.))))		
  
  stopifnot(all(colnames(otherData) %in% c(colnames(numericData), colnames(factorData))))  # Would indicate unhandled column types
  
  
  ## Calculate partial residuals
  partialDat <- modelData %>% 
    data.frame() %>% 
    select(one_of(var))
  
  if (length(numericData) > 0) partialDat <- cbind(partialDat, numericData)
  if (length(factorData) > 0) partialDat <- cbind(partialDat, factorData)
  
  
  partialResids <- get_partial_resids(gamFit, termnames, seWithMean)
  partialResids <- cbind(partialDat,
                         Residual = partialResids)
  
  
  ## Predictions
  # - Predictions over a smooth range of values spanning the range of var:
  newData <- seq(min(modelData[, var]), max(modelData[, var]), by = resolution) %>% 
    data.frame()
  
  colnames(newData) <- var
  
  # - All other data get set to their median (or the most common factor level)
  if (length(numericData) > 0) newData <- cbind(newData, numericData)
  if (length(factorData) > 0) newData <- cbind(newData, factorData)
  
  # - Make predictions
  newPredictions <- get_partial_preds(gamFit, newData, termnames, seWithMean) %>% 
    mutate(NotSignificant = ylower <= 0 & yupper >= 0,
           IsSignificant = if_else(all(NotSignificant), 'No', 'Yes')) %>%    # Check if CI crosses 0 over entire range
    cbind(newData)
  
  partialResids$IsSignificant <- unique(newPredictions$IsSignificant)
  
  # Return:
  list(effects = newPredictions, partialResiduals = partialResids)
}

# Plotting functions added by Cara Brook:
plot.partial <- function(df, var, response_var,alt_var){
  df1 = df$effects
  df2= df$partialResiduals
  #head(df2)
  
  #head(df1)
  names(df1)[names(df1)==var] <- "var"
  names(df2)[names(df2)==var] <- "var"
  
  fillz = c("No"="gray70", "Yes" = "royalblue")
  
  
  #p2 <- ggplot(data=df2, aes(var,  Residual)) +
  #     geom_boxplot(aes(var~Residual))
  
  p1 <- ggplot(data=df1, aes(var, y)) + 
    geom_crossbar(aes(ymin=ylower, ymax=yupper, fill=IsSignificant), 
                  alpha=.4, show.legend = F) +
    #geom_point(aes(x=var, y=y, color=var), size=5) +
    #geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
    scale_fill_manual(values = fillz) +
    geom_hline(aes(yintercept=0), linetype=2) + theme_bw() +
    theme(panel.grid = element_blank(), axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size=8),
          plot.margin = unit(c(.1,.1,.5,.1), "cm"))+
    ylab(paste0("Partial effect of ", var, " on ", response_var))+  xlab(alt_var)
  
  #print(p1)
  
  return(p1)
}
plot.partial.cont <- function(df, log, var, response_var, alt_var){
  df1 = df$effects
  df2= df$partialResiduals
  #head(df2)
  
  #head(df1)
  names(df1)[names(df1)==var] <- "var"
  names(df2)[names(df2)==var] <- "var"
  
  fillz = c("No"="gray70", "Yes" = "skyblue")
  
  
  #p2 <- ggplot(data=df2, aes(var,  Residual)) +
  #     geom_boxplot(aes(var~Residual))
  
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
      theme(panel.grid = element_blank(), #axis.title.x = element_blank(),
            #axis.text.x = element_text(size=8, angle = 45),
            plot.margin = unit(c(.1,.1,.5,.1), "cm"),
            legend.position = 'NONE')+
      ylab(paste0("Partial effect of ", alt_var, " on ", response_var)) +  xlab(alt_var)
    
    
    
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
            plot.margin = unit(c(.1,.1,.5,.1), "cm"), 
            legend.position = c(.85,.85))+
      ylab(paste0("Partial effect of ", alt_var, " on ", response_var)) + xlab(alt_var)
  }
  
  print(p1)
  
  return(p1)
}


######### GAM EIDOLON MALE ####
modl<-gam(bat_flies~s(yday,k=7, bs="cc")+ s(year, bs="re")+s(bat_forearm_mm,bs="tp",k=7)+s(bat_weight_g,bs="tp",k=7),data=EIDM)
names(EIDF)
year.dat <- get_partial_effects(modl, var="year")
yday.dat <- get_partial_effects_continuous(modl, var="yday")
form.dat<-get_partial_effects_continuous(modl, var="bat_forearm_mm")
mass.dat<-get_partial_effects_continuous(modl, var="bat_weight_g")

p2a <- plot.partial(df=year.dat, var="year", response_var = "Count", alt_var = "Year of collection"); p2a
p2b <- plot.partial.cont(df=yday.dat, var="yday",  response_var = "Count", alt_var = "Day of the year", log = F)
p2c <- plot.partial.cont(df=form.dat, var="bat_forearm_mm",  response_var = "Count", alt_var = "Forearm length (mm)", log = F)
p2d <- plot.partial.cont(df=mass.dat, var="bat_weight_g",  response_var = "Count", alt_var = "Body mass (g)", log = F)

p2 <- cowplot::plot_grid(p2a, p2b,p2c,p2d, ncol=2, nrow = 2, labels=c("A", "B","C","D"), label_x = .1)

p2


ggsave(file =  "20221214__partial_effect_EIDM.png",
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)
######### GAM EIDOLON FEMALE ####
modl<-gam(bat_flies~s(yday,k=7, bs="cc")+ s(year, bs="re")+s(bat_forearm_mm,bs="tp",k=7)+s(bat_weight_g,bs="tp",k=7),data=EIDF)
names(EIDF)
year.dat <- get_partial_effects(modl, var="year")
yday.dat <- get_partial_effects_continuous(modl, var="yday")
form.dat<-get_partial_effects_continuous(modl, var="bat_forearm_mm")
mass.dat<-get_partial_effects_continuous(modl, var="bat_weight_g")

p1a <- plot.partial(df=year.dat, var="year", response_var = "Count", alt_var = "Year of collection"); p1a
p1b <- plot.partial.cont(df=yday.dat, var="yday",  response_var = "Count", alt_var = "Day of the year", log = F)
p1c <- plot.partial.cont(df=form.dat, var="bat_forearm_mm",  response_var = "Count", alt_var = "Forearm length (mm)", log = F)
p1d <- plot.partial.cont(df=mass.dat, var="bat_weight_g",  response_var = "Count", alt_var = "Body mass (g)", log = F)

p1 <- cowplot::plot_grid(p1a, p1b,p1c,p1d, ncol=2, nrow = 2, labels=c("A", "B","C","D"), label_x = .1)

p1


ggsave(file =  "20221214__partial_effect_EIDF.png",
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)
######### GAM ROUSETTUS FEMALE EUCAMPSIPODFA ####
modl<-gam(bat_flies~s(yday,k=7, bs="cc")+ s(year, bs="re")+s(bat_forearm_mm,bs="tp",k=7)+s(bat_weight_g,bs="tp",k=7),data=ROUF)
names(ROUF)
year.dat <- get_partial_effects(modl, var="year")
yday.dat <- get_partial_effects_continuous(modl, var="yday")
form.dat<-get_partial_effects_continuous(modl, var="bat_forearm_mm")
mass.dat<-get_partial_effects_continuous(modl, var="bat_weight_g")

p3a <- plot.partial(df=year.dat, var="year", response_var = "Count", alt_var = "Year of collection"); p3a
p3b <- plot.partial.cont(df=yday.dat, var="yday",  response_var = "Count", alt_var = "Day of the year", log = F)
p3c <- plot.partial.cont(df=form.dat, var="bat_forearm_mm",  response_var = "Count", alt_var = "Forearm length (mm)", log = F)
p3d <- plot.partial.cont(df=mass.dat, var="bat_weight_g",  response_var = "Count", alt_var = "Body mass (g)", log = F)

p3 <- cowplot::plot_grid(p3a, p3b,p3c,p3d, ncol=2, nrow = 2, labels=c("A", "B","C","D"), label_x = .1)

p3


ggsave(file =  "20221214__partial_effect_ROUF_EM.png",
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)
######### GAM ROUSETTUS MALE EUCAMPSIPODA####
modl<-gam(bat_flies~s(yday,k=7, bs="cc")+ s(year, bs="re")+s(bat_forearm_mm,bs="tp",k=7)+s(bat_weight_g,bs="tp",k=7),data=ROUM)
names(ROUM)
year.dat <- get_partial_effects(modl, var="year")
yday.dat <- get_partial_effects_continuous(modl, var="yday")
form.dat<-get_partial_effects_continuous(modl, var="bat_forearm_mm")
mass.dat<-get_partial_effects_continuous(modl, var="bat_weight_g")

p4a <- plot.partial(df=year.dat, var="year", response_var = "Count", alt_var = "Year of collection"); p4a
p4b <- plot.partial.cont(df=yday.dat, var="yday",  response_var = "Count", alt_var = "Day of the year", log = F)
p4c <- plot.partial.cont(df=form.dat, var="bat_forearm_mm",  response_var = "Count", alt_var = "Forearm length (mm)", log = F)
p4d <- plot.partial.cont(df=mass.dat, var="bat_weight_g",  response_var = "Count", alt_var = "Body mass (g)", log = F)

p4 <- cowplot::plot_grid(p4a, p4b,p4c,p4d, ncol=2, nrow = 2, labels=c("A", "B","C","D"), label_x = .1)

p4


ggsave(file =  "20221214__partial_effect_ROUM_EM.png",
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)
######### GAM ROUSETTUS MALE MEGASTREBLA ####
modl<-gam(meglastreblidae~s(yday,k=7, bs="cc")+ s(year, bs="re")+s(bat_forearm_mm,bs="tp",k=7)+s(bat_weight_g,bs="tp",k=7),data=ROUM)
names(ROUM)
year.dat <- get_partial_effects(modl, var="year")
yday.dat <- get_partial_effects_continuous(modl, var="yday")
form.dat<-get_partial_effects_continuous(modl, var="bat_forearm_mm")
mass.dat<-get_partial_effects_continuous(modl, var="bat_weight_g")

p4a <- plot.partial(df=year.dat, var="year", response_var = "Count", alt_var = "Year of collection"); p4a
p4b <- plot.partial.cont(df=yday.dat, var="yday",  response_var = "Count", alt_var = "Day of the year", log = F)
p4c <- plot.partial.cont(df=form.dat, var="bat_forearm_mm",  response_var = "Count", alt_var = "Forearm length (mm)", log = F)
p4d <- plot.partial.cont(df=mass.dat, var="bat_weight_g",  response_var = "Count", alt_var = "Body mass (g)", log = F)

p4 <- cowplot::plot_grid(p4a, p4b,p4c,p4d, ncol=2, nrow = 2, labels=c("A", "B","C","D"), label_x = .1)

p4


ggsave(file =  "20221214__partial_effect_ROUM_MW.png",
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)

######### GAM ROUSETTUS FEMALE MEGASTREBLA ####
modl<-gam(meglastreblidae~s(yday,k=7, bs="cc")+ s(year, bs="re")+s(bat_forearm_mm,bs="tp",k=7)+s(bat_weight_g,bs="tp",k=7),data=ROUF)
names(ROUF)
year.dat <- get_partial_effects(modl, var="year")
yday.dat <- get_partial_effects_continuous(modl, var="yday")
form.dat<-get_partial_effects_continuous(modl, var="bat_forearm_mm")
mass.dat<-get_partial_effects_continuous(modl, var="bat_weight_g")

p5a <- plot.partial(df=year.dat, var="year", response_var = "Count", alt_var = "Year of collection"); p5a
p5b <- plot.partial.cont(df=yday.dat, var="yday",  response_var = "Count", alt_var = "Day of the year", log = F)
p5c <- plot.partial.cont(df=form.dat, var="bat_forearm_mm",  response_var = "Count", alt_var = "Forearm length (mm)", log = F)
p5d <- plot.partial.cont(df=mass.dat, var="bat_weight_g",  response_var = "Count", alt_var = "Body mass (g)", log = F)

p5 <- cowplot::plot_grid(p5a, p5b,p5c,p5d, ncol=2, nrow = 2, labels=c("A", "B","C","D"), label_x = .1)

p5


ggsave(file =  "20221214__partial_effect_ROUF_MW.png",
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)
