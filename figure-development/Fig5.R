rm(list=ls())

library(ggplot2)

homewd= "/Users/carabrook/Developer/Mada-Ectoparasites"


#load the two loagged datasets for Rousettus and Eidolon

new_Rou <- read.csv(file = paste0(homewd, "/data/Rousettus_lagged_data.csv"))
new_ED <- read.csv(file = paste0(homewd, "/data/Eidolon_lagged_data.csv"))

#just do adults



new_Rou1 = subset(new_Rou, roost_site=="Maromizaha_Rmad" & bat_age_class!="J")
unique(new_Rou1$roost_site)

#first, load functions
calc.one.poisson <- function(mod, dat, delta_AICc, model_num){
  
  #run the model
  
  
  m.out <- glm(mod$call[2], data=dat, na.action = na.fail, family = "poisson")
  
  #get the R2 of the whole model and the relative contributions of each predictor,
  #both forced to 100% and as a subset
  
  if(length(attr(mod$terms, "dataClasses"))>2){
    
    
    out1 <- domin(formula_overall = formula(mod$call[[2]]), reg=glm, fitstat=list(pscl::pR2, "r2CU"), data=dat, family = poisson())
    #out1 <-calc.relimp(m.out,rela=F)
    #out2 <-calc.relimp(m.out,rela=T)
    
    
    
    com.df <- cbind.data.frame(predictor= names(out1$General_Dominance),
                               modelRsq = rep(out1$Fit_Statistic_Overall, length(names(out1$General_Dominance))),
                               lmg_raw = out1$General_Dominance,
                               lmg_percent = out1$Standardized)
    # 
    # 
    # 
    # com.df <- cbind.data.frame(predictor= rownames(out1),
    #                            modelRsq = rep(out1@R2, length(names(out1@lmg))),
    #                            lmg_raw = out1@lmg,
    #                            lmg_percent = out2@lmg)
    
  }else{
    
    sum.df = summary(mod)
    
    com.df <- cbind.data.frame(predictor= names(attr(mod$terms, "dataClasses")[2]),
                               modelRsq = pR2(mod)['McFadden'],
                               lmg_raw = pR2(mod)['McFadden'],
                               lmg_percent = 100)
    
  }
  
  com.df$AICc <- AICc(mod)
  com.df$delta_AICc <- delta_AICc
  com.df$model_num <- model_num
  #and mark which predictors are significant
  
  #sum.mod <- summary(mod)
  return(com.df)
}
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


data_Rou<-dplyr::select(new_Rou1,bat_flies,mean_Hday,mean_tempLag,mean_precLag,mass_forearm_residual,bat_weight_g,bat_forearm_mm,bat_sex)
names(new_Rou1)
head(data_Rou)
nrow(data_Rou[is.na(data_Rou$mass_forearm_residual),]) #18
nrow(data_Rou[is.na(data_Rou$mean_Hday),])
nrow(data_Rou[is.na(data_Rou$mean_precLag),])


str(data_Rou)

data_Rou<-na.omit(data_Rou)
#Model d
# so it drops any columns with any NA values
#dat.sub = dat[complete.cases(dat),]


globalRM1 <- glm(bat_flies ~ mean_Hday + mean_tempLag + 
                   mean_precLag + mass_forearm_residual + bat_sex, 
                 data = data_Rou, family="poisson", na.action = na.fail)


summary(globalRM1)


library(MuMIn)
dredgeRM2 = dredge(global.model = globalRM1) # use RM2
 
#look at top 10
dredgeRM2[1:10,] #AIC values within 4 points of each other are largely equivalent
subset(dredgeRM2, delta < 10)


#subselect the top X models - here 5 (there are only 5 possible in this case - but you have 10 in your dataset)
AIC.max.RM <-sort(dredgeRM2$AICc)[5] # could change if you wanted more or fewer 
out.sum.RM = get.models(dredgeRM2, subset=AICc<=AIC.max.RM) #from MuMIN package

#then calculate the relative contribution of each predictor within each model
#to the total R-squared within each mode
# this uses functions I wrote above. make sure '1:5' after 'model_num' and 'delta' terms
# matches the number of models selected under AIC.max (ends in 5 here but could be 10 or 15, etc)
library(domir)
rel.comps.RM <- mapply(calc.one.poisson, mod=out.sum.RM, model_num=as.list(1:5), 
                       delta_AICc = as.list(dredgeRM2$delta[1:5]), 
                       MoreArgs = list(dat=data_Rou), SIMPLIFY = F)


comp.df.RM <- data.table::rbindlist(rel.comps.RM)

#NA predictors are random intercepts, so rename these
comp.df.RM$predictor[is.na(comp.df.RM$predictor)] <- "random\nintercept"
unique(comp.df.RM$predictor)
comp.df.RM$predictor <- factor(comp.df.RM$predictor,levels = c("bat_sex", "mass_forearm_residual", "mean_Hday", "mean_precLag", "mean_tempLag"))
                               #levels = c("bat_sex", "bat_age_class","bat_forearm_mm","bat_weight_g",  "mean_Temp", "mean_Hday", "mean_precLag" ))

comp.df.RM$species <- "Eucampsipoda madagascariensis"
head(comp.df.RM)
#the main body of the plot
p2a <- ggplot(data=comp.df.RM) + #facet_grid(~species) +
  geom_tile(aes(x=predictor, y=model_num, fill=lmg_percent), color="gray", linewidth=1, show.legend = F) +
  #ggtitle("Eucampsipoda madagascariensis")+
  scale_fill_viridis_c(direction=-1,limits=c(0,1)) + 
  scale_y_reverse() + 
  theme_bw() + 
  scale_x_discrete(breaks = c("bat_sex", "mass_forearm_residual", "mean_Hday", "mean_precLag", "mean_tempLag"),
                   labels = c("Sex", "Mass : Forearm\nResidual","Mean\nHumidity", "Lagged\nPrecipitation", "Lagged\nTemperature"))+
  theme(panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), #axis.title.y = element_text(size=14), 
        legend.position = "bottom",
        #strip.background = element_blank(),
        #strip.text = element_text(face="italic", size=16, hjust=-.01),
        #plot.title = element_text(face = "italic"),
        plot.margin = unit(c(.4,.1,.1,0), "cm"),
        axis.text.x = element_text(size=14),
        #plot.title.position = "panel",
        #plot.tag.position = "topleft",
        #plot.tag.location = "panel",
        #axis.text.y = element_text(size=12)
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(fill="Relative contribution to standardized\nCragg and Uhler's pseudo R-sq.") ;p2a


#delta AIC
p2b <- ggplot(data=comp.df.RM) + facet_grid(species~., switch = "y") +
  geom_label(aes(x=1, y=model_num, label=specify_decimal(delta_AICc,2)), size=4.5) +
  theme_bw() + theme(panel.grid = element_blank(), axis.title = element_blank(),
                     axis.text = element_blank(), axis.ticks=element_blank(),
                     panel.background = element_blank(),
                     strip.background = element_blank(),# element_rect(fill="white"),
                     strip.text = element_text(face="italic", size=16),
                     plot.background = element_blank(),
                     plot.margin = unit(c(.8,.2,2.8,0), "cm"),
                     #plot.margin = unit(c(1.8,0,2.1,0), "cm"),
                     plot.title = element_text(hjust=0.5, face = "bold", vjust=9),
                     panel.border = element_blank())+labs(title="delta\nAICc") + scale_y_reverse()

#R-squared
p2c <- ggplot(data=comp.df.RM) + 
  geom_label(aes(x=0, y=model_num, label=specify_decimal(modelRsq,2)), size=4.5) +
  theme_bw() + 
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        axis.text = element_blank(), axis.ticks=element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        #plot.margin = unit(c(2.3,0,2.5,0), "cm"),
        plot.margin = unit(c(1.2,0,2.6,0), "cm"),
        plot.title = element_text(hjust=0.5, face = "bold", vjust=10),
        panel.border = element_blank()) +labs(title="R-sq.") + scale_y_reverse()
p2c
#and combine
#p2all <- cowplot::plot_grid(p2a, p2b, p2c, rel_widths = c(1,.1,.1), ncol = 3, nrow = 1)
p2all <- cowplot::plot_grid(p2b, p2c, p2a, rel_widths = c(.15,.15, 1), ncol = 3, nrow = 1)
Fig4A <- p2all + theme(#panel.border = element_rect(color="white", fill=NA), 
                       panel.background = element_rect(fill="white", colour = "white"),  
                       plot.margin = unit(c(.2,.1,.6,.1), "cm")) ;Fig4A





#Now get the top model fit
library(sjPlot)
library(ggeffects)


AIC.mod1<-dredgeRM2$AICc[1]

#Extract the best fit model
topfitRM1<-get.models(dredgeRM2,subset =AICc==AIC.mod1)[["26"]]
summary(topfitRM1)

#plot(topfitRM1)

plot_model(topfitRM1,type = "pred")

#Extract the data for ploting the the main predictors
plot_model(topfitRM1,type = "est")

topfitRM1_est<-get_model_data(topfitRM1,type="est")
head(topfitRM1_est)

unique(topfitRM1_est$term)

topfitRM1_est$term <- as.character(topfitRM1_est$term)
#topfitRM1_est$term[topfitRM1_est$term=="mean_Hday"] <- "Mean\nHumidity"
topfitRM1_est$term[topfitRM1_est$term=="mean_precLag"] <- "Lagged\nPrecipitation"
topfitRM1_est$term[topfitRM1_est$term=="bat_sexmale"] <- "Male Sex"
topfitRM1_est$term[topfitRM1_est$term=="mean_tempLag"] <- "Lagged\nTemperature"
#topfitRM1_est$term[topfitRM1_est$term=="bat_age_classJ"] <- "Juvenile\nAge"
#topfitRM1_est$term[topfitRM1_est$term=="bat_forearm_mm"] <- "Forearm\nLength (mm)"
#topfitRM1_est$term[topfitRM1_est$term=="bat_weight_g"] <- "Mass (g)"
topfitRM1_est$term <- factor(topfitRM1_est$term, levels=rev(c("Male Sex","Lagged\nPrecipitation",  "Lagged\nTemperature")))

topfitRM1_est$group[topfitRM1_est$p.value>=0.5]<-"notsig"
topfitRM1_est$p.stars[topfitRM1_est$p.stars==""] <- "."

colz <- c('pos' = "red3", 'neg'="cornflowerblue", 'notsig'="gray50")

head(topfitRM1_est)

Fig4B<-ggplot(data=topfitRM1_est)+
  geom_point(aes(x=estimate,y=term,colour=group),size=3)+
  geom_errorbar(aes(y=term,xmin=conf.low,xmax=conf.high,colour=group),width=.05,linewidth=1)+
  geom_label(aes(y=term, x=1.4, label=p.stars), label.size = 0, size=8) +
  geom_vline(aes(xintercept=1), linetype=2)+
  scale_color_manual(values = colz)+
  coord_cartesian(xlim=c(.75,1.45)) +
  #scale_x_continuous(breaks = c(0,log10(2),log10(4)), labels = c(1,2,4)) +
  ylab("Term")+
  #xlab("log10(estimate)")+
  xlab("Incidence Rate Ratio")+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size=14,hjust = .5),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.margin = unit(c(.6,.2,.6,.2), "cm"));Fig4B


topfitRM1_pred<-data.frame(get_model_data(topfitRM1,type="pred", terms = c("mean_precLag[0,.1,.2,.3,.5,.6]")))
topfitRM1_pred$group_col <- "mean_precLag"
topfitRM1_pred2<-data.frame(get_model_data(topfitRM1,type="pred", terms = c("mean_tempLag[14,16,18,20,22,24]")))
topfitRM1_pred2$group_col <- "mean_tempLag"
topfitRM1_pred <- rbind(topfitRM1_pred, topfitRM1_pred2)
                                                                           
#d<-fortify(topfitRM1_pred$mean_Temp)
#e<-fortify(topfitRM1_pred$mean_Hday)
#f<-fortify(topfitRM1_pred$mean_precLag)
#g<-fortify(topfitRM1_pred$mass_forearm_residual)
#topfitRM1_pred<-rbind(d,e,f,g)
#topfitRM1_pred<- data.table::rbindlist(topfitRM1_pred)

topfitRM1_pred$posneg <- "pos"
unique(topfitRM1_pred$group_col)

#topfitRM1_pred$posneg[topfitRM1_pred$group_col=="bat_sex"] <- "neg"
#topfitRM1_pred$posneg[topfitRM1_pred$group_col=="mean_Hday"] <- "pos"
topfitRM1_pred$posneg[topfitRM1_pred$group_col=="mean_precLag"] <- "pos"
topfitRM1_pred$posneg[topfitRM1_pred$group_col=="mean_tempLag"] <- "pos"
#topfitRM1_pred$posneg[topfitRM1_pred$group_col=="bat_forearm_mm"] <- "pos"
#topfitRM1_pred$posneg[topfitRM1_pred$group_col=="mass_forearm_residual"] <- "notsig"

topfitRM1_pred$group_col <- as.character(topfitRM1_pred$group_col)
#topfitRM1_pred$group_col[topfitRM1_pred$group_col=="mean_Hday"] <- "atop('mean humidity','(%)')"


topfitRM1_pred$group_col[topfitRM1_pred$group_col=="mean_precLag"] <- "plain('Lagged Precipitation (mm)')"
topfitRM1_pred$group_col[topfitRM1_pred$group_col=="mean_tempLag"] <- "paste('Lagged Temperature ', '('^0~'C)')"

topfitRM1_pred$group_col <- factor(topfitRM1_pred$group_col, levels=c("plain('Lagged Precipitation (mm)')","paste('Lagged Temperature ', '('^0~'C)')"))


#topfitRM1_pred$group_col[topfitRM1_pred$group_col=="mean_precLag"] <- "atop('Lagged Precipitation','(mm)')"
#topfitRM1_pred$group_col[topfitRM1_pred$group_col=="mean_tempLag"] <- "atop('Lagged Temperature', '('^0~'C)')"

topfitRM1_est$p.stars[topfitRM1_est$p.stars==""] <- "."

unique(topfitRM1_pred$group_col)
Fig4C<-ggplot(data=topfitRM1_pred) + 
  facet_grid(~group_col,scales = "free_x", labeller = label_parsed, switch = "x") +
  geom_line(aes(x=x, y=predicted, color=posneg), size=1, show.legend = F) + ylab(bquote('predicted abundance')) +
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, fill=posneg), alpha=.3, show.legend = F) + theme_bw() +
  scale_color_manual(values=colz) + scale_fill_manual(values=colz) +
  theme(panel.grid = element_blank(), 
        strip.text = element_text(size=16),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(), axis.text = element_text(size = 14),
        plot.margin = unit(c(.6,.2,.1,.2), "cm"));Fig4C



# And combine into Figure 3
Fig4top <- cowplot::plot_grid(Fig4A, Fig4B, Fig4C, ncol = 3, nrow = 1, labels=c("A", "B", "C"), align = "v", axis = "tb",
                              label_size = 26, rel_widths = c(1.2,.9,1.5))
Fig4top
# 
# 
# Fig3Top <- cowplot::plot_grid(Fig4AB, Fig4C, ncol = 1, nrow = 2, labels = c("", "C"), label_size = 22, rel_heights = c(1,.9)) + theme(plot.background = element_rect(fill="white"))
# print(Fig3)
# 
# ggsave(file = paste0(homewd, "/final-figures/Fig3.png"),
#        plot = Fig3,
#        units="mm",  
#        width=110, 
#        height=90, 
#        scale=3, 
#        dpi=300)




#### Plot for Eidolon Dupreanum
#make a new dataset with the lagged versions of these climate variables
# humidity at the same time step as the ectoparasite abundance
# Temp and Precipitation lag from 3 months



# FIGS2 Wfor eidolun
new_ED = subset(new_ED, bat_age_class!="J")
data_Eid<-dplyr::select(new_ED,bat_age_class,bat_flies,mean_Hday,mean_tempLag,mean_precLag,bat_weight_g,bat_forearm_mm,mass_forearm_residual,bat_sex)
names(data_Eid)
nrow(data_Eid[is.na(data_Eid$mass_forearm_residual),]) #52
data_Eid$mass_forearm_residual[is.na(data_Eid$mass_forearm_residual)]<-0
nrow(data_Eid[is.na(data_Eid$mean_Hday),])
nrow(data_Eid[is.na(data_Eid$mean_precLag),])
unique(data_Eid$bat_age_class)

#change the age class by adulte(A) and Juvenille (J)
data_Eid$bat_age_class[data_Eid$bat_age_class%in%c("NL","L","P")]<-"A"

#just adults


str(data_Eid)

data_Eid<-na.omit(data_Eid)


globalED1 <- glm(bat_flies ~ mean_Hday + mean_tempLag + 
                   mean_precLag + mass_forearm_residual + bat_sex, 
                 data = data_Eid, family="poisson", na.action = na.fail)

summary(globalED1)

# globalED2 <- glm(formula = bat_flies ~ mean_Hday + 
#                    mean_tempLag + mean_precLag + bat_weight_g+ 
#                    bat_forearm_mm+bat_age_class + bat_sex,family="poisson", 
#                    data = data_Eid, , na.action = na.fail)
# summary(globalED2)
# step(globalED2)
# 
# AIC(globalED1, globalED2) #ED1 is better 
# #and this is the same one you used in the case of Rousettus, so let's go with this


library(MuMIn)
dredgeED1 = dredge(global.model = globalED1)

#look at top 10
dredgeED1[1:10,] #AIC values within 4 points of each other are largely equivalent
subset(dredgeED1, delta < 10)


#subselect the top X models - here 5 (there are only 5 possible in this case - but you have 10 in your dataset)
AIC.max <-sort(dredgeED1$AICc)[5] # could change if you wanted more or fewer 
out.sum = get.models(dredgeED1, subset=AICc<=AIC.max) #from MuMIN package

#then calculate the relative contribution of each predictor within each model
#to the total R-squared within each mode
# this uses functions I wrote above. make sure '1:5' after 'model_num' and 'delta' terms
# matches the number of models selected under AIC.max (ends in 5 here but could be 10 or 15, etc)

rel.comps <- mapply(calc.one.poisson, mod=out.sum, model_num=as.list(1:5), 
                    delta_AICc = as.list(dredgeED1$delta[1:5]), 
                    MoreArgs = list(dat=data_Eid), SIMPLIFY = F)


comp.df <- data.table::rbindlist(rel.comps)

#NA predictors are random intercepts, so rename these
comp.df$predictor[is.na(comp.df$predictor)] <- "random\nintercept"
unique(comp.df$predictor)
comp.df$predictor <- factor(comp.df$predictor, levels = c("bat_sex", "mass_forearm_residual", "mean_Hday","mean_precLag", "mean_tempLag"))
comp.df$species <- "Cyclopodia dubia"


#the main body of the plot
p1a <- ggplot(data=comp.df) + #facet_grid(~species) +
  geom_tile(aes(x=predictor, y=model_num, fill=lmg_percent), color="gray", linewidth=1) +
  scale_fill_viridis_c(direction=-1,limits=c(0,1)) + scale_y_reverse() + theme_bw() + 
  scale_x_discrete(breaks = c("bat_sex", "mass_forearm_residual", "mean_Hday", "mean_precLag", "mean_tempLag"),
                   labels = c("Sex", "Mass : Forearm\nResidual","Mean\nHumidity", "Lagged\nPrecipitation", "Lagged\nTemperature"))+
  #facet_grid(variable~DENV.serotype) +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), #axis.title.y = element_text(size=14), 
        legend.position = "bottom",
        axis.text.x = element_text(size=14),
        #strip.background = element_blank(),
        #strip.text = element_text(face="italic", size=16, hjust=-.01),
        #axis.text.y = element_text(size=12),
        plot.margin = unit(c(.4,.1,0,0), "cm"),
        plot.title = element_text(face = "italic"),
        legend.text = element_text(size=11),
        legend.title = element_text(size=13),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  #ggtitle("Cyclopodia dubia")+
  labs(fill="Relative contribution to standardized\nCragg and Uhler's pseudo R-sq.") ;p1a


#delta AIC
p1b <- ggplot(data=comp.df) + facet_grid(species~., switch = "y") +
  geom_label(aes(x=1, y=model_num, label=specify_decimal(delta_AICc,2)), size=4.5) +
  theme_bw() + theme(panel.grid = element_blank(), axis.title = element_blank(),
                     axis.text = element_blank(), axis.ticks=element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     strip.background = element_blank(),# element_rect(fill="white"),
                     strip.text = element_text(face="italic", size=16),
                     plot.margin = unit(c(.7,.2,3.4,0), "cm"),
                     plot.title = element_text(hjust=0.5, face = "bold", vjust=9),
                     panel.border = element_blank())+labs(title="delta\nAICc") + scale_y_reverse()





#R-squared
p1c <- ggplot(data=comp.df) + geom_label(aes(x=0, y=model_num, label=specify_decimal(modelRsq,2)), size=4.5) +
  theme_bw() + theme(panel.grid = element_blank(), axis.title = element_blank(),
                     axis.text = element_blank(), axis.ticks=element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     plot.margin = unit(c(1.2,0,3.4,0), "cm"),
                     plot.title = element_text(hjust=0.5, face = "bold", vjust=10),
                     panel.border = element_blank()) +labs(title="R-sq.") + scale_y_reverse()

#and combine
#p1all <- cowplot::plot_grid(p1a, p1b, p1c, rel_widths = c(1,.1,.1), ncol = 3, nrow = 1)
p1all <- cowplot::plot_grid(p1b, p1c, p1a, rel_widths = c(.15,.15, 1), ncol = 3, nrow = 1)
Fig4D <- p1all + theme( panel.background = element_rect(fill="white", colour = "white"),  
                        plot.margin = unit(c(0.1,.1,.9,.1), "cm")) ;Fig4D

# and save as desired.

AIC.mod<-dredgeED1$AICc[1]

#Extract the best fit model
topfitED1<-get.models(dredgeED1,subset =AICc==AIC.mod)[["28"]]
summary(topfitED1)
plot_model(topfitED1,type = "pred")

#Extract the data for ploting the the main predictors
plot_model(topfitED1,type = "est")
topfitED1_est<-get_model_data(topfitED1,type="est")
head(topfitED1_est)
unique(topfitED1_est$term)

topfitED1_est$term <- as.character(topfitED1_est$term)
#topfitED1_est$term[topfitED1_est$term=="mean_Hday"] <- "Mean\nHumidity"
topfitED1_est$term[topfitED1_est$term=="mean_precLag"] <- "Lagged\nPrecipitation"
topfitED1_est$term[topfitED1_est$term=="bat_sexmale"] <- "Male Sex"
topfitED1_est$term[topfitED1_est$term=="mean_tempLag"] <- "Lagged\nTemperature"
#topfitED1_est$term[topfitED1_est$term=="bat_age_classJ"] <- "Juvenile\nAge"
#topfitED1_est$term[topfitED1_est$term=="bat_weight_g"] <- "Mass (g)"
#topfitED1_est$term[topfitED1_est$term=="bat_forearm_mm"] <- "Forearm\nLength (mm)"
topfitED1_est$term[topfitED1_est$term=="mass_forearm_residual"] <- "Mass : Forearm\nResidual"
topfitED1_est$term <- factor(topfitED1_est$term, levels=rev(c("Male Sex",  "Mass : Forearm\nResidual", "Lagged\nPrecipitation",
                                                              "Lagged\nTemperature")))
topfitED1_est$p.stars[topfitED1_est$p.stars==""] <- "."
topfitED1_est$group[topfitED1_est$p.value>=0.05]<-"notsig"

colz <- c('pos' = "red3", 'neg'="cornflowerblue", 'notsig'="gray50")

head(topfitED1_est)

Fig4E<-ggplot(data=topfitED1_est)+
  geom_point(aes(x=estimate,y=term,colour=group),size=3)+
  geom_errorbar(aes(y=term,xmin=conf.low,xmax=conf.high,colour=group),width=.05,linewidth=1)+
  geom_label(aes(y=term, x=10, label=p.stars), label.size = 0, size=8) +
  geom_vline(aes(xintercept=1), linetype=2)+
  scale_color_manual(values = colz)+
  ylab("Term")+xlab("Incidence Rate Ratio")+
  coord_cartesian(xlim =c(0.5,11)) +
  theme_bw()+
  #scale_x_log10(aes(x=estimate))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size=14,hjust = .5),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.margin = unit(c(.4,.2,2.7,.2), "cm"));Fig4E





#FigS2ab <- cowplot::plot_grid(FigS2a, FigS2b, ncol = 2, nrow = 1, labels=c("C", "D"), label_size = 20, rel_widths = c(1,.63))
#FigS2ab



topfitED1_pred<-data.frame(get_model_data(topfitED1,type="pred", terms = c("mean_precLag[0,.1,.2,.3,.5,.6]")))
topfitED1_pred$group_col <- "mean_precLag"
topfitED1_pred2<-data.frame(get_model_data(topfitED1,type="pred", terms = c("mean_tempLag[14,16,18,20,22,24]")))
topfitED1_pred2$group_col <- "mean_tempLag"
topfitED1_pred <- rbind(topfitED1_pred, topfitED1_pred2)


topfitED1_pred$posneg <- "pos"
unique(topfitED1_pred$group_col)

#topfitED1_pred$posneg[topfitED1_pred$group=="bat_sex"] <- "notsig"
#topfitED1_pred$posneg[topfitED1_pred$group=="mean_Hday"] <- "neg"
topfitED1_pred$posneg[topfitED1_pred$group=="mean_precLag"] <- "pos"
topfitED1_pred$posneg[topfitED1_pred$group=="mean_tempLag"] <- "pos"
#topfitED1_pred$posneg[topfitED1_pred$group=="mass_forearm_residual"] <- "neg"
#topfitED1_pred$posneg[topfitED1_pred$group=="bat_weight_g"] <- "notsig"

topfitED1_pred$group_col <- as.character(topfitED1_pred$group_col)


topfitED1_pred$group_col[topfitED1_pred$group_col=="mean_precLag"] <- "plain('Lagged Precipitation (mm)')"
topfitED1_pred$group_col[topfitED1_pred$group_col=="mean_tempLag"] <- "paste('Lagged Temperature ', '('^0~'C)')"

topfitED1_pred$group_col <- factor(topfitED1_pred$group_col, levels = c("plain('Lagged Precipitation (mm)')",
                                                                        "paste('Lagged Temperature ', '('^0~'C)')"))

# #topfitED1_pred$group_col[topfitED1_pred$group_col=="mean_Hday"] <- "atop('mean humidity','(%)')"
# topfitED1_pred$group_col[topfitED1_pred$group_col=="mean_precLag"] <- "atop('Lagged Precipitation','(mm)')"
# topfitED1_pred$group_col[topfitED1_pred$group_col=="mean_tempLag"] <- "atop('Lagged Temperature', '('^0~'C)')"


unique(topfitRM1_pred$group)
head(topfitED1_pred)


Fig4F<-ggplot(data=topfitED1_pred) + 
  facet_grid(~group_col,scales = "free_x", labeller = label_parsed, switch = "x") +
  geom_line(aes(x=x, y=predicted, color=posneg), size=1, show.legend = F) + ylab(bquote('predicted abundance')) +
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, fill=posneg), alpha=.3, show.legend = F) + theme_bw() +
  scale_color_manual(values=colz) + scale_fill_manual(values=colz) +
  theme(panel.grid = element_blank(), #strip.text = element_text(size=18),
        strip.text = element_text(size=16),
        strip.placement = "outside",
        strip.background = element_blank(),
        #strip.background = element_rect(fill="white"),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(), axis.text = element_text(size = 14),
        plot.margin = unit(c(.4,.2,2.2,.2), "cm"));Fig4F

# And combine into Figure 3
Fig4bottom <- cowplot::plot_grid(Fig4D, Fig4E,Fig4F, ncol = 3, nrow = 1, labels=c("D", "E", "F"), label_size = 26, rel_widths = c(1.19,.95,1.5), align = "v", axis = "tb")

Fig4All <- cowplot::plot_grid(Fig4top, Fig4bottom, ncol=1, nrow=2, align = "v", axis = "l", rel_heights = c(1,1.1)) + theme(panel.background = element_rect(fill="white"))

ggsave(file = paste0(homewd, "/final-figures/Fig4.png"),
       plot = Fig4All,
       units="mm",  
       width=220, 
       height=110, 
       scale=2.8, 
       dpi=300)


