rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(MuMIn)
library(relaimpo)
library(domir)

#load data 
homewd= "/Users/carabrook/Developer/Mada-Ectoparasites"

dat <- read.csv(file = paste0(homewd, "/Data-and-Code/Eidolon_lagged_data.csv"), header = T, stringsAsFactors = F)
head(dat)

#check on those with 'NA' in mass-forearm residual
tmp <- subset(dat, is.na(mass_forearm_residual)) 

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




#global model of predictors of ecto parasite load

#first, set na options
options(na.action ="na.fail")

dat #there are lots of NAs in the mass : forearm column - why?
#you need to go back and link the original data to the catching data by sampleid

#for now, just subset the data - "complete.cases" selects only the columns with complete information-
# so it drops any columns with any NA values
dat.sub = dat[complete.cases(dat),]


global.model <- glm(bat_flies ~ mean_Hday + mean_tempLag + 
                      mean_precLag + mass_forearm_residual + bat_sex, 
                    data = dat.sub, family="poisson", na.action = na.fail)
summary(global.model)



#run dredge
out.comp <- dredge(global.model =  global.model)


#subselect the top X models - here 5 (there are only 5 possible in this case - but you have 10 in your dataset)
AIC.max <-sort(out.comp$AICc)[10] # could change if you wanted more or fewer 
out.sum = get.models(out.comp, subset=AICc<=AIC.max) #from MuMIN package - this selects the top X number of models from dredge and formats as a list

#then calculate the relative contribution of each predictor within each model
#to the total R-squared within each mode
# this uses functions I wrote above. make sure '1:5' after 'model_num' and 'delta' terms
# matches the number of models selected under AIC.max (ends in 5 here but could be 10 or 15, etc)
rel.comps <- mapply(calc.one.poisson, mod=out.sum, model_num=as.list(1:10), 
                    delta_AICc = as.list(out.comp$delta[1:10]), 
                    MoreArgs = list(dat=dat.sub), SIMPLIFY = F)

comp.df <- data.table::rbindlist(rel.comps)

#NA predictors are random intercepts, so rename these
comp.df$predictor[is.na(comp.df$predictor)] <- "random\nintercept"


#the main body of the plot
p1a <- ggplot(data=comp.df) + geom_tile(aes(x=predictor, y=model_num, fill=lmg_percent), color="gray", size=1) +
  scale_fill_viridis_c(limits=c(0,1)) + scale_y_reverse() + theme_bw() + 
  #facet_grid(variable~DENV.serotype) +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), #axis.title.y = element_text(size=14), 
        legend.position = "top",
        axis.text.x = element_text(size=10),
        #axis.text.y = element_text(size=12)
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + labs(fill="Relative contribution to standardized\nCragg and Uhler's pseudo R-sq.") 


#delta AIC
p1b <- ggplot(data=comp.df) + geom_label(aes(x=1, y=model_num, label=specify_decimal(delta_AICc,2))) +
  theme_bw() + theme(panel.grid = element_blank(), axis.title = element_blank(),
                     axis.text = element_blank(), axis.ticks=element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     plot.margin = unit(c(1.8,0,2.1,0), "cm"),
                     plot.title = element_text(hjust=0.5, face = "bold", vjust=9),
                     panel.border = element_blank())+labs(title="delta\nAICc") + scale_y_reverse()

#R-squared
p1c <- ggplot(data=comp.df) + geom_label(aes(x=0, y=model_num, label=specify_decimal(modelRsq,2))) +
  theme_bw() + theme(panel.grid = element_blank(), axis.title = element_blank(),
                     axis.text = element_blank(), axis.ticks=element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     plot.margin = unit(c(2.3,0,2.1,0), "cm"),
                     plot.title = element_text(hjust=0.5, face = "bold", vjust=10),
                     panel.border = element_blank()) +labs(title="R-sq.") + scale_y_reverse()

#and combine
p1all <- cowplot::plot_grid(p1a, p1b, p1c, rel_widths = c(1,.1,.1), ncol = 3, nrow = 1)
p1all_border <- p1all + theme(panel.border = element_rect(color="black", fill=NA))

print(p1all_border)

# and save as desired.

ggsave(file ="heatmap.png",
       plot=p1all_border,
       units="mm",  
       width=50, 
       height=60, 
       scale=3, 
       dpi=300)

