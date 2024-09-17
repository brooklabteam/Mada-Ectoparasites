rm(list=ls())
library(tidyverse)
library(png)
library(bipartite)



# Set working drive
homewd = "/Users/carabrook/Developer/Mada-Ectoparasites"
setwd(homewd)


#two plots, one above the other

# load contingency tables
dat<-read.csv(paste0(homewd,"/data/angelo_ectos.csv"),sep = ',',dec = '.')
dat2<-read.csv(paste0(homewd,"/data/angelo.ectos2.csv"),sep = ',',dec = '.')

# Then I change the name of each row by the first column
row.names(dat) <- row.names(dat2)<- c("Eidolon dupreanum",  "Rousettus madagascariensis")
names(dat) <- c("Bat Flies", "Fleas","Mites", "Ticks")
names(dat2)[names(dat2)=="Other.mites"] <- "Other Mites"


 png(filename=paste0(homewd, "/final-figures/Fig1.png"),
     width = 120, 
     height = 50, 
     units = "cm", 
     pointsize = 14,
     bg = "white", 
     res = 300)

par(font=1)
plotweb(dat, labsize = 6,
              y.width.low=0.05, 
              y.width.high=0.05, 
              method="normal",
              low.lablength=0,
              adj.high = c(.5,-1),
              low.y=1.4,high.y=1.8, 
              col.low="green", 
              text.low.col="black", 
              #low.lab.dis=.05, low.lablength=20, 
              bor.col.interaction ="grey80",
              #high.lablength=12, high.lab.dis = .06,low.xoff = 0.0001,
              arrow = "up.center",
              x.lim = c(-.05,1.05),y.lim = c(-0.1,2))

par(font=3)
plotweb(t(dat2), 
        labsize = 6,
        add = T,
        y.width.low=0.05, 
        y.width.high=0.05, 
        method="normal",
        low.y=.6,high.y=1, 
        adj.high = c(.5,-1.5),
        adj.low = c(.5,1),
        #high.lablength=0,
        col.low="black", 
        col.high = "green",
        text.low.col="black", 
        #low.lab.dis=.05, low.lablength=20, 
        bor.col.interaction ="grey80",
        low.lab.dis = .12,
        #high.lablength=12, high.lab.dis = .06,low.xoff = 0.0001,
        arrow = "down.center",
        x.lim = c(-.05,1.05),y.lim = c(-0.1,2))
        #x.lim = c(0,1.25),y.lim = c(0,1))
#?plotweb

#now bring in images

path_ecto<-paste0(homewd,"/images/nowbg/")
bat<-readPNG(paste0(path_ecto,"bats.png"));rasterImage(bat,-.07,.9, -.01,1.4) #up
com<-readPNG(paste0(path_ecto,"community.png"));rasterImage(com,-.08,1.6,0,2)
cp<-readPNG(paste0(path_ecto,"cp.png"));rasterImage(cp,0.0,-.09,0.08,.3)
eg<-readPNG(paste0(path_ecto,"eg.png"));rasterImage(eg,0.1,-.03,.18,.23)
em<-readPNG(paste0(path_ecto,"em.png"));rasterImage(em,0.2,-.04,.29,.32)
ms<-readPNG(paste0(path_ecto,"ms.png"));rasterImage(ms,0.34,-.04,.44,.32) 
me<-readPNG(paste0(path_ecto,"me.png"));rasterImage(me,0.52,-.04,.59,.32) 
tk<-readPNG(paste0(path_ecto,"tk.png"));rasterImage(tk,0.69,-.04,.75,.32) 
om<-readPNG(paste0(path_ecto,"om.png"));rasterImage(om,0.83,-.04,.91,.32) 
fl<-readPNG(paste0(path_ecto,"fl.png"));rasterImage(fl,0.98,-.04,1.08,.32) 
dev.off()









#and save the whole plot
dev.off()

