rm(list=ls())
library(tidyverse)
library(png)
library(bipartite)



# Set working drive
homewd = "/Users/carabrook/Developer/Mada-Ectoparasites"
setwd(homewd)


# load contingency table
dat2<-read.csv(paste0(homewd,"/data/angelo.ectos2.csv"),sep = ',',dec = '.')

# Then I change the name of each row by the first column
row.names(dat2)<- c("Eidolon dupreanum", "Pteropus rufus", "Rousettus madagascariensis")
head(dat2)



png(filename=paste0(homewd, "/final-figures/Fig1.png"),
    width = 480, 
    height = 480, 
    units = "mm", 
    pointsize = 14,
    bg = "white", 
    res = 300)
    

plotweb(dat2, 
        y.width.low=0.05, y.width.high=0.05, 
        method="normal",
        low.y=1.4,high.y=1.8, 
        col.low="green", 
        text.low.col="black", 
        #low.lab.dis=.05, low.lablength=20, 
        bor.col.interaction ="grey80",
        #high.lablength=12, high.lab.dis = .06,low.xoff = 0.0001,
        arrow = "down.center",
        x.lim = c(-.1,1.1),y.lim = c(-.5,2))
        x.lim = c(0,1.25),y.lim = c(0,1))
#?plotweb

#now bring in images

path_ecto<-paste0(homewd,"/images/nowbg/")
bat<-readPNG(paste0(path_ecto,"bats.png"))
rasterImage(bat,1.1, # back
            .4, # down
            1.2, #front
            .7) #up
com<-readPNG(paste0(path_ecto,"community.png"));rasterImage(com,1.1,.9,1.24,1.2)

fl<-readPNG(paste0(path_ecto,"fl.png"));rasterImage(fl,1,1.92,1.1,2.1)
om<-readPNG(paste0(path_ecto,"om.png"));rasterImage(om,0.8,1.92,0.87,2.1) 
tk<-readPNG(paste0(path_ecto,"tk.png"));rasterImage(tk,0.66,1.92,0.75,2.1)
me<-readPNG(paste0(path_ecto,"me.png"));rasterImage(me,0.52,1.92,.60,2.1) 
ms<-readPNG(paste0(path_ecto,"ms.png"));rasterImage(ms,0.3,1.9,.47,2.25) 
em<-readPNG(paste0(path_ecto,"em.png"));rasterImage(em,0.2,1.92,.33,2.2)
eg<-readPNG(paste0(path_ecto,"eg.png"));rasterImage(eg,0.1,1.97,.2,2.12)
cp<-readPNG(paste0(path_ecto,"cp.png"));rasterImage(cp,0.0,1.95,.13,2.3)

#and save the whole plot
dev.off()

