rm(list=ls())
library(tidyverse)
library(png)
library(bipartite)



# Set working drive
homewd = "/Users/carabrook/Developer/Mada-Ectoparasites"
#homewd ="C:/Users/tlaverty/Desktop/R/R_repositories/Mada-Ectoparasites"
setwd(homewd)


#two plots, one above the other

# load contingency tables
dat<-read.csv(paste0(homewd,"/data/angelo_ectos.csv"),sep = ',',dec = '.')
dat2<-read.csv(paste0(homewd,"/data/angelo.ectos2.csv"),sep = ',',dec = '.')


# Then I change the name of each row by the first column
row.names(dat) <- row.names(dat2)<- c("Eidolon dupreanum",  "Rousettus madagascariensis")
names(dat) <- c("Bat Flies", "Fleas","Mites", "Ticks")
names(dat2)[names(dat2)=="Other.mites"] <- "Other Mites"

#reorder columns and tweak text spacing for figure
dat3 <- dat2[,c(8,2,1,3,4,5,7,6)]
names(dat3)[names(dat3) == 'Thaumapsylla'] <- 'Thaumapsylla sp.                  '
names(dat3)[names(dat3) == 'Echidnophaga'] <- '  Echidnophaga sp.'
names(dat3)[names(dat3) == 'Eucampsipoda'] <- 'Eucampsipoda\nmadagascariensis'
names(dat3)[names(dat3) == 'Cyclopodia'] <- "Cyclopodia dubia"
names(dat3)[names(dat3) == 'Megastrebla'] <- 'Megastrebla\nwenzeli'
names(dat3)[names(dat3) == 'Meristapsis'] <- 'Meristapsis sp.'
names(dat3)[names(dat3) == 'Ornithodoros'] <- 'Ornithodoros\nsp.'

png(filename=paste0(homewd, "/figure-development/prep-files/Fig1pre.png"),
    width = 120, 
    height = 50, 
    units = "cm", 
    pointsize = 14,
    bg = "white", 
    res = 300)

par(font=1)
plotweb(dat[,c(2,1,3:4)], labsize = 5,
        y.width.low=0.05, 
        y.width.high=0.05, 
        method="normal",
        low.lablength=0,
        adj.high = c(.5,-1),
        low.y=1.4,high.y=1.8, 
        col.low="black", 
        col.high=c("#a6cee3","#1f78b4","#b2df8a","#33a02c"), 
        col.interaction=c("#a6cee3","#1f78b4","#b2df8a","#33a02c"),
        #low.lab.dis=.05, low.lablength=20, 
        bor.col.interaction ="grey80",
        #high.lablength=12, high.lab.dis = .06,low.xoff = 0.0001,
        arrow = "up.center",
        x.lim = c(-.05,1.05),y.lim = c(-0.1,2))


par(font=3)
plotweb(t(dat3), 
        labsize = 5,
        add = T,
        y.width.low=0.05, 
        y.width.high=0.05, 
        method="normal",
        low.y=.6,high.y=1.05, 
        adj.high = c(.5,-1.5),
        adj.low = c(.5,1),
        #high.lablength=0,
        col.low=c(rep("#a6cee3",2),rep("#1f78b4", 3),rep("#b2df8a", 2),"#33a02c"),
        col.interaction=c(rep("#a6cee3",4),rep("#1f78b4", 6),rep("#b2df8a", 4), rep("#33a02c", 2)),
        col.high = "black",
        text.low.col="black", 
        #low.lab.dis=.05, low.lablength=20, 
        bor.col.interaction ="grey80",
        low.lab.dis = .12,
        #text.rot = 15,
        #high.lablength=12, high.lab.dis = .06,low.xoff = 0.0001,
        arrow = "down.center",
        x.lim = c(-.05,1.05),y.lim = c(-0.1,2))
#x.lim = c(0,1.25),y.lim = c(0,1))
#?plotweb


#now bring in images

path_ecto<-paste0(homewd,"/images/nowbg/")
bat<-readPNG(paste0(path_ecto,"bats.png"));rasterImage(bat,-.07,.9, -.01,1.4) #up
com<-readPNG(paste0(path_ecto,"community.png"));rasterImage(com,-.08,1.6,0,2)
cp<-readPNG(paste0(path_ecto,"cp.png"));rasterImage(cp,0.09,-.21,.18,.15)
eg<-readPNG(paste0(path_ecto,"eg.png"));rasterImage(eg, -0.01,-.01,.07,.28)
em<-readPNG(paste0(path_ecto,"em.png"));rasterImage(em,0.26,-.1,0.34,.31)
ms<-readPNG(paste0(path_ecto,"ms.png"));rasterImage(ms,0.41,-.06,.49,.32) 
me<-readPNG(paste0(path_ecto,"me.png"));rasterImage(me,0.58,-.06,.65,.32) 
tk<-readPNG(paste0(path_ecto,"tk.png"));rasterImage(tk,0.99,-.04,1.06,.32) 
om<-readPNG(paste0(path_ecto,"om.png"));rasterImage(om,0.80,-.06,.94,.33) 
fl<-readPNG(paste0(path_ecto,"fl.png"));rasterImage(fl,-0.1,.034,0,.36) 
dev.off()

#now load in Affinity Designer or Preview and remove italics as needed
