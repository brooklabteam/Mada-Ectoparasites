### ALLUVIAL PLOT Using the Bipartite package

#Packages that we going to use for this analyse
library(bipartite)

#Change working directorie
setwd("D:/EKIPA_FANIHY/Ectoparasite_project_Theresa/r_code_data")

# Import dataset
# (I use this lab dataset For the alluvial plot)
# This is the  data that We identified in the lab but not the one from the field
afa<-read.csv("20200628_AA_SG_Ectoparasite_Species.xlsx - ID_Ectoparasites.csv")
View(afa)

 
# I try to check the list of ectoparasites
unique(afa$Ecto_type)
afa$Ecto_type[afa$Ecto_type=="Bat fly"]<-"Bat flies"
afa$Ecto_type[afa$Ecto_type=="Batflies"]<-"Bat flies"
# Then try to check the bat species and change the name of the species
unique(afa$Bat_species)
afa$Bat_species[afa$Bat_species=="Eiodolon_dupreaneum"]<-"Eiodolon dupreaneums"
afa$Bat_species[afa$Bat_species=="Pteropus_rufus"]<-"Pteropus rufus"
afa$Bat_species[afa$Bat_species=="Rousettus_madagascariensis"]<-"Rousettus madagascariensis"
afa$Ecto_genus[is.na(afa$Ecto_genus)]<-"Other mites"
unique(afa$Ecto_genus)
#Since we don't want to use the "Egg" as ectoparasiste and we want to remove the NA 
# so we gona do a subset of the data with out "Egg" and NA 
afa<-subset(afa,Ecto_type!="Egg")
names(afa)

# Now we are going to selecte the colume that we need for the alluvial plot
# here we need the two column "Bat species"=> column 3 and "Ecto type"=> column 5
bip<-afa[c(3,5)]

# And from this two columns we gona make a contengency table with the frequence on it
bip1=table(bip$Bat_species,bip$Ecto_type)
bip1=print.table(bip1)
View(bip1)

# Now I save this data here 
write.csv(bip1,"angelo.ectos.csv")

# I re-import the data 
dat<-read.csv('angelo.ectos.csv',sep = ',',dec = '.')

# Then I make the first column with the bat species  name (here named X by defauls) as column name
row.names(dat)<-dat$X
View(dat)

# I removed the first column (X column) and have a kind of matrix
dat=dat[,-1]

# Now I plot the interaction 
plotweb(dat,
        text.rot = 360,
        method="normal",
        bor.col.interaction ="grey80", col.high = "grey10",col.low="green",
        ybig = 1,arrow = "down.center",labsize = 1,x.lim = c(0,1.25),y.lim = c(0,2))

# Here I want to add the pictur of bats and ectoparasite in the plot. 
# all of the picture I need to make good ones and upload all of these there.
library(png)
bat<-readPNG("bats.png")
rasterImage(bat,1.3, # back
            .2, # down
            1.14, #front
            .8) #up
bf<-readPNG("img_batfly.png");rasterImage(bf,0.1,2.1,.3,1.7) 
fl<-readPNG("img_flea.png");rasterImage(fl,0.39,1.73,.5,2)
mt<-readPNG("img_mite.png");rasterImage(mt,0.68,1.75,.85,2.1)
tc<-readPNG("img_tick.png");rasterImage(tc,1,1.7,1.2,2)


###############################THE FOLLOWING CODES ARE UNDER MODIFICATION ##########################
## In this part we are going to make an other plot with the ecto type and the ecto genus but from the same data set
# I select the third and sixth columns and named this subset as bip2
#bip2<-afa[c(5,6)]
#bip2$Ecto_genus[ is.na(bip2$Ecto_genus)]<-"Other mites"
#And I try to build a contengency table with the ferequency 
#bip2=table(bip2$Ecto_type,bip2$Ecto_genus)
#bip2=print.table(bip2)
#View(bip2)

#write.csv(bip2,"angelo.ectos2.csv")

#dat2<-read.csv('angelo.ectos2.csv',sep = ',',dec = '.')

# Then I change the name of each row by the first column
#row.names(dat2)<-dat2$X
#View(dat2)


#dat2=dat2[,-1]

# And I plot the web above the first one  (if add=TRUE )
#plotweb(dat2, y.width.low=0.03, y.width.high=0.2, method="cca",
#        add=TRUE, low.y=1.9,high.y=3.7, col.low="green", text.low.col="black", 
#        low.lab.dis=0, low.lablength=0, bor.col.interaction ="grey80",
#        high.lablength=8,low.xoff = 0.00001)