### FIGURE 05GENETIC TREE  #####

rm(list=ls())

library(ggplot2)
library(ggtree)
library(plyr)
library(dplyr)
library(ape)
library(ggnewscale)
library(treeio)
library(png)
library(grid)
library(extrafont)



# Add a line where you define 'homewd' as the working directory on your own computer
# you can comment this out and link to yours
homewd="/Users/carabrook/Developer/Mada-Ectoparasites"


#load the tree -- here the trimmed version
tree <- read.tree(file = paste0(homewd, "/data/phylo/T3.raxml.supportFBP"))

head(tree)

class(tree)



#root it on drosophila
rooted.tree <- root(tree, which(tree$tip.label == "NC_001709.1_Drosophila_melanogaster_mitochondrion__complete_genome"))

head(rooted.tree)
#take a quick look in base R
plot(rooted.tree)


#make a data frame
tree.dat <- cbind.data.frame(old_tip_label=rooted.tree$tip.label)

tree.dat$Accession_Number = sapply(strsplit(tree.dat$old_tip_label,".1_"),"[",1)
#tree.dat$Accession_Number = sapply(strsplit(tree.dat$Accession_Number,"_extraction"),"[",1)

head(tree.dat)


#load tree data prepared from elsewhere
dat <- read.csv(file=paste0(homewd,"/data/phylo/ecto_metadata_combined_update_2024ValBIO.csv"), header = T, stringsAsFactors = F)
head(dat)
names(dat)

#how many individual bats?
dat$bat_sampleid <- sapply(strsplit(dat$Accession_Number_Old, "_"), '[',1)

#check subgroup names
unique(dat$Genus)
unique(dat$Accession_Number)

#eliminate the extra info
#dat$Accession_Number <- sapply(strsplit(dat$Accession_Number,"."),'[',1)

#and merge

setdiff(tree.dat$Accession_Number, dat$Accession_Number)
setdiff(dat$Accession_Number, tree.dat$Accession_Number)

tree.merge <- merge(tree.dat, dat, by.x="Accession_Number",by.y = "Accession_Number", all.x = T, sort = F)

head(tree.merge)

names(tree.merge)

tree.merge[duplicated(tree.merge$Accession_Number),]
tree.merge <- tree.merge[!duplicated(tree.merge),]
subset(tree.merge, is.na(Country_of_Collection))
unique(tree.merge$Genus)

head(tree.merge)


# Thoses number are duplicated #"AB632571" "MK140180" "KF021535" "MF462046" "MK140104"
# I need to remove the duplication



#make a color vector for each genus
colz = c("Basilia"="darkgoldenrod1", "Brachytarsina" = "mediumseagreen", "Cyclopodia"= "magenta", "Drosophila" = "black", "Eucampsipoda" = "darkorchid1", "Leptocyclopodia"="darkred","Megastrebla" ="purple4", "Nycteribia" = "cyan4", "Penicillidia"="darkorange3", "Phthiridium" = "tomato",  "Streblidae" = "royalblue")# "Trichobius" = "purple4",  )

#pick order for the labels
tree.merge$Genus <- factor(tree.merge$Genus, levels = sort(unique(tree.merge$Genus)))   
head(tree.merge)

length(unique(tree.merge$bat_sampleid[tree.merge$Type==1])) #32
#how many of each?
length(unique(tree.merge$bat_sampleid[tree.merge$Type==1 & tree.merge$Genus=="Eucampsipoda"])) #30 unique bats
length(unique(tree.merge$bat_sampleid[tree.merge$Type==1 & tree.merge$Genus=="Megastrebla"])) #2 unique bats
length(unique(tree.merge$bat_sampleid[tree.merge$Type==1 & tree.merge$Genus=="Cyclopodia"])) #2 unique bats
length(tree.merge$bat_sampleid[tree.merge$Type==1 & tree.merge$Genus=="Cyclopodia"]) #2
length(tree.merge$bat_sampleid[tree.merge$Type==1 & tree.merge$Genus=="Megastrebla"]) #3... 2 from the same bat
length(tree.merge$bat_sampleid[tree.merge$Type==1 & tree.merge$Genus=="Eucampsipoda"]) #38

#make summary table of the unique bats by species and the ectos sampled from each

TableS2 <- subset(tree.merge, Type==1)
TableS2 <- dplyr::select(TableS2, Host_Binomial, bat_sampleid, Binomial, Accession_Number)
TableS2 <- arrange(TableS2, Host_Binomial, Binomial, bat_sampleid, Accession_Number)
write.csv(TableS2 , file = paste0(homewd, "/final-tables/tableS2.csv"), row.names = F)
# 2 Cyclopodia sequenced from 2 Eidolon.
# 38 Eucampsipoda sequenced from 30 Rousettus
# 3 Megastrebla sequenced from 2 Rousettus (both of the Rousettus also provided a single Eucampsipoda sequence)


#and  category
tree.merge$Type <- as.character(tree.merge$Type)

tree.merge$Type[tree.merge$Type=="0"] <- "Reference sequence"
tree.merge$Type[tree.merge$Type=="1"] <- "New this study"
tree.merge$Type <- as.factor(tree.merge$Type)
tree.merge$Binomial <- gsub(pattern = "_", replacement = " ", x = tree.merge$Binomial)
tree.merge$Host_Binomial <- gsub(pattern = "_", replacement = " ", x = tree.merge$Host_Binomial)
tree.merge$Country_of_Collection <- gsub(pattern = "_", replacement = " ", x = tree.merge$Country_of_Collection)

tree.merge$new_label <- paste0(tree.merge$Accession_Number, " | ", tree.merge$Binomial, " | ", tree.merge$Host_Binomial, " | ", tree.merge$Year_Sample_Collected, " | ", tree.merge$Country_of_Collection)

tree.merge <- dplyr::select(tree.merge, new_label, names(tree.merge)[1:11])

rooted.tree$tip.label <- tree.merge$new_label
tree.merge$new_label <- paste0(tree.merge$Accession_Number, " | ", tree.merge$Binomial, " | ", tree.merge$Host_Binomial, " | ", tree.merge$Year_Sample_Collected, " | ", tree.merge$Country_of_Collection)

head(rooted.tree)
head(tree.merge)

#length(tree.merge$b) tree.merge$Type

shapez = c("New this study" =  24, "Reference sequence" = 22)
colz2 = c('1' =  "yellow", '0' = "white")

#add node shapes to represent bootstrap values
p0<-ggtree(rooted.tree)
p0.dat <- p0$data
p0.dat$Bootstrap <- NA
Bootstrap<-p0.dat$Bootstrap[(length(tree.dat$tip_label)+1):length(p0.dat$label)] <- as.numeric(p0.dat$label[(length(tree.dat$tip_label)+1):length(p0.dat$label)])#fill with label
#p0.dat$Bootstrap[is.na(p0.dat$Bootstrap) & p0.dat$label==""] <- 100 


#rooted.tree$node.label <- as.numeric(rooted.tree$node.label)
#rooted.tree$node.label[rooted.tree$node.label<50] <- NA

#edit a few that are funky:
tree.merge$new_label[tree.merge$new_label=="OM327588 | Brachytarsina kanoi |  | 2022 | Pakistan"] <- "OM327588 | Brachytarsina kanoi | 2022 | Pakistan"
tree.merge$new_label[tree.merge$new_label=="OM327589 | Brachytarsina kanoi |  | 2022 | Pakistan"] <- "OM327589 | Brachytarsina kanoi | 2022 | Pakistan"
tree.merge$new_label[tree.merge$new_label=="MH282032 | Basilia tiptoni | NA | 2013 | Panama"] <- "MH282032 | Basilia tiptoni | 2013 | Panama"
tree.merge$new_label[tree.merge$new_label=="NC_001709 | Drosophila melanogaster | NA | 1999 | USA" ] <- "NC_001709 | Drosophila melanogaster | 1999 | USA"

rooted.tree$tip.label[rooted.tree$tip.label=="OM327588 | Brachytarsina kanoi |  | 2022 | Pakistan"]  <- "OM327588 | Brachytarsina kanoi | 2022 | Pakistan"
rooted.tree$tip.label[rooted.tree$tip.label=="OM327589 | Brachytarsina kanoi |  | 2022 | Pakistan"] <- "OM327589 | Brachytarsina kanoi | 2022 | Pakistan"
rooted.tree$tip.label[rooted.tree$tip.label=="MH282032 | Basilia tiptoni | NA | 2013 | Panama"] <- "MH282032 | Basilia tiptoni | 2013 | Panama"
rooted.tree$tip.label[rooted.tree$tip.label=="NC_001709 | Drosophila melanogaster | NA | 1999 | USA"] <- "NC_001709 | Drosophila melanogaster | 1999 | USA"

#Get the clade numbers to known the clades that we are going  collapse

p1 <- ggtree(rooted.tree) %<+% tree.merge + 
  #geom_nodelab(size=1.5,nudge_x = -.01, nudge_y = .7) +
  geom_nodepoint(aes(fill=Bootstrap, show.legend = T), shape=21, stroke=.2, size=1.5, color="black")+
  scale_fill_continuous(low="white", high="black", limits=c(0,100))+
  geom_treescale(x=.033,y=94, fontsize = 3.5)+
  ggnewscale::new_scale_fill() +
  geom_tippoint(aes(fill=Genus, color=Genus, shape=Type)) +
  geom_tiplab(size = 1.5)+
  scale_fill_manual(values=colz) + 
  scale_color_manual(values=colz) + 
  scale_shape_manual(values=shapez) + 
  #theme_bw()+
  theme(legend.position = "inside",
        legend.position.inside  = c(.25,.85),
        legend.box = "horizontal",
        legend.text = element_text(size=8),
        legend.title = element_text(size=9),
        legend.key.height = unit(.8,"lines"))+ 
  xlim(c(0,.4)) #+ ylim(c(0,160))
p1

#save the plots (this could go to the supplementary information)
ggsave(file = paste0(homewd, "/final-figures/FigS6.png"),
       plot = p1,
       units="mm",  
       width=110, 
       height=90, 
       scale=2.5, 
       dpi=300)

ggsave(file = paste0(homewd, "/final-figures/FigS6.pdf"),
       plot = p1,
       units="mm",  
       width=110, 
       height=90, 
       scale=2.5, 
       dpi=300)

#now collapse some nodes to make the main text figure more readable

#find the nodes
## Here, we are going to collapse the samples with the More Recent Commons Ancestor (MRCA) ####
#first check from the previous plot all of the first and the last tiplabel sharing the same RCA
#then let check the name with the following code to avoid the miss writing
rooted.tree$tip.label

# Now lets try to find the common ancestor for each clade and stor it one object
eucampsMad_Roumad <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OR732293 | Eucampsipoda madagascariensis | Rousettus madagascariensis | 2019 | Madagascar"),which(rooted.tree$tip.label == "OR732245 | Eucampsipoda madagascariensis | Rousettus madagascariensis | 2019 | Madagascar"),method="phylo")
eucampsAfr_Rouaeg <- MRCA(rooted.tree, which(rooted.tree$tip.label == "MH151066 | Eucampsipoda africana | Rousettus aegyptiacus | 2018 | Nigeria" ),which(rooted.tree$tip.label == "MH151063 | Eucampsipoda africana | Rousettus aegyptiacus | 2018 | Nigeria"),method="phylo")
eucampsThe_Rouobl <- MRCA(rooted.tree, which(rooted.tree$tip.label == "KF021498 | Eucampsipoda theodori | Rousettus obliviosus | 2010 | Comoros" ),which(rooted.tree$tip.label == "KF021500 | Eucampsipoda theodori | Rousettus obliviosus | 2010 | Comoros"),method="phylo")
eucampsAfr_Roules <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OM283590 | Eucampsipoda africana | Rousettus leschenaultii | 2019 | Pakistan"),which(rooted.tree$tip.label =="OM283592 | Eucampsipoda africana | Rousettus leschenaultii | 2019 | Pakistan"),method="phylo")
cyclopDub_Eidup   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OR732304 | Cyclopodia dubia | Eidolon dupreanum | 2019 | Madagascar"),which(rooted.tree$tip.label == "MF462043 | Cyclopodia dubia | Eidolon dupreanum | 2017 | Madagascar"),method="phylo")
cycloPhors_Ptesp   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "KF273770 | Cyclopodia horsefieldi | Pteropus hypomelanus | 2006 | Malaysia" ),which(rooted.tree$tip.label == "KF273782 | Cyclopodia horsefieldi | Pteropus vampyrus | 2004 | Malaysia" ),method="phylo")
streb_Hyp   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "MW792204 | Streblidae spp | Hipposideros ruber | 2017 | Uganda"),which(rooted.tree$tip.label == "MW792205 | Streblidae spp | Hipposideros ruber | 2017 | Uganda"  ),method="phylo")
megaWenz_Roumad   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OR732258 | Megastrebla wenzeli | Rousettus madagascariensis | 2019 | Madagascar"),which(rooted.tree$tip.label == "OR732302 | Megastrebla wenzeli | Rousettus madagascariensis | 2019 | Madagascar"))
brachy_kanoi      <- MRCA(rooted.tree,which(rooted.tree$tip.label=="OM327589 | Brachytarsina kanoi | 2022 | Pakistan"), which(rooted.tree$tip.label=="MT362949 | Brachytarsina spp | bat | 2020 | South Korea"))
peni_ful   <- MRCA(rooted.tree,which(rooted.tree$tip.label== "ON704710 | Penicillidia fulvida | Miniopterus spp | 2015 | Kenya"), which(rooted.tree$tip.label=="ON704664 | Penicillidia fulvida | Rhinolophus fumigatus | 2015 | Kenya" ))
peni_mono  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "AB632567 | Penicillidia monoceros | Myotis daubentonii | 2011 | Japan"), which(rooted.tree$tip.label=="MW590972 | Penicillidia monoceros | bird | 2021 | Finland"))
peni_dufo  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "MK140181 | Penicillidia dufourii | Myotis myotis | 2018 | Romania" ), which(rooted.tree$tip.label=="MK140183 | Penicillidia dufourii | Myotis blythii | 2018 | Hungary"))
peni_cons  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "MK140180 | Penicillidia conspicua | Miniopterus schreibersii | 2018 | Romania"), which(rooted.tree$tip.label=="MK140180 | Penicillidia conspicua | Miniopterus schreibersii | 2018 | Romania"))
peni_other  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "KF021518 | Penicillidia fulvida | Miniopterus gleni | 2012 | Madagascar"), which(rooted.tree$tip.label=="KF021535 | Penicillidia oceania | Miniopterus schreibersi | 2006 | Philippines"))
pthrid  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "MT362948 | Phthiridium spp | bat | 2020 | South Korea"), which(rooted.tree$tip.label=="MK140116 | Phthiridium spp | Rhinolophus mehelyi | 2018 | Romania"))
nycterib  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "KF021501 | Nycteribia parvula | Miniopterus schreibersii | 2006 | Philippines" ), which(rooted.tree$tip.label=="KF021492 | Nycteribia africana | Rousettus aegyptiacus | 2006 | Kenya"))
basil  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "MH282032 | Basilia tiptoni | 2013 | Panama" ), which(rooted.tree$tip.label=="OL847632 | Basilia lindolphoi | Myotis nigricans | 2021 | Brasil"))
basil2  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "MK140104 | Basilia nana | Myotis myotis | 2018 | Hungary" ), which(rooted.tree$tip.label=="AB632538 | Basilia rybini | Myotis daubentonii | 2011 | Japan"))

  
 

pa<- 
  ggtree(rooted.tree) %<+% tree.merge + 
  #geom_nodelab(size=3,nudge_x = -.005, nudge_y = .7) +
  geom_tippoint(aes(fill=Genus),size=2, color="black", show.legend = T, shape=21) +
  geom_tiplab(size = 3, hjust=-.01)+
  scale_fill_manual(values=colz) + 
  #scale_color_manual(values=colz) + 
  #scale_shape_manual(values=shapez) + 
  geom_treescale(x=.03,y=55)+
  #theme_bw()+
  theme(legend.position = c(.15,.75))+ 
  xlim(c(0,.49)) +
  ggnewscale::new_scale_fill() +
  geom_nodepoint(aes(fill=Bootstrap, show.legend = T), shape=21, stroke=.2, size=2, color="black")+
  scale_fill_continuous(low="white", high="black", limits=c(0,100))+
  geom_cladelabel(node = eucampsMad_Roumad,label = "Eucampsipoda madagascariensis | Rousettus madagascariensis (collapsed)",offset=0, fontsize = 3,  fill="yellow", geom="label", alpha=.3) +
  geom_cladelabel(node = eucampsAfr_Rouaeg, label = "Eucampsipoda africana | Rousettus aegyptiacus (collapsed)",offset=0, fontsize = 3, color="black")+
  geom_cladelabel(node = eucampsThe_Rouobl, label = "Eucampsipoda theodori | Rousettus obliviosus (collapsed)",offset=0, fontsize = 3, color="black")+
  geom_cladelabel(node = eucampsAfr_Roules, label = "Eucampsipoda africana | Rousettus leschenaultii (collapsed)",offset=0, fontsize = 3, color="black") +
  geom_cladelabel(node = cyclopDub_Eidup, label = "Cyclopodia dubia | Eidolon dupreanum (collapsed)",offset=-0.005, fontsize =3,  fill="yellow", geom="label", alpha=.3)+
  geom_cladelabel(node = cycloPhors_Ptesp, label = "Cyclopodia horsefieldi | Pteropus spp. (collapsed)",offset=0.002, fontsize =3, color="black")+
  geom_cladelabel(node = megaWenz_Roumad, label = "Megastrebla wenzeli | Rousettus madagascariensis (collapsed)",offset=0, fontsize = 3,  fill="yellow", geom="label", alpha=.3)+
  geom_cladelabel(node = brachy_kanoi, label = "Brachytarsina spp. (collapsed)",offset=0.039, fontsize = 3, color="black")+
  geom_cladelabel(node = streb_Hyp, label = "Streblidae spp. | Hipposideros ruber (collapsed)",offset=.039, fontsize = 3, color="black")+
  geom_cladelabel(node = peni_ful, label = "Penicillidia fulvida (collapsed)",offset=0, fontsize = 3, color="black")+
  geom_cladelabel(node = peni_mono, label = "Penicillidia monoceros (collapsed)",offset=0, fontsize = 3, color="black")+
  geom_cladelabel(node = peni_dufo, label = "Penicillidia dufourii (collapsed)",offset=0, fontsize = 3, color="black")+
  geom_cladelabel(node = peni_other, label = "Other Penicillidia (collapsed)",offset=0.065, fontsize = 3, color="black")+
  geom_cladelabel(node = nycterib, label = "All Nycteribia (collapsed)",offset=0.071, fontsize = 3, color="black")+
  geom_cladelabel(node = basil, label = "Basilia (collapsed)",offset=0.075, fontsize = 3, color="black")+
  geom_cladelabel(node = basil2, label = "Basilia (collapsed)",offset=0.045, fontsize = 3, color="black")+
  geom_cladelabel(node = pthrid, label = "Phthiridium (collapsed)",offset=0.035, fontsize = 3, color="black")#+
  # #geom_text(aes(x=0.5,y=62), label="Eucampsipoda sundaica")+
  #xlim(c(0,2)) + ylim(c(0,115))
pa


#here is how collapsing the tree from each node (MRCA) that we are stored from the previous MCRA codes
pa.1 <- collapse(pa,node = eucampsMad_Roumad,mode='max',fill="darkorchid1", color="black") #+ geom_point2(aes(subset=(node==eucampsMad_Roumad1)), size=3, shape=24, fill="darkorchid1")
pa.2 <- collapse(pa.1,node = eucampsAfr_Rouaeg,mode='min',fill="darkorchid1", color="black")
pa.3 <- collapse(pa.2,node = eucampsThe_Rouobl,mode='max',fill="darkorchid1", color="black")
pa.4 <- collapse(pa.3,node = eucampsAfr_Roules,mode='max',fill="darkorchid1", color="black")
pa.5 <- collapse(pa.4,node = cyclopDub_Eidup,mode='max',fill="magenta", color="black")
pa.6 <- collapse(pa.5,node = megaWenz_Roumad,mode='max',fill="purple4", color="black")
pa.7 <- collapse(pa.6,node = brachy_kanoi,mode='max',fill="mediumseagreen", color="black")
pa.8 <- collapse(pa.7,node = cycloPhors_Ptesp,mode='max',fill="magenta", color="black")
pa.9 <- collapse(pa.8,node = streb_Hyp,mode='max',fill="royalblue", color="black")
pa.10 <- collapse(pa.9,node =  peni_ful,mode='max',fill="darkorange3", color="black")
pa.11 <- collapse(pa.10,node =  peni_mono,mode='max',fill="darkorange3", color="black")
pa.12 <- collapse(pa.11,node =  peni_dufo,mode='max',fill="darkorange3", color="black")
pa.13 <- collapse(pa.12,node =  pthrid,mode='max',fill="tomato", color="black")
pa.14 <- collapse(pa.13,node = peni_other,mode='max',fill="darkorange3", color="black")
pa.15 <- collapse(pa.14,node =  nycterib,mode='max',fill="cyan4", color="black")
pa.16 <- collapse(pa.15,node =  basil,mode='max',fill="darkgoldenrod1", color="black")
pa.17 <- collapse(pa.16,node =  basil2,mode='max',fill="darkgoldenrod1", color="black")

#pa.13



#save the plots
ggsave(file = paste0(homewd, "/final-figures/Fig5.png"),
       plot = pa.17,
       units="mm",  
       width=120, 
       height=110, 
       scale=2.6, 
       dpi=300)


