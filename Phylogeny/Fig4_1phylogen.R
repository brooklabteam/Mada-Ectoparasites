### FIGURE 04GENETIC TREE  #####

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
homewd="D:/EKIPA_FANIHY/Ekipa_fanihy_praper_project/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"


#load the tree -- here the trimmed version
tree <- read.tree(file = paste0(homewd, "/Data-and-Code/T3.raxml.supportFBP"))

View(tree)

class(tree)



#root it on drosophila
rooted.tree <- root(tree, which(tree$tip.label == "NC_001709.1_Drosophila_melanogaster_mitochondrion__complete_genome"))

View(rooted.tree)
#take a quick look in base R
plot(rooted.tree)
View(tree)

#make a data frame
tree.dat <- cbind.data.frame(old_tip_label=rooted.tree$tip.label)

tree.dat$Accession_Number = sapply(strsplit(tree.dat$old_tip_label,".1_"),"[",1)
#tree.dat$Accession_Number = sapply(strsplit(tree.dat$Accession_Number,"_extraction"),"[",1)

View(tree.dat)


#load tree data prepared from elsewhere
dat <- read.csv(file=paste0(homewd,"/Data-and-Code/ecto_metadata_combined_update_2024ValBIO.csv"), header = T, stringsAsFactors = F)
View(dat)
names(dat)
#check subgroup names
unique(dat$Genus)
unique(dat$Accession_Number)

#eliminate the extra info
#dat$Accession_Number <- sapply(strsplit(dat$Accession_Number,"."),'[',1)

#and merge

setdiff(tree.dat$Accession_Number, dat$Accession_Number)
setdiff(dat$Accession_Number, tree.dat$Accession_Number)

tree.merge <- merge(tree.dat, dat, by.x="Accession_Number",by.y = "Accession_Number", all.x = T, sort = F)



names(tree.merge)

tree.merge[duplicated(tree.merge$Accession_Number),]
tree.merge <- tree.merge[!duplicated(tree.merge),]
subset(tree.merge, is.na(Country_of_Collection))
unique(tree.merge$Genus)

View(tree.merge)


# Thoses number are duplicated #"AB632571" "MK140180" "KF021535" "MF462046" "MK140104"
# I need to remove the duplication



#make a color vector for each genus
colz = c("Basilia"="darkgoldenrod1", "Brachytarsina" = "mediumseagreen", "Cyclopodia"= "magenta", "Drosophila" = "black", "Eucampsipoda" = "darkorchid1", "Leptocyclopodia"="darkred","Megastrebla" ="purple4", "Nycteribia" = "cyan4", "Penicillidia"="darkorange3", "Phthiridium" = "tomato",  "Streblidae" = "royalblue")# "Trichobius" = "purple4",  )

#pick order for the labels
tree.merge$Genus <- factor(tree.merge$Genus, levels = sort(unique(tree.merge$Genus)))   
View(tree.merge)
#tree.merge[31,]


#and  category
tree.merge$Type <- as.character(tree.merge$Type)

tree.merge$Type[tree.merge$Type=="0"] <- "Reference sequence"
tree.merge$Type[tree.merge$Type=="1"] <- "New this study"
tree.merge$Type <- as.factor(tree.merge$Type)
tree.merge$new_label <- paste0(tree.merge$Accession_Number, " | ", tree.merge$Binomial, " | ", tree.merge$Host_Binomial, " | ", tree.merge$Year_Sample_Collected, " | ", tree.merge$Country_of_Collection)

tree.merge <- dplyr::select(tree.merge, new_label, names(tree.merge)[1:11])

rooted.tree$tip.label <- tree.merge$new_label
tree.merge$new_label <- paste0(tree.merge$Accession_Number, " | ", tree.merge$Binomial, " | ", tree.merge$Host_Binomial, " | ", tree.merge$Year_Sample_Collected, " | ", tree.merge$Country_of_Collection)

View(rooted.tree)
View(tree.merge)

shapez = c("New this study" =  24, "Reference sequence" = 16)
colz2 = c('1' =  "yellow", '0' = "white")

rooted.tree$node.label <- as.numeric(rooted.tree$node.label)
rooted.tree$node.label[rooted.tree$node.label<50] <- NA



#Get the clad numbers to known the clads that we are going  collapse

p1 <- ggtree(rooted.tree) %<+% tree.merge + 
  geom_nodelab(size=1.5,nudge_x = -.01, nudge_y = .7) +
  geom_tippoint(aes(fill=Genus, color=Genus, shape=Type)) +
  geom_tiplab(size = 1.5)+
  scale_fill_manual(values=colz) + 
  scale_color_manual(values=colz) + 
  scale_shape_manual(values=shapez) + 
  geom_treescale(x=.05,y=35)+
  #theme_bw()+
  theme(legend.position = c(.15,.75))+ 
  xlim(c(0,.49)) #+ ylim(c(0,160))
p1

#save the plots (this could go to the supplementary information)
ggsave(file = paste0(homewd, "/Phylogeny/S4_phylogenetic_tree_allxxx.png"),
       plot = p1,
       units="mm",  
       width=110, 
       height=90, 
       scale=2.5, 
       dpi=300)

#then go back and change the manual zhape names



## Here, we are going to collapse the samples with the More Recent Commons Ancestor (MRCA) ####
#first check from the previous plot all of the first and the last tiplabel sharing the same RCA
#then let check the name with the following code to avoid the miss writing
rooted.tree$tip.label

# Now lets try to find the common ancestor for each clade and stor it one object
eucampsMad_Roumad <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OR732293 | Eucampsipoda_madagascariensis | Rousettus_madagascariensis | NA | Madagascar"),which(rooted.tree$tip.label == "OR732245 | Eucampsipoda_madagascariensis | Rousettus_madagascariensis | NA | Madagascar"),method="phylo")
eucampsAfr_Rouaeg <- MRCA(rooted.tree, which(rooted.tree$tip.label == "MH151066 | Eucampsipoda_africana | Rousettus_aegyptiacus | 2018 | Nigeria" ),which(rooted.tree$tip.label == "MH151063 | Eucampsipoda_africana | Rousettus_aegyptiacus | 2018 | Nigeria"),method="phylo")
eucampsThe_Rouobl <- MRCA(rooted.tree, which(rooted.tree$tip.label == "KF021498 | Eucampsipoda_theodori | Rousettus_obliviosus | 2010 | Comoros" ),which(rooted.tree$tip.label == "KF021500 | Eucampsipoda_theodori | Rousettus_obliviosus | 2010 | Comoros"),method="phylo")
eucampsAfr_Roules <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OM283590 | Eucampsipoda_africana | Rousettus_leschenaultii | 2019 | Pakistan"),which(rooted.tree$tip.label =="OM283592 | Eucampsipoda_africana | Rousettus_leschenaultii | 2019 | Pakistan"),method="phylo")
cyclopDub_Eidup   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OR732304 | Cyclopodia_dubia | Eidolon_dupreanum | NA | Madagascar"),which(rooted.tree$tip.label == "MF462043 | Cyclopodia_dubia | Eidolon_dupreanum | 2017 | Madagascar"),method="phylo")
cycloPhors_Ptesp   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "KF273770 | Cyclopodia_horsefieldi | Pteropus_hypomelanus | 2006 | Malaysia" ),which(rooted.tree$tip.label == "KF273782 | Cyclopodia_horsefieldi | Pteropus_vampyrus | 2004 | Malaysia" ),method="phylo")
streb_Hyp   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "MW792204 | Streblidae_spp | Hipposideros_ruber | 2017 | Uganda"),which(rooted.tree$tip.label == "MW792205 | Streblidae_spp | Hipposideros_ruber | 2017 | Uganda"  ),method="phylo")


megaWenz_Roumad   <- MRCA(rooted.tree, which(rooted.tree$tip.label == "OR732258 | Megastrebla_wenzeli | Rousettus_madagascariensis | NA | Madagascar"),which(rooted.tree$tip.label == "OR732302 | Megastrebla_wenzeli | Rousettus_madagascariensis | NA | Madagascar"))
brachy_kanoi      <- MRCA(rooted.tree,which(rooted.tree$tip.label=="OM327589 | Brachytarsina_kanoi |  | 2022 | Pakistan"), which(rooted.tree$tip.label=="OM327588 | Brachytarsina_kanoi |  | 2022 | Pakistan"))
peni_ful   <- MRCA(rooted.tree,which(rooted.tree$tip.label== "ON704710 | Penicillidia_fulvida | Miniopterus_spp | 2015 | Kenya"), which(rooted.tree$tip.label=="ON704664 | Penicillidia_fulvida | Rhinolophus_fumigatus | 2015 | Kenya" ))
peni_mono  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "AB632567 | Penicillidia_monoceros | Myotis_daubentonii | 2011 | Japan"), which(rooted.tree$tip.label=="MW590972 | Penicillidia_monoceros | bird | 2021 | Finland"))
peni_dufo  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "MK140181 | Penicillidia_dufourii | Myotis_myotis | 2018 | Romania" ), which(rooted.tree$tip.label=="MK140183 | Penicillidia_dufourii | Myotis_blythii | 2018 | Hungary"))
peni_cons  <- MRCA(rooted.tree,which(rooted.tree$tip.label== "MK140180 | Penicillidia_conspicua | Miniopterus_schreibersii | 2018 | Romania"), which(rooted.tree$tip.label=="MK140180 | Penicillidia_conspicua | Miniopterus_schreibersii | 2018 | Romania"))
 
#Make a simple tree with out the tip.label and not using the tree.merge data
#this will be the base befor the collapse


pa<- ggtree(rooted.tree)+ 
  geom_tippoint()+
  #Label for the clades to be colapsed 
  geom_cladelabel(node = eucampsMad_Roumad,label = "Eucampsipoda madagascariensis| Rousettus madagascariens",offset=0, fontsize = 4, color="black") +
  geom_cladelabel(node = eucampsAfr_Rouaeg, label = "Eucampsipoda africana | Rousettus aegyptiacus",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = eucampsThe_Rouobl, label = "Eucampsipoda theodori | Rousettus obliviosus",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = eucampsAfr_Roules, label = "Eucampsipoda africana | Rousettus leschenaultii",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = cyclopDub_Eidup, label = "Cyclopodia dubia| Eidolon dupreanum",offset=0, fontsize =4, color="black")+
  geom_cladelabel(node = cycloPhors_Ptesp, label = "Cyclopodia horsefieldi| Pteropus spp.",offset=0, fontsize =4, color="black")+
  geom_cladelabel(node = megaWenz_Roumad, label = "Megastrebla wenzeli | Rousettus madagascariens",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = brachy_kanoi, label = "Brachytarsina sp | 2022 | Pakistan & Korea",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = streb_Hyp, label = "Streblidae_sp|Hipposideros ruber",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = peni_ful, label = "Penicillidia fulvida",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = peni_mono, label = "Penicillidia monoceros",offset=0, fontsize = 4, color="black")+
  geom_cladelabel(node = peni_dufo, label = "Penicillidia dufourii",offset=0, fontsize = 4, color="black")+
  #geom_text(aes(x=0.5,y=62), label="Eucampsipoda sundaica")+
  
  
  xlim(c(0,2)) + ylim(c(0,115))
pa


#here is how collapsing the tree from each node (MRCA) that we are stored from the previous MCRA codes
pa.1 <- collapse(pa,node = eucampsMad_Roumad,mode='max',fill="transparent")
pa.2 <- collapse(pa.1,node = eucampsAfr_Rouaeg,mode='min',fill="transparent")
pa.3 <- collapse(pa.2,node = eucampsThe_Rouobl,mode='max',fill="transparent")
pa.4 <- collapse(pa.3,node = eucampsAfr_Roules,mode='max',fill="transparent")
pa.5 <- collapse(pa.4,node = cyclopDub_Eidup,mode='max',fill="transparent")
pa.6 <- collapse(pa.5,node = megaWenz_Roumad,mode='max',fill="transparent")
pa.7 <- collapse(pa.6,node = brachy_kanoi,mode='max',fill="transparent")
pa.8 <- collapse(pa.7,node = cycloPhors_Ptesp,mode='max',fill="transparent")
pa.9 <- collapse(pa.8,node = streb_Hyp,mode='max',fill="transparent")
pa.10 <- collapse(pa.9,node =  peni_ful,mode='max',fill="transparent")
pa.11 <- collapse(pa.10,node =  peni_mono,mode='max',fill="transparent")
pa.12 <- collapse(pa.11,node =  peni_dufo,mode='max',fill="transparent")

pa.12



pb <- pa.12 %<+% tree.merge+ 
  #geom_nodelab(size=5,nudge_x = -.02, nudge_y = .7) +
  geom_tippoint(aes(fill=Genus, color=Genus,shape=Type),size=3) +
  geom_tiplab(size = 4,
              #align=TRUE,
              #linesize=0,
              #linetype='dashed'
              )+
  scale_fill_manual(values=colz) +
  scale_color_manual(values=colz) + 
  scale_shape_manual(values=shapez, labels=c("New this study"="Collapsed", "Reference sequence"="Not collapsed")) + 
  #geom_treescale(x=.05,y=65)+
  #theme_bw()+
  theme(legend.position = c(.1,.75)) +
  xlim(c(0,.8)) + ylim(c(0,82))+
  
  #colz = c("Basilia"="darkgoldenrod1", "Brachytarsina" = "mediumseagreen" , "Cyclopodia"= "magenta", "Drosophila" = "black", "Eucampsipoda" = "darkorchid1", "Leptocyclopodia"="darkred","Megastrebla" ="purple4", "Nycteribia" = "cyan4", "Penicillidia"="darkorange3", "Phthiridium" = "tomato",  "Streblidae" = "royalblue")# "Trichobius" = "purple4",  )
  
  geom_point2(aes(subset=(node==eucampsMad_Roumad)), shape=17, size=3, fill="darkorchid1", color="darkorchid1")+
  geom_point2(aes(subset=(node==eucampsAfr_Rouaeg)), shape=17, size=3, fill="darkorchid1", color="darkorchid1")+
  geom_point2(aes(subset=(node==eucampsThe_Rouobl)), shape=17, size=3, fill="darkorchid1", color="darkorchid1")+
  geom_point2(aes(subset=(node==eucampsAfr_Roules)), shape=17, size=3, fill="darkorchid1", color="darkorchid1")+
  geom_point2(aes(subset=(node==cyclopDub_Eidup)), shape=17, size=3, fill="magenta", color="magenta") +
 # geom_point2(aes(subset=(node==brachy_kanoi)), shape=17, size=3, fill="#FF61CC", color="#FF61CC")+
  geom_point2(aes(subset=(node==cycloPhors_Ptesp)), shape=17, size=3, fill="magenta", color="magenta")+
  geom_point2(aes(subset=(node==brachy_kanoi)), shape=17, size=3, fill="mediumseagreen", color="mediumseagreen")+
  geom_point2(aes(subset=(node==streb_Hyp)), shape=17, size=3, fill="#FF61CC", color="#FF61CC")+
  geom_point2(aes(subset=(node==megaWenz_Roumad)), shape=17, size=3, fill="purple4", color="purple4")+
  geom_point2(aes(subset=(node==peni_mono)), shape=17, size=3, fill="darkorange3", color="darkorange3")+
  geom_point2(aes(subset=(node==peni_ful)), shape=17, size=3, fill="darkorange3", color="darkorange3")+
  geom_point2(aes(subset=(node==peni_dufo)), shape=17, size=3, fill="darkorange3", color="darkorange3")
  #geom_point2(aes(subset=(node==peni_cons)), shape=17, size=3, fill="#FF61CC", color="#FF61CC")



pb



#save the plots
ggsave(file = paste0(homewd, "/Phylogeny/Fig4-angelo_colapsp1.png"),
       plot = pb,
       units="mm",  
       width=120, 
       height=95, 
       scale=2.7, 
       dpi=300)


