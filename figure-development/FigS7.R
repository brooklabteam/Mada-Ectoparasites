### FIGURE S7GENETIC TREE  #####
## 18S RNA

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
#tree <- read.tree(file = paste0(homewd, "/data/phylo/18S-trim-T3.raxml.supportFBP"))
tree <- read.tree(file = paste0(homewd, "/data/phylo/18S-full-T3.raxml.supportFBP"))

head(tree)

class(tree)



#root it on drosophila
rooted.tree <- root(tree, which(tree$tip.label == "M21017_1_D_melanogaster_18S__5_8S_2S_and_28S_rRNA_genes__complete__and_18S_rRNA_gene__5__end__clone_pDm238"))

head(rooted.tree)
#take a quick look in base R
plot(rooted.tree)


#make a data frame
tree.dat <- cbind.data.frame(old_tip_label=rooted.tree$tip.label)

tree.dat$Accession_Number = sapply(strsplit(tree.dat$old_tip_label,"_1_"),"[",1)
#tree.dat$Accession_Number = sapply(strsplit(tree.dat$Accession_Number,"_extraction"),"[",1)

head(tree.dat)


#load tree data prepared from elsewhere
dat <- read.csv(file=paste0(homewd,"/data/phylo/18S_metadata.csv"), header = T, stringsAsFactors = F)
head(dat)
names(dat)
 
#how many individual bats?
length(dat$Accession_Number)#92
#dat$bat_sampleid <- sapply(strsplit(dat$Accession_Number_Old, "_"), '[',1)

#check subgroup names
unique(dat$Genus)
unique(dat$Accession_Number)

#eliminate the extra info
#dat$Accession_Number <- sapply(strsplit(dat$Accession_Number,"."),'[',1)

#and merge

setdiff(tree.dat$Accession_Number, dat$Tip_Link)
setdiff(dat$Tip_Link, tree.dat$Accession_Number)

tree.merge <- merge(tree.dat, dat, by.x="Accession_Number", by.y="Tip_Link", all.x = T, sort = F)

head(tree.merge)
names(tree.merge)[names(tree.merge)=="Accession_Number"] <- "Accession_Plus_Name"
names(tree.merge)[names(tree.merge)=="Accession_Number.y"] <- "Accession_Number"

names(tree.merge)

tree.merge[duplicated(tree.merge$Accession_Number),]

subset(tree.merge, is.na(Country_of_Collection))
sort(unique(tree.merge$Genus))

head(tree.merge)

#make a color vector for each genus
library(scales)
show_col(hue_pal()(25))
colz = c("Ascodipteron" =  "#F8766D", "Basilia"="darkgoldenrod1","Brachyotheca" = "mediumseagreen",  "Cyclopodia"= "magenta",  "Dipseliopoda"="#A3A500",
            "Drosophila" = "black", "Eucampsipoda" = "darkorchid1", "Glossina"="olivedrab1" , "Lipoptena"="yellow2",  "Leptocyclopodia"="darkred",
         "Megastrebla" ="purple4", "Nycteribia"  ="cyan" ,  "Ornithoica"="#00B0F6",  "Ornithomyia"="slategray1", "Penicillidia"="darkorange3",
         "Phthiridium" = "#FF65AC","Raymondia"= "red" , "Trichobius" = "royalblue","Strebla" = "pink" )

#"Brachytarsina" = "mediumseagreen",


   
#pick order for the labels
tree.merge$Genus <- factor(tree.merge$Genus, levels = sort(unique(tree.merge$Genus)))   
head(tree.merge)

length(unique(tree.merge$old_tip_label[tree.merge$Type==1])) #12 - new sequences

#how many of each?
length(unique(tree.merge$old_tip_label[tree.merge$Type==1 & tree.merge$Genus=="Eucampsipoda"])) #5 from 4 bats
length(unique(tree.merge$old_tip_label[tree.merge$Type==1 & tree.merge$Genus=="Megastrebla"])) #3 from 2 bats
length(unique(tree.merge$old_tip_label[tree.merge$Type==1 & tree.merge$Genus=="Cyclopodia"])) #4 from 4 bats
tree.merge$Accession_Plus_Name[tree.merge$Type==1]

#make summary table of the unique bats by species and the ectos sampled from each

TableS3 <- subset(tree.merge, Type==1)
TableS3$bat_sampleid <- sapply(strsplit(TableS3$Accession_Plus_Name,"_"),"[",1)
TableS3 <- dplyr::select(TableS3, Host_Binomial, bat_sampleid, Binomial, Accession_Number)
TableS3 <- arrange(TableS3, Host_Binomial, Binomial, bat_sampleid, Accession_Number)
write.csv(TableS3 , file = paste0(homewd, "/final-tables/tableS2b.csv"), row.names = F)
# 4 Cyclopodia sequenced from 4 Eidolon.
# 5 Eucampsipoda sequenced from 4 Rousettus
# 3 Megastrebla sequenced from 2 Rousettus 


#and  category
tree.merge$Type <- as.character(tree.merge$Type)

tree.merge$Type[tree.merge$Type=="0"] <- "Reference sequence"
tree.merge$Type[tree.merge$Type=="1"] <- "New this study"
tree.merge$Type <- as.factor(tree.merge$Type)

tree.merge$Binomial <- gsub(pattern = "_", replacement = " ", x = tree.merge$Binomial)
tree.merge$Host_Binomial <- gsub(pattern = "_", replacement = " ", x = tree.merge$Host_Binomial)
tree.merge$Country_of_Collection <- gsub(pattern = "_", replacement = " ", x = tree.merge$Country_of_Collection)


tree.merge$new_label <- paste0(tree.merge$Accession_Number, " | ", tree.merge$Binomial, " | ", tree.merge$Host_Binomial, " | ", tree.merge$Year_Sample_Collected, " | ", tree.merge$Country_of_Collection)

#those missing country are also missing host
tree.merge$new_label[is.na(tree.merge$Country_of_Collection)] <- paste0(tree.merge$Accession_Number[is.na(tree.merge$Country_of_Collection)], " | ", tree.merge$Binomial[is.na(tree.merge$Country_of_Collection)], " | ", tree.merge$Year_Sample_Collected[is.na(tree.merge$Country_of_Collection)])

#some are just missing host
tree.merge$new_label[is.na(tree.merge$Host_Binomial) & !is.na(tree.merge$Country_of_Collection)] <- paste0(tree.merge$Accession_Number[is.na(tree.merge$Host_Binomial) & !is.na(tree.merge$Country_of_Collection)], " | ", tree.merge$Binomial[is.na(tree.merge$Host_Binomial) & !is.na(tree.merge$Country_of_Collection)], " | ", tree.merge$Year_Sample_Collected[is.na(tree.merge$Host_Binomial) & !is.na(tree.merge$Country_of_Collection)], " | ", tree.merge$Country_of_Collection[is.na(tree.merge$Host_Binomial) & !is.na(tree.merge$Country_of_Collection)])

head(tree.merge)
tree.merge <- dplyr::select(tree.merge, new_label, old_tip_label, Accession_Number, Genus, Species, Binomial, Host_Binomial, Year_Sample_Collected, Country_of_Collection, Type)
#gut check - looks right
cbind(tree.merge$new_label, tree.merge$old_tip_label)

rooted.tree$tip.label <- tree.merge$new_label
#tree.merge$new_label <- paste0(tree.merge$Accession_Number, " | ", tree.merge$Binomial, " | ", tree.merge$Host_Binomial, " | ", tree.merge$Year_Sample_Collected, " | ", tree.merge$Country_of_Collection)

head(rooted.tree)
head(tree.merge)

#add node shapes to represent bootstrap values
p0<-ggtree(rooted.tree)
p0.dat <- p0$data
p0.dat$Bootstrap <- NA
Bootstrap<-p0.dat$Bootstrap[(length(tree.dat$tip_label)+1):length(p0.dat$label)] <- as.numeric(p0.dat$label[(length(tree.dat$tip_label)+1):length(p0.dat$label)])#fill with label
#p0.dat$Bootstrap[is.na(p0.dat$Bootstrap) & p0.dat$label==""] <- 100 #not sure why the HeV/NiV node has no value

#length(tree.merge$b) tree.merge$Type

shapez = c("New this study" =  24, "Reference sequence" = 22)
colz2 = c('1' =  "yellow", '0' = "white")


#edit a few that are funky:
# #tree.merge$new_label[tree.merge$new_label=="OM327588 | Brachytarsina_kanoi |  | 2022 | Pakistan"] <- "OM327588 | Brachytarsina_kanoi | 2022 | Pakistan"
# tree.merge$new_label[tree.merge$new_label=="OM327589 | Brachytarsina_kanoi |  | 2022 | Pakistan"] <- "OM327589 | Brachytarsina_kanoi | 2022 | Pakistan"
# tree.merge$new_label[tree.merge$new_label=="MH282032 | Basilia_tiptoni | NA | 2013 | Panama"] <- "MH282032 | Basilia_tiptoni | 2013 | Panama"
# tree.merge$new_label[tree.merge$new_label=="NC_001709 | Drosophila_melanogaster | NA | 1999 | USA" ] <- "NC_001709 | Drosophila_melanogaster | 1999 | USA"
# 
# rooted.tree$tip.label[rooted.tree$tip.label=="OM327588 | Brachytarsina_kanoi |  | 2022 | Pakistan"]  <- "OM327588 | Brachytarsina_kanoi | 2022 | Pakistan"
# rooted.tree$tip.label[rooted.tree$tip.label=="OM327589 | Brachytarsina_kanoi |  | 2022 | Pakistan"] <- "OM327589 | Brachytarsina_kanoi | 2022 | Pakistan"
# rooted.tree$tip.label[rooted.tree$tip.label=="MH282032 | Basilia_tiptoni | NA | 2013 | Panama"] <- "MH282032 | Basilia_tiptoni | 2013 | Panama"
# rooted.tree$tip.label[rooted.tree$tip.label=="NC_001709 | Drosophila_melanogaster | NA | 1999 | USA"] <- "NC_001709 | Drosophila_melanogaster | 1999 | USA"



p1 <- ggtree(rooted.tree) %<+% tree.merge + 
  geom_treescale(x=.24,y=15, fontsize = 3) +
  geom_nodepoint(aes(fill=Bootstrap, show.legend = T), shape=21, stroke=.2, size=1.5, color="black")+
  scale_fill_continuous(low="white", high="black", limits=c(0,100))+
  #geom_nodelab(size=1,nudge_x = -.01, nudge_y = .7) +
  ggnewscale::new_scale_fill() +
  geom_tippoint(aes(fill=Genus, color=Genus, shape=Type)) +
  geom_tiplab(size = 1.5)+
  scale_fill_manual(values=colz, guide = guide_legend(order = 1)) + 
  scale_color_manual(values=colz, guide = guide_legend(order = 1)) + 
  scale_shape_manual(values=shapez, guide = guide_legend(order = 3)) + 
  #theme_bw()+
  theme(legend.position = "inside",
        legend.box = "horizontal",
        legend.background = element_rect(fill="transparent"),
        legend.text = element_text(size=4),
        legend.title = element_text(size=5),
        legend.key.height = unit(.6,"lines"),
        legend.position.inside  = c(.78,.18))+ 
  xlim(c(0,.3)) + ylim(c(0,90)) 
  

  
p1

#save the plots (this could go to the supplementary information)
ggsave(file = paste0(homewd, "/final-figures/FigS7.png"),
       plot = p1,
       units="mm",  
       width=110, 
       height=90, 
       scale=2, 
       dpi=300)

#save the plots (this could go to the supplementary information)
ggsave(file = paste0(homewd, "/final-figures/FigS7.pdf"),
       plot = p1,
       units="mm",  
       width=110, 
       height=90, 
       scale=2, 
       dpi=300)


