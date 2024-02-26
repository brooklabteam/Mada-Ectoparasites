rm(list=ls())

library(ggplot2)
library(ggtree)
library(plyr)
library(dplyr)
library(ape)
library(ggnewscale)


# angelo, add a line where you define 'homewd' as the wd on your own computer
homewd="/Users/carabrook/Developer/Mada-Ectoparasites/"

#load the tree -- here the trimmed version
tree <- read.tree(file = paste0(homewd, "/Phylogeny/phylo-trimmed/T3.raxml.supportFBP"))
#and angelo's version
tree <- read.tree(file = paste0(homewd, "Phylogeny/phylo-trimmed/T3.raxml.supportFBP.angelo.supportFBP"))
#root it on drosophila
rooted.tree <- root(tree, which(tree$tip.label == "NC_001709_1_Drosophila_melanogaster_mitochondrion__complete_genome_extraction"))
#take a quick look in base R
plot(rooted.tree)

#make a data frame
tree.dat <- cbind.data.frame(old_tip_label=rooted.tree$tip.label)
tree.dat$Accession_Number = sapply(strsplit(tree.dat$old_tip_label,"_1_"),"[",1)
tree.dat$Accession_Number = sapply(strsplit(tree.dat$Accession_Number,"_extraction"),"[",1)

head(tree.dat)

#load tree data prepared from elsewhere
dat <- read.csv(file=paste0(homewd,"/Phylogeny/ecto_metadata_combined.csv"), header = T, stringsAsFactors = F)
head(dat)
#check subgroup names
unique(dat$Genus)
unique(dat$Accession_Number)

#eliminate the extra info
#dat$Accession_Number <- sapply(strsplit(dat$Accession_Number,"-"),'[',1)

#and merge
setdiff(tree.dat$Accession_Number, dat$Accession_Number)
setdiff(dat$Accession_Number, tree.dat$Accession_Number)

tree.merge <- merge(tree.dat, dat, by="Accession_Number", all.x = T, sort = F)

tree.merge[duplicated(tree.merge$Accession_Number),]
tree.merge <- tree.merge[!duplicated(tree.merge),]
subset(tree.merge, is.na(Country_of_Collection))
unique(tree.merge$Genus)
#make a color vector for each genus
colz = c("Basilia"="darkgoldenrod1", "Brachytarsina" = "mediumseagreen", "Cyclopodia"= "magenta", "Drosophila" = "black", "Eucampsipoda" = "darkorchid1", "Leptocyclopodia"="darkred","Megastrebla" ="purple4", "Nycteribia" = "cyan4", "Penicillidia"="darkorange3", "Phthiridium" = "tomato",  "Streblidae" = "royalblue")# "Trichobius" = "purple4",  )

#pick order for the labels
tree.merge$Genus <- factor(tree.merge$Genus, levels = sort(unique(tree.merge$Genus)))   

#and  category
tree.merge$Type <- as.character(tree.merge$Type)
tree.merge$Type[tree.merge$Type=="0"] <- "Reference sequence"
tree.merge$Type[tree.merge$Type=="1"] <- "New this study"
tree.merge$Type <- as.factor(tree.merge$Type)
tree.merge$new_label <- paste0(tree.merge$Accession_Number, " | ", tree.merge$Binomial, " | ", tree.merge$Host_Binomial, " | ", tree.merge$Year_Sample_Collected, " | ", tree.merge$Country_of_Collection)

tree.merge <- dplyr::select(tree.merge, new_label, names(tree.merge)[1:9])
rooted.tree$tip.label <- tree.merge$new_label

#shapez = c("1" =  24, "0" = 21)
#colz2 = c('1' =  "yellow", '0' = "white")
rooted.tree$node.label <- as.numeric(rooted.tree$node.label)
rooted.tree$node.label[rooted.tree$node.label<50] <- NA

p1 <- ggtree(rooted.tree) %<+% tree.merge + 
  geom_nodelab(size=1.5,nudge_x = -.01, nudge_y = .7) +
  geom_tippoint(aes(fill=Genus, color=Genus, shape=Type)) +
  geom_tiplab(size = 1.5)+
  scale_fill_manual(values=colz) + 
  scale_color_manual(values=colz) + 
  #scale_shape_manual(values=shapez) + 
  geom_treescale(x=.05,y=65)+
  theme(legend.position = c(.15,.75)) +
  xlim(c(0,.8)) + ylim(c(0,160))
p1



ggsave(file = paste0(homewd, "/Phylogeny/Fig4-angelo.png"),
       plot = p1,
       units="mm",  
       width=110, 
       height=90, 
       scale=2.5, 
       dpi=300)

