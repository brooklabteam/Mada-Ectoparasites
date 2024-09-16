library(tidyverse)
library(ggalluvial)


# Set working drive
setwd("C:/Users/kathe/Documents/Madagascar_ectos/")

#######################################  Overall Alluvial  ######################################


# reads in the blood meal data set
data <- read.csv("20200628_AA_SG_Ectoparasite_Species.csv")

# creates a new dataframe with a new column with the proportion of blood meals detected
alluvial <- data %>%
  as.data.frame() %>%
  mutate(Ecto_type = recode(Ecto_type, Batflies = "Bat fly")) %>% 
  select(Bat_species, Ecto_type) %>%
  dplyr::filter(!is.na(Ecto_type)) %>%
  dplyr::filter(Ecto_type != "Egg") %>%
  mutate(Bat_species = as.factor(Bat_species),
         Ecto_type = as.factor(Ecto_type)) %>%
  count(Bat_species, Ecto_type)
  
Plot <- ggplot(alluvial, aes(y = n, axis1 = Bat_species, axis2 = Ecto_type)) +
  geom_alluvium(aes(fill = Ecto_type), width =1/5, knot.pos = 0, na.rm=FALSE) +
  geom_stratum(width = 1/5,  color = "black", alpha = 0) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) # makes it really hard to tell that Pteropus rufus  only has 2 samples which are mites. looks like they have bat flies too. Need to play with labels and box sizes.
Plot


# Resources
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html