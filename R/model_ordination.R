# Model for ordination ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/processed")

### Load data ###
sites <- read_csv("data_processed_sites.csv", col_names = T, na = "na", col_types = 
                    cols(
                      .default = col_double(),
                      plotTemp = col_factor(),
                      plot = col_factor(),
                      block = col_factor(),
                      year = col_factor(levels = c("Control","2014","2016")),
                      treatment = col_factor(levels = c("Gravel supply","Sand supply","Embankment removal")),
                      habitatType = col_factor(),
                      substrate = col_factor()
                    )        
)

(sites <- select(sites, no, plotTemp, plot, block, year, barrierDist, treatment, habitatType, herbHeight, herbCover, soilCover, speciesRich, shannon))
sites <- filter(sites, treatment != "Embankment removal" & no != "83")

species <- read_csv("data_processed_species.csv", col_names = T, na = "na", col_types = 
                    cols(
                      .default = col_double(),
                      name = col_factor(),
                      abb = col_factor()
                    )        
)
species <- select(species, !(name))
species <- column_to_rownames(species, "abb")
species <- species[,c(16:45,61:90,106:135)]
species <- species[rowSums(species) > 0, colSums(species) > 0]
species <- as_tibble(t(species))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 NMDS #####################################################################################

#### a ordination ----------------------------------------------------------------------------------------
dist <- wisconsin(sqrt(species))
dist <- vegdist(species, method = "bray", binary = F, na.rm = T)
(ordi <- metaMDS(dist, try = 99, previous.best = T, na.rm = T))
stressplot(ordi) # stress: 0.12

#### b environmental factors ----------------------------------------------------------------------------------------
(ef <- envfit(ordi ~ herbHeight + herbCover + soilCover + barrierDist, 
              data = sites, permu = 999, na.rm = T))
plot(ordi, type = "n")
plot(ef, add = T, p. = .05)
text(ordi, dis = "sites", cex = .7)
ordiellipse(ordi, sites$year, kind = "sd", draw = "lines", label = T)
plot(ordi, type = "n")
plot(ef, add = T, p. = .05)
text(ordi, dis = "sites", cex = .7)
ordiellipse(ordi, sites$treatment, kind = "sd", draw = "lines", label = T)


### 2 PERMANOVA #####################################################################################

#### a year ----------------------------------------------------------------------------------------
(disp.year <- betadisper(dist, sites$year))
permutest(disp.year) # similar dispersion -> PERMANOVA possible
(permanova.year <- adonis(species ~ year, data = sites, 
                          strata = sites$block, permutations = 999, method = "bray"))
densityplot(permustats(permanova.year)) # significant

#### b treatment ----------------------------------------------------------------------------------------
(disp.treat <- betadisper(dist, sites$treatment))
permutest(disp.treat) # different dispersion -> PERMANOVA not possible
(permanova.treat <- adonis(species ~ treatment, data = sites, 
                           strata = sites$block, permutations = 999, method = "bray"))
densityplot(permustats(permanova.treat)) # n.s.
