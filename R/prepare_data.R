## Script to prepare site data ###



### Packages ###
library(tidyverse)
library(vegan)

### Start ###
#installr::updateR(browse_news = F, install_R = T, copy_packages = T, copy_Rprofile.site = T, keep_old_packages = T, update_packages = T, start_new_R = F, quit_R = T)
rm(list = ls())
setwd("Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/raw")


### 1 Load data #####################################################################################

sites <- read_csv("data_raw_sites.csv", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          plotTemp = col_factor(),
                          plot = col_factor(),
                          block = col_factor(),
                          year = col_factor(levels = c("Control","2014","2016")),
                          site = col_factor(),
                          date = col_date(),
                          barrier = col_factor(),
                          treatment = col_factor(levels = c("Gravel supply","Sand supply","Embankment removal")),
                          habitatType = col_factor(),
                          substrate = col_factor(),
                          slope = col_factor()
                        )        
)

(sites <- select(sites, no, plotTemp, plot, block, year, barrierriverKm, plotriverKm, treatment, habitatType, shrubHeight, shrubCover, herbHeight, herbCover, mossCover, soilCover, substrate))

species <- read_csv("data_raw_species.csv", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         name = col_factor(),
                         abb = col_factor()
                       )        
)

traits <- read_csv("data_raw_traits.csv", col_names = T, na = "na", col_types = 
                      cols(
                        .default = col_double(),
                        name = col_factor(),
                        descriptor = col_factor(),
                        abb = col_factor(),
                        family = col_factor(),
                        lifeform = col_factor(),
                        growthform = col_factor(),
                        sociology = col_factor(),
                        typ = col_factor(),
                        rlg = col_factor(),
                        rlb = col_factor(),
                        neo = col_factor()
                      )        
)

(traits <- select(traits, name, abb, lifeform, growthform, n, f, typ, neo))


### 2 Create variables #####################################################################################

### Variable: dummies for confidence interval
sites$conf.low <- c(1:135)
sites$conf.high <- c(1:135)


### a Barrier distance -------------------------------------------------------------------------------------------
sites$barrierDist <- sites$plotriverKm - sites$barrierriverKm
sites <- select(sites, -plotriverKm, -barrierriverKm)

### b species richness -------------------------------------------------------------------------------------------
diversity <- column_to_rownames(species, "name")
diversity <- t(select(diversity, -abb))
sites$shannon <- diversity(diversity, index = "shannon")
sites$simpson <- diversity(diversity, index = "simpson")
sites$speciesRich <- specnumber(diversity)
rm(diversity)

### c species richness of all life forms -------------------------------------------------------------------------------------------
lifeform <- select(traits, name, lifeform)

therophytes <- inner_join(species, filter(lifeform, lifeform == "T" | lifeform == "H_T"), by = "name")
perennials <- inner_join(species, filter(lifeform, lifeform == "N"), by = "name")
wood <- inner_join(species, filter(lifeform, lifeform == "P"), by = "name")

therophytes <- select(therophytes, -lifeform, -abb)
perennials <- select(perennials, -lifeform, -abb)
wood <- select(wood, -lifeform, -abb)

therophytes <- column_to_rownames(therophytes, "name")
perennials <- column_to_rownames(perennials, "name")
wood <- column_to_rownames(wood, "name")

sites$Therophytes <- specnumber(t(therophytes))
sites$Perennials <- specnumber(t(perennials))
sites$Wood <- specnumber(t(wood))

rm(therophytes, perennials, wood, lifeform)

### d target species cover -------------------------------------------------------------------------------------------
typ <- select(traits, name, typ)

reed <- inner_join(species, filter(typ, typ == "Reed"), by = "name")
sand <- inner_join(species, filter(typ, typ == "Sand"), by = "name")
gravel <- inner_join(species, filter(typ, typ == "Gravel"), by = "name")

reed <- select(reed, -typ, -abb)
sand <- select(sand, -typ, -abb)
gravel <- select(gravel, -typ, -abb)

reed <- column_to_rownames(reed, "name")
sand <- column_to_rownames(sand, "name")
gravel <- column_to_rownames(gravel, "name")

sites$reedCover <- colSums(reed)
sites$sandCover <- colSums(sand)
sites$gravelCover <- colSums(gravel)

rm(reed, sand, gravel, typ)

sites$aimCover <- sites$reedCover + sites$sandCover + sites$gravelCover


### 3 Save processed data #####################################################################################

write.table(species, "Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/processed/data_processed_species.csv", sep = ",", row.names = F, quote = F)
write.table(sites, "Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/processed/data_processed_sites.csv", sep = ",", row.names = F, quote = F)
write.table(traits, "Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/processed/data_processed_traits.csv", sep = ",", row.names = F, quote = F)
