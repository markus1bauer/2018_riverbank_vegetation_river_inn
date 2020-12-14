## Script to prepare structural mapping data ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
#installr::updateR(browse_news = F, install_R = T, copy_packages = T, copy_Rprofile.site = T, keep_old_packages = T, update_packages = T, start_new_R = F, quit_R = T)
rm(list = ls())
setwd("Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/raw")

### Load data ----------------------------------------------------------------------------------------
sites <- read_csv("data_raw_structural_mapping.csv", col_names = T, na = "na", col_types = 
                    cols(
                      .default = col_double(),
                      site = col_factor(),
                      treatment = col_factor()
                      )        
)


### Create variables ----------------------------------------------------------------------------------
### Calculate ratio of 2014 ###
sites <- mutate(sites, gravelRatio_2014 = gravelArea_2014 / totalArea_2014)
sites <- mutate(sites, sandRatio_2014 = sandArea_2014 / totalArea_2014)
sites <- mutate(sites, reedRatio_2014 = reedArea_2014 / totalArea_2014)
sites <- mutate(sites, tallherbalvegetationRatio_2014 = tallherbalvegetationArea_2014 / totalArea_2014)
### Calculate ratio of 2016 ###
sites <- mutate(sites, gravelRatio_2016 = gravelArea_2016 / totalArea_2016)
sites <- mutate(sites, sandRatio_2016 = sandArea_2016 / totalArea_2016)
sites <- mutate(sites, reedRatio_2016 = reedArea_2016 / totalArea_2016)
sites <- mutate(sites, tallherbalvegetationRatio_2016 = tallherbalvegetationArea_2016 / totalArea_2016)
### Calculate change of areas###
sites <- mutate(sites, totalArea_Change = totalArea_2016 - totalArea_2014)
sites <- mutate(sites, gravelArea_Change = gravelArea_2016 - gravelArea_2014)
sites <- mutate(sites, sandArea_Change = sandArea_2016 - sandArea_2014)
sites <- mutate(sites, reedArea_Change = reedArea_2016 - reedArea_2014)
sites <- mutate(sites, tallherbalvegetationArea_Change = tallherbalvegetationArea_2016 - tallherbalvegetationArea_2014)
### Calculate change of ratios###
sites <- mutate(sites, gravelRatio_Change = gravelRatio_2016 - gravelRatio_2014)
sites <- mutate(sites, sandRatio_Change = sandRatio_2016 - sandRatio_2014)
sites <- mutate(sites, reedRatio_Change = reedRatio_2016 - reedRatio_2014)
sites <- mutate(sites, tallherbalvegetationRatio_Change = tallherbalvegetationRatio_2016 - tallherbalvegetationRatio_2014)

### Save processed data-------------------------------------------------------------------------------
write.table(sites, "Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/processed/data_processed_structural_mapping.csv", sep = ",", row.names = F, quote = F)
