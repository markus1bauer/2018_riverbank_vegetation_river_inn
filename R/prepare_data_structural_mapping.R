# Riverbank vegetation at River Inn
# Prepare spatial data ####
# Markus Bauer
# 2022-02-14



### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
setwd(here("data", "raw"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Load data ###
sites <- read_csv("data_raw_structural_mapping.csv",
  col_names = TRUE, na = "na",
  col_types =
    cols(
      .default = col_double(),
      site = col_factor(),
      treatment = col_factor()
    )
)

### Create variables ###
sites <- sites %>%
  mutate(
    gravelRatio_2014 = gravelArea_2014 / totalArea_2014,
    sandRatio_2014 = sandArea_2014 / totalArea_2014,
    reedRatio_2014 = reedArea_2014 / totalArea_2014,
    tallherbalvegetationRatio_2014 =
      tallherbalvegetationArea_2014 / totalArea_2014,
    gravelRatio_2016 = gravelArea_2016 / totalArea_2016,
    sandRatio_2016 = sandArea_2016 / totalArea_2016,
    reedRatio_2016 = reedArea_2016 / totalArea_2016,
    tallherbalvegetationRatio_2016 =
      tallherbalvegetationArea_2016 / totalArea_2016,
    totalArea_Change = totalArea_2016 - totalArea_2014,
    gravelArea_Change = gravelArea_2016 - gravelArea_2014,
    sandArea_Change = sandArea_2016 - sandArea_2014,
    reedArea_Change = reedArea_2016 - reedArea_2014,
    tallherbalvegetationArea_Change =
      tallherbalvegetationArea_2016 - tallherbalvegetationArea_2014,
    gravelRatio_Change = gravelRatio_2016 - gravelRatio_2014,
    sandRatio_Change = sandRatio_2016 - sandRatio_2014,
    reedRatio_Change = reedRatio_2016 - reedRatio_2014,
    tallherbalvegetationRatio_Change =
      tallherbalvegetationRatio_2016 - tallherbalvegetationRatio_2014
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ####################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


write_csv(sites, here("data", "processed", "data_processed_structural_mapping.csv"))
