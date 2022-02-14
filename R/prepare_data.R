# Riverbank vegetation at River Inn
# Prepare sites, species and traits data ####
# Markus Bauer
# 2022-02-14



### Packages ###
library(here)
library(tidyverse)
library(WorldFlora)
library(vegan)
library(styler)

### Start ###
# installr::updateR(browse_news = FALSE, install_R = TRUE, copy_packages = TRUE, copy_Rprofile.site = TRUE, keep_old_packages = TRUE, update_packages = TRUE, start_new_R = FALSE, quit_R = TRUE)
rm(list = ls())
setwd(here("data", "raw"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Load data #############################################################

sites <- read_csv("data_raw_sites.csv",
  col_names = TRUE, na = "na",
  col_types =
    cols(
      .default = col_double(),
      plotTemp = col_factor(),
      plot = col_factor(),
      block = col_factor(),
      year = col_factor(levels = c("Control", "2014", "2016")),
      site = col_factor(),
      date = col_date(),
      barrier = col_factor(),
      treatment = col_factor(levels = c(
        "Gravel supply", "Sand supply",
        "Embankment removal"
      )),
      habitatType = col_factor(),
      substrate = col_factor(),
      slope = col_factor()
    )
) %>%
  select(
    no, plotTemp, plot, block, year, barrierriverKm, plotriverKm, treatment, habitatType,
    shrubHeight, shrubCover, herbHeight, herbCover, mossCover, soilCover, substrate
  )

species <- read_csv("data_raw_species.csv",
  col_names = TRUE, na = "na",
  col_types =
    cols(
      .default = col_double(),
      name = col_factor(),
      abb = col_factor()
    )
)

traits <- read_csv("data_raw_traits.csv",
  col_names = TRUE, na = "na",
  col_types =
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
) %>%
  select(name, abb, lifeform, growthform, n, f, typ, neo)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Simple variables ######################################################

sites <- sites %>%
  mutate(
    conf.low = seq_along(plot),
    conf.high = seq_along(plot),
    barrier_distance = plotriverKm - barrierriverKm
  ) %>%
  select(-plotriverKm, -barrierriverKm)


### 2 Species richness ######################################################

diversity <- species %>%
  column_to_rownames("name") %>%
  select(-abb) %>%
  t()
sites <- sites %>%
  mutate(
    shannon = diversity(diversity, index = "shannon"),
    simpson = diversity(diversity, index = "simpson"),
    speciesrichness = specnumber(diversity)
  )


### 3 Life forms ###########################################################

lifeform <- select(traits, name, lifeform)
therophytes <- species %>%
  inner_join(filter(lifeform, lifeform == "T" | lifeform == "H_T"), by = "name") %>%
  select(-lifeform, -abb) %>%
  column_to_rownames("name")
perennials <- species %>%
  inner_join(filter(lifeform, lifeform == "N"), by = "name") %>%
  select(-lifeform, -abb) %>%
  column_to_rownames("name")
wood <- species %>%
  inner_join(filter(lifeform, lifeform == "P"), by = "name") %>%
  select(-lifeform, -abb) %>%
  column_to_rownames("name")
sites <- sites %>%
  mutate(
    therophytes = specnumber(t(therophytes)),
    perennials = specnumber(t(perennials)),
    wood = specnumber(t(wood))
  )


### 4 Target species cover #################################################

typ <- select(traits, name, typ)
reed <- species %>%
  inner_join(filter(typ, typ == "Reed"), by = "name") %>%
  select(-typ, -abb) %>%
  column_to_rownames("name")
sand <- species %>%
  inner_join(filter(typ, typ == "Sand"), by = "name") %>%
  select(-typ, -abb) %>%
  column_to_rownames("name")
gravel <- species %>%
  inner_join(filter(typ, typ == "Gravel"), by = "name") %>%
  select(-typ, -abb) %>%
  column_to_rownames("name")
sites <- sites %>%
  mutate(
    reed_cover = colSums(reed),
    sand_cover = colSums(sand),
    gravel_cover = colSums(gravel),
    aim_cover = reed_cover + sand_cover + gravel_cover
  )

rm(list = setdiff(ls(), c("sites", "species", "traits")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ####################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


write_csv(species, here("data", "processed", "data_processed_species.csv"))
write_csv(sites, here("data", "processed", "data_processed_sites.csv"))
write_csv(traits, here("data", "processed", "data_processed_traits.csv"))
