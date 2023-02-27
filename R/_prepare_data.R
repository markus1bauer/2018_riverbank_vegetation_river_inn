# Riverbank vegetation at River Inn
# Prepare sites, species and traits data ####
# Markus Bauer
# 2022-02-14



### Packages ###
library(here)
library(tidyverse)
library(sf)
library(vegan)

### Start ###
renv::status()
rm(list = ls())



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### 1 Load data ###############################################################


sites <- read_csv(
  here("data", "raw", "data_raw_sites.csv"),
  col_names = TRUE,
  na = "na",
  col_types =
    cols(
      .default = "?",
      year = col_factor(levels = c("Control", "2014", "2016")),
      treatment = col_factor(
        levels = c(
          "Gravel supply",
          "Sand supply",
          "Embankment removal"
          )
        )
      )
)

species <- read_csv(
  here("data", "raw", "data_raw_species.csv"),
  col_names = TRUE,
  na = "na",
  col_types =
    cols(
      .default = "d",
      name = "f"
    )
)

traits <- read_csv(
  here("data", "raw", "data_raw_traits.csv"),
  col_names = TRUE,
  na = "na",
  col_types =
    cols(
      .default = "?",
      sociology = "d"
    )
) 



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ##########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### 1 Simple variables ########################################################


sites <- sites %>%
  mutate(
    conf.low = seq_along(plot),
    conf.high = seq_along(plot),
    barrier_distance = plotriverKm - barrierriverKm
  )

### Transform variables ###
sites <- left_join(
  sites %>%
    select(-easting, -northing),
  sites %>%
    st_as_sf(coords = c("easting", "northing"), crs = 31468) %>%
    st_transform(crs = 4326) %>%
    sfheaders::sf_to_df() %>%
    rename(lat = y, lon = x) %>%
    rename(no = sfg_id),
  by = "no"
)
  
### Associate each species with plant sociological ID ###
traits <- traits %>%
  separate(name, into = c("genus", "species", "x", "subspecies"), sep = "_",
           remove = FALSE, extra = "merge", fill = "right") %>%
  mutate(
    genus = str_extract(genus, "\\w{3}"),
    species = str_extract(species, "\\w{5}"),
    subspecies = str_extract(subspecies, "\\w{4}")
    ) %>%
  unite(col = "abb", genus, species, subspecies, sep = "",
        remove = TRUE, na.rm = TRUE) %>%
  mutate(
    sand = if_else(
      # Isoeto-Nanojuncetea (Zwergbinsen-Teichboden-Ges.)
      sociology >= 3100 & sociology < 3200, "sand", if_else(
        # Chenopodion rubri
        sociology == 3212, "sand", if_else(
          # Agrostietea stoloniferae (Flutrasen und Feuchtweiden)
          sociology >= 3800 & sociology < 3900, "sand", NA_character_
          )
        )
      ),
    gravel = if_else(
      # Epilobietalia fleischeri & Achnatheretalia
      sociology >= 4440 & sociology < 4460, "gravel", NA_character_
      ),
    reed = if_else(
      # Phragmitetea (Röhrichte und Seggenrieder)
      sociology >= 1500 & sociology < 1600, "reed", NA_character_
      ),
    tall_herbal_vegetation = if_else(
      # Calystegietalia
      sociology >= 3520 & sociology < 3530, "tall_herbal_vegetation", NA_character_
    )
  ) %>%
  unite(
    col = "typ", sand, gravel, reed, tall_herbal_vegetation,
    sep = "_", remove = TRUE, na.rm = TRUE
    )



### 2 Species richness ########################################################


diversity <- species %>%
  column_to_rownames("name") %>%
  t()
sites <- sites %>%
  mutate(
    shannon = diversity(diversity, index = "shannon"),
    simpson = diversity(diversity, index = "simpson"),
    speciesrichness = specnumber(diversity)
  )



### 3 Life forms ##############################################################


lifeform <- traits %>%
  select(name, lifeform)
therophytes <- species %>%
  inner_join(filter(lifeform, lifeform == "T" | lifeform == "H_T"), by = "name") %>%
  select(-lifeform) %>%
  column_to_rownames("name")
perennials <- species %>%
  inner_join(filter(lifeform, lifeform == "N"), by = "name") %>%
  select(-lifeform) %>%
  column_to_rownames("name")
wood <- species %>%
  inner_join(filter(lifeform, lifeform == "P"), by = "name") %>%
  select(-lifeform) %>%
  column_to_rownames("name")
sites <- sites %>%
  mutate(
    therophytes = specnumber(t(therophytes)),
    perennials = specnumber(t(perennials)),
    wood = specnumber(t(wood))
  )


### 4 Target species cover ####################################################

typ <- traits %>%
  select(name, typ)
reed <- species %>%
  inner_join(filter(typ, typ == "reed"), by = "name") %>%
  select(-typ) %>%
  column_to_rownames("name")
sand <- species %>%
  inner_join(filter(typ, typ == "sand"), by = "name") %>%
  select(-typ) %>%
  column_to_rownames("name")
gravel <- species %>%
  inner_join(filter(typ, typ == "gravel"), by = "name") %>%
  select(-typ) %>%
  column_to_rownames("name")
sites <- sites %>%
  mutate(
    reed_cover = colSums(reed),
    sand_cover = colSums(sand),
    gravel_cover = colSums(gravel)
  )

rm(list = setdiff(ls(), c("sites", "species", "traits")))



### 5 Final selection of variables #############################################


traits <- traits %>%
  select(name, abb, lifeform, growthform, n, f, typ, neo)

sites <- sites %>%
  select(
    no, plotTemp, plot, block, year, lat, lon, barrier_distance, treatment,
    shrubHeight, shrubCover, herbHeight, herbCover, soilCover,
    conf.low, conf.high,
    speciesrichness, shannon, simpson,
    therophytes, perennials, wood,
    reed_cover, sand_cover, gravel_cover
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



write_csv(species, here("data", "processed", "data_processed_species.csv"))
write_csv(sites, here("data", "processed", "data_processed_sites.csv"))
write_csv(traits, here("data", "processed", "data_processed_traits.csv"))
