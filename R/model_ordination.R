# Riverbank vegetation at River Inn
# Model for ordination ####
# Markus Bauer
# 2022-02-14



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
sites <- read_csv("data_processed_sites.csv",
  col_names = TRUE, na = "na",
  col_types =
    cols(
      .default = "?",
      year = col_factor(levels = c("Control", "2014", "2016")),
      treatment = col_factor(levels = c(
        "Gravel supply",
        "Sand supply",
        "Embankment removal"
      )),
      barrier_distance = "d"
    )
) %>%
  filter(treatment != "Embankment removal" & no != "83")

traits <- read_csv("data_processed_traits.csv",
                   col_names = TRUE, na = "na",
                   col_types =
                     cols(
                       .default = "?"
                     )
) %>%
  select(name, abb)

species <- read_csv("data_processed_species.csv",
  col_names = TRUE, na = "na",
  col_types =
    cols(
      .default = col_double(),
      name = col_factor()
    )
) %>%
  left_join(traits, by = "name") %>%
  column_to_rownames("abb") %>%
  select(-name)
species <- species[, c(16:45, 61:90, 106:135)]
species <- species[rowSums(species) > 0, colSums(species) > 0]
species <- species %>%
  t() %>%
  as_tibble()



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 NMDS ##################################################################

#### a ordination -----------------------------------------------------------
(ordi <- metaMDS(species, try = 99, previous.best = TRUE, na.rm = TRUE))
stressplot(ordi) # stress: 0.12

#### b environmental factors ------------------------------------------------
(ef <- envfit(ordi ~ herbHeight + herbCover + soilCover + barrier_distance,
  data = sites, permu = 999, na.rm = TRUE
))
plot(ordi, type = "n")
plot(ef, add = TRUE, p. = .05)
text(ordi, dis = "sites", cex = .7)
ordiellipse(ordi, sites$year, kind = "sd", draw = "lines", label = TRUE)
plot(ordi, type = "n")
plot(ef, add = TRUE, p. = .05)
text(ordi, dis = "sites", cex = .7)
ordiellipse(ordi, sites$treatment, kind = "sd", draw = "lines", label = TRUE)


### 2 PERMANOVA #############################################################

 dist <- vegdist(species, method = "bray", binary = FALSE, na.rm = TRUE)

#### a year -----------------------------------------------------------------
(disp.year <- betadisper(dist, sites$year))
permutest(disp.year) # similar dispersion -> PERMANOVA possible
(permanova.year <- adonis(species ~ year,
  data = sites,
  strata = sites$block, permutations = 999, method = "bray"
))
densityplot(permustats(permanova.year)) # significant

#### b treatment ------------------------------------------------------------
(disp.treat <- betadisper(dist, sites$treatment))
permutest(disp.treat) # different dispersion -> PERMANOVA not possible
(permanova.treat <- adonis(species ~ treatment,
  data = sites,
  strata = sites$block, permutations = 999, method = "bray"
))
densityplot(permustats(permanova.treat)) # n.s.
