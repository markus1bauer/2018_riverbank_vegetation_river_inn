# Riverbank vegetation at River Inn
# Mode for target species cover ####
# Markus Bauer
# 2022-02-14



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(lmerTest)
library(DHARMa)
library(emmeans)

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
  gather("aimType", "aim_cover",
         c("gravel_cover", "sand_cover", "reed_cover")) %>%
  mutate(aimType = factor(aimType,
                          levels = c("gravel_cover",
                                     "sand_cover",
                                     "reed_cover")),
         y = aim_cover)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
# simple effects:
plot(y ~ treatment, sites)
plot(y ~ aimType, sites)
plot(y ~ barrier_distance, sites)
plot(y ~ year, sites)
plot(y ~ block, sites)
# 2way (aimType:year):
ggplot(sites, aes(aimType, y, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 2way (aimType:treatment):
ggplot(sites, aes(aimType, y, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 3way (aimType:treatment:year):
ggplot(sites, aes(year, y, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE) +
  facet_wrap(~aimType)
# interactions with block:
ggplot(sites, aes(block, y, color = aimType)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
ggplot(sites, aes(block, y, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
ggplot(sites, aes(block, y, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)

##### b Outliers, zero-inflation, transformations? --------------------------
dotchart((sites$y),
  groups = factor(sites$treatment),
  main = "Cleveland dotplot"
)
dotchart((sites$y),
  groups = factor(sites$year),
  main = "Cleveland dotplot"
)
dotchart((sites$y),
  groups = factor(sites$block),
  main = "Cleveland dotplot"
)
par(mfrow = c(1, 1))
boxplot(sites$y)
par(mfrow = c(2, 2))
plot(table((sites$y)),
  type = "h",
  xlab = "Observed values", ylab = "Frequency"
)
ggplot(sites, aes(y)) +
  geom_density()
ggplot(sites, aes(log(y))) +
  geom_density()


## 2 Robust model ##########################################################

### Kontrolle vs. 2016 -----------------------------------------------------
data <- sites[!(sites$year == "2014"), ]
m1 <- lmer(sqrt(y + 175) ~ aimType * year +
  (1 | block), data, REML = FALSE)
car::Anova(m1)

### 2014 vs. 2016 ----------------------------------------------------------
data <- sites[!(sites$year == "Control"), ]
m2 <- lmer(log2(y + 2) ~ aimType * year +
  (1 | block), data, REML = FALSE)
car::Anova(m2)
m3 <- lmer(log2(y + 2) ~ aimType * treatment +
  (1 | block), data, REML = FALSE)
car::Anova(m3)
