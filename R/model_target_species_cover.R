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
      .default = col_double(),
      plotTemp = col_factor(),
      plot = col_factor(),
      block = col_factor(),
      year = col_factor(levels = c("Control", "2014", "2016")),
      treatment = col_factor(levels = c(
        "Gravel supply",
        "Sand supply",
        "Embankment removal"
      )),
      habitatType = col_factor(),
      substrate = col_factor()
    )
) %>%
  select(
    aimCover, plotTemp, plot, block, year, barrier_distance, treatment,
    gravelCover, sandCover, reedCover
  ) %>%
  gather("aimType", "aimCover", c("gravelCover", "sandCover", "reedCover")) %>%
  mutate(aimType = factor(aimType, levels = c("gravelCover", "sandCover", "reedCover")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
# simple effects:
par(mfrow = c(2, 2))
plot(aimCover ~ treatment, sites)
plot(aimCover ~ aimType, sites)
plot(aimCover ~ barrier_distance, sites)
plot(aimCover ~ year, sites)
plot(aimCover ~ block, sites)
# 2way (aimType:year):
ggplot(sites, aes(aimType, aimCover, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 2way (aimType:treatment):
ggplot(sites, aes(aimType, aimCover, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 3way (aimType:treatment:year):
ggplot(sites, aes(year, aimCover, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE) +
  facet_wrap(~aimType)
# interactions with block:
ggplot(sites, aes(block, aimCover, color = aimType)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
ggplot(sites, aes(block, aimCover, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
ggplot(sites, aes(block, aimCover, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)

##### b Outliers, zero-inflation, transformations? --------------------------
par(mfrow = c(2, 2))
dotchart((sites$aimCover),
  groups = factor(sites$treatment),
  main = "Cleveland dotplot"
)
dotchart((sites$aimCover),
  groups = factor(sites$year),
  main = "Cleveland dotplot"
)
dotchart((sites$aimCover),
  groups = factor(sites$block),
  main = "Cleveland dotplot"
)
par(mfrow = c(1, 1))
boxplot(sites$aimCover)
par(mfrow = c(2, 2))
plot(table((sites$aimCover)),
  type = "h",
  xlab = "Observed values", ylab = "Frequency"
)
ggplot(sites, aes(aimCover)) +
  geom_density()
ggplot(sites, aes(log(aimCover))) +
  geom_density()


## 2 Robust model ##########################################################

### Kontrolle vs. 2016 -----------------------------------------------------
data <- sites[!(sites$year == "2014"), ]
m1 <- lmer(sqrt(aimCover + 175) ~ aimType * year +
  (1 | block), data, REML = FALSE)
car::Anova(m1)

### 2014 vs. 2016 ----------------------------------------------------------
data <- sites[!(sites$year == "Control"), ]
m2 <- lmer(log2(aimCover + 2) ~ aimType * year +
  (1 | block), data, REML = FALSE)
car::Anova(m2)
m3 <- lmer(log2(aimCover + 2) ~ aimType * treatment +
  (1 | block), data, REML = FALSE)
car::Anova(m3)
