# Riverbank vegetation at River Inn
# Model for vegetation cover ####
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
    no, plotTemp, plot, block, year, barrier_distance, treatment,
    habitatType, herbCover
  ) %>%
  subset(herbCover > 0)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
# simple effects:
plot(herbCover ~ treatment, sites)
plot(herbCover ~ habitatType, sites)
plot(herbCover ~ barrier_distance, sites)
plot(herbCover ~ year, sites)
plot(herbCover ~ block, sites)
# 2way (treatment:year):
ggplot(sites, aes(treatment, herbCover, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 2way (treatment:barrierDist):
ggplot(sites, aes(barrier_distance, herbCover, color = treatment)) +
  geom_smooth() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# interactions with block:
ggplot(sites, aes(block, herbCover, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
ggplot(sites, aes(block, herbCover, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)

##### b Outliers, zero-inflation, transformations? --------------------------
dotchart((sites$herbCover),
  groups = factor(sites$treatment),
  main = "Cleveland dotplot"
)
dotchart((sites$herbCover),
  groups = factor(sites$year),
  main = "Cleveland dotplot"
)
dotchart((sites$herbCover),
  groups = factor(sites$block),
  main = "Cleveland dotplot"
)
boxplot(sites$herbCover)
plot(table((sites$herbCover)),
  type = "h",
  xlab = "Observed values", ylab = "Frequency"
)
ggplot(sites, aes(herbCover)) +
  geom_density()
ggplot(sites, aes(herbCover)) +
  geom_density()


## 2 Model building #########################################################

#### a models ---------------------------------------------------------------
# random structure
m1 <- lmer(herbCover ~ treatment * year + (1 | block / plotTemp), sites, REML = FALSE)
VarCorr(m1)
# 3w-model
m2 <- lmer((herbCover) ~ treatment * year * barrier_distance +
  (1 | block / plotTemp), sites, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
# 2w-model
m3 <- lmer((herbCover) ~ treatment * year +
  (1 | block / plotTemp), sites, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
# 2w-model without plotTemp
m4 <- lmer((herbCover) ~ treatment * year +
  (1 | block), sites, REML = FALSE)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)

#### b comparison -----------------------------------------------------------
anova(m2, m3, m4) # --> m4
rm(m1, m2, m3)

#### c model check ----------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = TRUE)
plotResiduals(main = "treatment", simulationOutput$scaledResiduals, sites$treatment)
plotResiduals(main = "year", simulationOutput$scaledResiduals, sites$year)
plotResiduals(main = "barrierDist", simulationOutput$scaledResiduals, sites$barrier_distance)
plotResiduals(main = "block", simulationOutput$scaledResiduals, sites$block)


## 3 Chosen model output ####################################################

### Model output ------------------------------------------------------------
m4 <- lmer(log(herbCover) ~ treatment * year +
  (1 | block), sites, REML = FALSE)
MuMIn::r.squaredGLMM(m4) # R2m = 0.586, R2c = 0.640
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = TRUE)
car::Anova(m4, type = 3)

### Effect sizes ------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ year | treatment, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ year | treatment, type = "response"), "trt.vs.ctrl", ref = 3)


## 4 Robust model ##########################################################

### Kontrolle vs. 2016 -----------------------------------------------------
data <- sites[!(sites$year == "2014"), ]
kruskal.test(data$herbCover, data$treatment)
pgirmess::kruskalmc(data$herbCover ~ data$treatment)

### 2014 vs. 2016 ----------------------------------------------------------
data <- sites[!(sites$year == "Control"), ]
m <- lmer(sqrt(herbCover + 70) ~ treatment * year +
            (1 | block), data, REML = FALSE)
car::Anova(m)
