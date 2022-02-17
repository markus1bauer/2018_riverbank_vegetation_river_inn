# Riverbank vegetation at River Inn
# Model for lifeform abundance ####
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
      plotTemp = col_factor(),
      plot = col_factor(),
      block = col_factor(),
      year = col_factor(levels = c("Control", "2014", "2016")),
      treatment = col_factor(levels = c(
        "Gravel supply",
        "Sand supply",
        "Embankment removal"
      ))
    )
) %>%
  select(
    no, plotTemp, plot, block, year, barrier_distance, treatment,
    therophytes, perennials, wood
  ) %>%
  pivot_longer(cols = c("therophytes", "perennials", "wood"),
               names_to = "lifeform", values_to = "n") %>%
  mutate(lifeform = factor(lifeform, levels = c("therophytes", "perennials", "wood")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
# simple effects:
plot(n ~ treatment, sites)
plot(n ~ lifeform, sites)
plot(n ~ barrier_distance, sites)
plot(n ~ year, sites)
plot(n ~ block, sites)
# 2way (lifeform:year):
ggplot(sites, aes(lifeform, n, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 2way (lifeform:treatment):
ggplot(sites, aes(lifeform, n, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 3way (lifeform:treatment:year):
ggplot(sites, aes(year, n, color = lifeform)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE) +
  facet_wrap(~treatment)
# interactions with block:
ggplot(sites, aes(block, n, color = lifeform)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
ggplot(sites, aes(block, n, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
ggplot(sites, aes(block, n, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)

##### b Outliers, zero-inflation, transformations? --------------------------
dotchart((sites$n), groups = factor(sites$treatment), main = "Cleveland dotplot")
dotchart((sites$n), groups = factor(sites$year), main = "Cleveland dotplot")
dotchart((sites$n), groups = factor(sites$block), main = "Cleveland dotplot")
boxplot(sites$n)
plot(table((sites$n)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(n)) +
  geom_density()
ggplot(sites, aes(log(n))) +
  geom_density()


## 2 Model building #########################################################

#### a models ---------------------------------------------------------------
# random structure
m1 <- glmer(n ~ treatment * year * lifeform +
  (1 | block / plotTemp), sites, family = poisson)
VarCorr(m1)
# 3w-model
m2 <- glmer((n) ~ treatment * year * lifeform +
  (1 | block), sites, family = poisson)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
# 2w-model
m3 <- glmer((n) ~ treatment + year + lifeform +
  lifeform:year + lifeform:treatment +
  (1 | block), sites, family = poisson)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
# 2w-model
m4 <- glmer((n) ~ treatment + year + lifeform +
  lifeform:treatment +
  (1 | block), sites, family = poisson)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)
# 1w-model
m5 <- glmer((n) ~ treatment + year + lifeform +
  (1 | block), sites, family = poisson)
isSingular(m5)
simulateResiduals(m5, plot = TRUE)


#### b comparison -----------------------------------------------------------
anova(m2, m3, m4, m5) # --> m2
rm(m1, m3, m4, m5)

#### c model check ----------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = TRUE)
plotResiduals(main = "treatment", simulationOutput$scaledResiduals, sites$treatment)
plotResiduals(main = "year", simulationOutput$scaledResiduals, sites$year)
plotResiduals(main = "lifeform", simulationOutput$scaledResiduals, sites$lifeform)
plotResiduals(main = "block", simulationOutput$scaledResiduals, sites$block)


## 3 Chosen model output ####################################################

### Model output ------------------------------------------------------------
m2 <- glmer((no) ~ treatment * year * lifeform +
  (1 | block), sites, family = poisson)
MuMIn::r.squaredGLMM(m2) # R2m = 0.966, R2c = 0.972
VarCorr(m2)
sjPlot::plot_model(m2, type = "re", show.values = TRUE)
car::Anova(m2, type = 3)

### Effect sizes ------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ year | treatment | lifeform, type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m2, ~ year | treatment | lifeform, type = "response"), "trt.vs.ctrl", ref = 3)


## 4 Robust model ##########################################################

### Kontrolle vs. 2016 -----------------------------------------------------
data <- sites[!(sites$year == "2014"), ]
m1 <- lmer(log2(n + 0.35) ~ treatment * lifeform +
  (1 | block), data, REML = FALSE)
car::Anova(m1)

### 2014 vs. 2016 ----------------------------------------------------------
data <- sites[!(sites$year == "Control"), ]
m2 <- lmer(log2(n + 0.3) ~ treatment * lifeform * year +
  (1 | block), data, REML = FALSE)
car::Anova(m2)
