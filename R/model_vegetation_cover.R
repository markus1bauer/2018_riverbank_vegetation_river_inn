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
  subset(herbCover > 0) %>%
  rename(y = herbCover)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
# simple effects:
plot(y ~ treatment, sites)
plot(y ~ barrier_distance, sites)
plot(y ~ year, sites)
plot(y ~ block, sites)
# 2way (treatment:year):
ggplot(sites, aes(treatment, y, color = year)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# 2way (treatment:barrierDist):
ggplot(sites, aes(barrier_distance, y, color = treatment)) +
  geom_smooth(method = "loess") +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)
# interactions with block:
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
boxplot(sites$y)
plot(table((sites$y)),
  type = "h",
  xlab = "Observed values", ylab = "Frequency"
)
ggplot(sites, aes(y)) +
  geom_density()
ggplot(sites, aes(y)) +
  geom_density()


## 2 Model building #########################################################

#### a models ---------------------------------------------------------------
# random structure
m1 <- lmer(y ~ treatment * year + (1 | block / plotTemp), sites, REML = FALSE)
VarCorr(m1)
# 3w-model
m2 <- lmer((y) ~ treatment * year * barrier_distance +
  (1 | block / plotTemp), sites, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
# 2w-model
m3 <- lmer((y) ~ treatment * year +
  (1 | block / plotTemp), sites, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
# 2w-model without plotTemp
m4 <- lmer((y) ~ treatment * year +
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
m4 <- lmer(log(y) ~ treatment * year +
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
kruskal.test(data$y, data$treatment)
pgirmess::kruskalmc(data$y ~ data$treatment)

### 2014 vs. 2016 ----------------------------------------------------------
data <- sites[!(sites$year == "Control"), ]
m <- lmer(sqrt(y + 70) ~ treatment * year +
            (1 | block), data, REML = FALSE)
car::Anova(m)
