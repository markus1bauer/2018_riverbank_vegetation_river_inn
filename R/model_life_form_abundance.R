# Model for lifeform abundance ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lmerTest)
library(DHARMa)
library(MuMIn)
library(car)
library(emmeans)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/data/processed")

### Load data ###
sites <- read_csv("data_processed_sites.csv", col_names = T, na = "na", col_types = 
                    cols(
                      .default = col_double(),
                      plotTemp = col_factor(),
                      plot = col_factor(),
                      block = col_factor(),
                      year = col_factor(levels = c("Control","2014","2016")),
                      treatment = col_factor(levels = c("Gravel supply","Sand supply","Embankment removal")),
                      habitatType = col_factor(),
                      substrate = col_factor()
                    )        
)

(sites <- select(sites, no, plotTemp, plot, block, year, barrierDist, treatment, therophytes, perennials, wood))
sites <- gather(sites, "lifeform", "no", c("therophytes","perennials","wood"))
sites$lifeform <- factor(sites$lifeform, levels = c("therophytes","perennials","wood"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(no ~ treatment, sites)
plot(no ~ lifeform, sites)
plot(no ~ barrierDist, sites)
plot(no ~ year, sites)
plot(no ~ block, sites)
#2way (lifeform:year):
ggplot(sites, aes(lifeform, no, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
#2way (lifeform:treatment):
ggplot(sites, aes(lifeform, no, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
#3way (lifeform:treatment:year):
ggplot(sites, aes(year, no, color = lifeform)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T) + facet_wrap(~treatment)
#interactions with block:
ggplot(sites, aes(block, no, color = lifeform)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
ggplot(sites, aes(block, no, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
ggplot(sites, aes(block, no, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((sites$no), groups = factor(sites$treatment), main = "Cleveland dotplot")
dotchart((sites$no), groups = factor(sites$year), main = "Cleveland dotplot")
dotchart((sites$no), groups = factor(sites$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(sites$no);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$no))
par(mfrow = c(2,2));
plot(table((sites$no)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(no)) + geom_density()
ggplot(sites, aes(log(no))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- glmer(no ~ treatment * year * lifeform + 
              (1|block/plotTemp), sites, family = poisson)
VarCorr(m1)
#3w-model
m2 <- glmer((no) ~ treatment * year * lifeform +
             (1|block), sites, family = poisson)
isSingular(m2)
simulateResiduals(m2, plot = T)
#2w-model
m3 <- glmer((no) ~ treatment + year + lifeform +
              lifeform:year + lifeform:treatment +
              (1|block), sites, family = poisson)
isSingular(m3)
simulateResiduals(m3, plot = T)
#2w-model
m4 <- glmer((no) ~ treatment + year + lifeform +
              lifeform:treatment +
             (1|block), sites, family = poisson)
isSingular(m4)
simulateResiduals(m4, plot = T)
#1w-model
m5 <- glmer((no) ~ treatment + year + lifeform +
              (1|block), sites, family = poisson)
isSingular(m5)
simulateResiduals(m5, plot = T)


#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4,m5) # --> m2
rm(m1,m3,m4,m5)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "treatment", simulationOutput$scaledResiduals, sites$treatment)
plotResiduals(main = "year", simulationOutput$scaledResiduals,sites$year)
plotResiduals(main = "lifeform", simulationOutput$scaledResiduals, sites$lifeform)
plotResiduals(main = "block", simulationOutput$scaledResiduals, sites$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m2 <- glmer((no) ~ treatment * year * lifeform +
              (1|block), sites, family = poisson)
MuMIn::r.squaredGLMM(m2) #R2m = 0.966, R2c = 0.972
VarCorr(m2)
sjPlot::plot_model(m2, type = "re", show.values = T)
car::Anova(m2, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ year | treatment | lifeform, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m2, ~ year | treatment | lifeform, type = "response"), "trt.vs.ctrl", ref = 3)


# Model for paper ####
sitesK16 <- sites[!(sites$year == "2014"),]
modelK16 <- lmer(log2(spec+0.35) ~ treatment * lifeform +
                    (1|block), sites1416, REML = F)
car::Anova(modelK16)

sites1416 <- sites[!(sites$year == "Control"),]
model1416 <- lmer(log2(spec+0.3) ~ treatment * lifeform * year +
                    (1|block), sites1416, REML = F)
car::Anova(model1416)