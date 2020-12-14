# Model for vegetation height ####



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

(sites <- select(sites, no, plotTemp, plot, block, year, barrierDist, treatment, habitatType, herbHeight))
sites <- subset(sites, herbHeight > 0)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(herbHeight ~ treatment, sites)
plot(herbHeight ~ habitatType, sites)
plot(herbHeight ~ barrierDist, sites)
plot(herbHeight ~ year, sites)
plot(herbHeight ~ block, sites)
#2way (treatment:year):
ggplot(sites, aes(treatment, herbHeight, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
#2way (treatment:barrierDist):
ggplot(sites, aes(barrierDist, herbHeight, color = treatment)) + geom_smooth() + geom_quasirandom(dodge.width = .7, groupOnX = T)
#interactions with block:
ggplot(sites, aes(block, herbHeight, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
ggplot(sites, aes(block, herbHeight, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((sites$herbHeight), groups = factor(sites$treatment), main = "Cleveland dotplot")
dotchart((sites$herbHeight), groups = factor(sites$year), main = "Cleveland dotplot")
dotchart((sites$herbHeight), groups = factor(sites$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(sites$herbHeight);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$no))
par(mfrow = c(2,2));
plot(table((sites$herbHeight)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(herbHeight)) + geom_density()
ggplot(sites, aes(sqrt(herbHeight))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(herbHeight ~ treatment * year + (1|block/plotTemp), sites, REML = F)
VarCorr(m1)
#3w-model
m2 <- lmer(log(herbHeight) ~ treatment * year * barrierDist +
             (1|block/plotTemp), sites, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)
#2w-model
m3 <- lmer(log(herbHeight) ~ treatment * year +
             (1|block/plotTemp), sites, REML = F)
isSingular(m3)
simulateResiduals(m3, plot = T)
#2w-model without plotTemp
m4 <- lmer(log(herbHeight) ~ treatment * year +
             (1|block), sites, REML = F)
isSingular(m4)
simulateResiduals(m4, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4) # --> m4
rm(m1,m2,m3)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "treatment", simulationOutput$scaledResiduals, sites$treatment)
plotResiduals(main = "year", simulationOutput$scaledResiduals,sites$year)
plotResiduals(main = "barrierDist", simulationOutput$scaledResiduals, sites$barrierDist)
plotResiduals(main = "block", simulationOutput$scaledResiduals, sites$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m4 <- lmer(log(herbHeight) ~ treatment * year +
             (1|block), sites, REML = F)
MuMIn::r.squaredGLMM(m4) #R2m = 0.659, R2c = 0.834
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = T)
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ year | treatment, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ year | treatment, type = "response"), "trt.vs.ctrl", ref = 3)

# Model for paper ####
sitesK16 <- sites[!(sites$year == "2014"),]
kruskal.test(sitesK16$herbHeight, sitesK16$treatment)
pgirmess::kruskalmc(sitesK16$herbHeight ~ sitesK16$treatment)

sites1416 <- sites[!(sites$year == "Control"),]
model1416 <- lmer(sqrt(herbHeight + 60) ~ treatment * year +
             (1|block), sites1416, REML = F)
car::Anova(model1416)