# Model for target vegetation cover ####



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

(sites <- select(sites, aimCover, plotTemp, plot, block, year, barrierDist, treatment, gravelCover, sandCover, reedCover))
sites <- gather(sites, "aimType", "aimCover", c("gravelCover","sandCover","reedCover"))
sites$aimType <- factor(sites$aimType, levels = c("gravelCover","sandCover","reedCover"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(aimCover ~ treatment, sites)
plot(aimCover ~ aimType, sites)
plot(aimCover ~ barrierDist, sites)
plot(aimCover ~ year, sites)
plot(aimCover ~ block, sites)
#2way (aimType:year):
ggplot(sites, aes(aimType, aimCover, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
#2way (aimType:treatment):
ggplot(sites, aes(aimType, aimCover, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
#3way (aimType:treatment:year):
ggplot(sites, aes(year, aimCover, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T) + facet_wrap(~aimType)
#interactions with block:
ggplot(sites, aes(block, aimCover, color = aimType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
ggplot(sites, aes(block, aimCover, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
ggplot(sites, aes(block, aimCover, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((sites$aimCover), groups = factor(sites$treatment), main = "Cleveland dotplot")
dotchart((sites$aimCover), groups = factor(sites$year), main = "Cleveland dotplot")
dotchart((sites$aimCover), groups = factor(sites$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(sites$aimCover);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$aimCover))
par(mfrow = c(2,2));
plot(table((sites$aimCover)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(aimCover)) + geom_density()
ggplot(sites, aes(log(aimCover))) + geom_density()


## 2 Model building ################################################################################

# Model for paper ####
sitesK16 <- sites[!(sites$year == "2014"),]
modelK16 <- lmer(sqrt(aimCover+175) ~ aimType * year +
                   (1|block), sitesK16, REML = F)
car::Anova(modelK16)

sites1416 <- sites[!(sites$year == "Control"),]
model1416_1 <- lmer(log2(aimCover+2) ~ aimType * year +
                    (1|block), sites1416, REML = F)
car::Anova(model1416_1)
model1416_2 <- lmer(log2(aimCover+2) ~ aimType * treatment +
                    (1|block), sites1416, REML = F)
car::Anova(model1416_2)
