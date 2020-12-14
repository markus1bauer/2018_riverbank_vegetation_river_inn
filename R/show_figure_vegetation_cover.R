# Show Figure vegetation cover ~ year:treatment ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(emmeans)
library(ggeffects)

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

(sites <- select(sites, no, plotTemp, plot, block, conf.low, conf.high, year, barrierDist, treatment, habitatType, herbCover))
sites <- subset(sites, herbCover > 0)

#### Chosen model ###
m4 <- lmer((herbCover) ~ treatment * year +
             (1|block), sites, REML = F)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### interaction: year:treatment ###
pdata <- ggemmeans(m4, terms = c("treatment", "year"), type = "fe")
pdata <- rename(pdata, herbCover = predicted, treatment = x, year = group)
meandata <- filter(pdata, year == "Control")
pd <- position_dodge(.6)
ggplot(pdata, aes(year, herbCover, shape = year, ymin = conf.low, ymax = conf.high))+
  facet_grid(~treatment) +
  geom_quasirandom(data = sites, aes(year, herbCover), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = herbCover), meandata, 
             color = "grey70", size = .25) +
  geom_hline(aes(yintercept = conf.low), meandata, 
             color = "grey70", linetype = "dashed", size = .25) +
  geom_hline(aes(yintercept = conf.high), meandata, 
             color = "grey70", linetype = "dashed", size = .25) +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  #annotate("text", label = "n.s.", x = 3.2, y = 0.0028) +
  scale_y_continuous(limits = c(0,115), breaks = seq(-100,300,20)) +
  scale_shape_manual(values = c(1,16,16)) +
  labs(x = "", y = expression(paste("Vegetation cover [%]")), shape = "") +
  guides(shape = F) +
  themeMB()
ggsave("figure_vegCover_(800dpi_12x8.5cm).tiff",
       dpi = 800, width = 12, height = 8.5, units = "cm") 
#, path = "Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/ouputs/figures")
