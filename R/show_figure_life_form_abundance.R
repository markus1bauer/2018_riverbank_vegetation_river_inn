# Show Figure abundance ~ lifeform:year:treatment ####



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

(sites <- select(sites, no, plotTemp, conf.low, conf.high, plot, block, year, barrierDist, treatment, Therophytes, Perennials, Wood))
sites <- gather(sites, "lifeform", "no", c("Therophytes","Perennials","Wood"))
sites$lifeform <- factor(sites$lifeform, levels = c("Therophytes","Perennials","Wood"))

#### Chosen model ###
m2 <- glmer((no) ~ treatment * year * lifeform +
              (1|block), sites, family = poisson)



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
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### interaction: lifeform:year:treatment ###
pd <- position_dodge(.6)
ggplot(sites, aes(year, no, fill = treatment, ymin = conf.low, ymax = conf.high))+
  geom_boxplot(position = pd, width = 0.5, color = "black") +
  #geom_quasirandom(data = sites, aes(year, no), 
  #                 color = "black", dodge.width = .6, size = 0.4) +
  facet_grid(~lifeform) +
  #annotate("text", label = "n.s.", x = 3.2, y = 0.0028) +
  scale_y_continuous(limits = c(0,17), breaks = seq(-100,300,5)) +
  scale_fill_grey(start = 0.3, end = 0.9) +
  labs(x = "", y = expression(paste("Abundance [#]")), fill = "") +
  themeMB()
ggsave("figure_lifeformNo_(800dpi_16x8.5cm).tiff",
       dpi = 800, width = 16, height = 8.5, units = "cm") 
#, path = "Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/ouput/figures")
