# Show Figure abundance ~ lifeform:year:treatment ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(vegan)

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

(sites <- select(sites, no, plotTemp, plot, block, year, barrierDist, treatment, habitatType, herbHeight, herbCover, soilCover, speciesRich, shannon))
sites <- filter(sites, treatment != "Embankment removal" & no != "83")

species <- read_csv("data_processed_species.csv", col_names = T, na = "na", col_types = 
                      cols(
                        .default = col_double(),
                        name = col_factor(),
                        abb = col_factor()
                      )        
)
species <- select(species, !(name))
species <- column_to_rownames(species, "abb")
species <- species[,c(16:45,61:90,106:135)]
species <- species[rowSums(species) > 0, colSums(species) > 0]
species <- as.tibble(t(species))

#### Chosen model ###
dist <- wisconsin(sqrt(species))
dist <- vegdist(species, method = "bray", binary = F, na.rm = T)
(ordi <- metaMDS(dist, try = 99, previous.best = T, na.rm = T))

#### b environmental factors ----------------------------------------------------------------------------------------
ef <- envfit(ordi ~ herbHeight + soilCover + barrierDist, 
              data = sites, permu = 999, na.rm = T)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

#### a Preparation ----------------------------------------------------------------------------------------
data.scores <- as.data.frame(scores(ordi)) #input of model
data.scores$site <- rownames(data.scores)
data.scores$year <- sites$year #Write data and 1. variable
data.scores$treatment <- sites$treatment #Write data and 2. variable
### Create Ellipses ###
data.scores.mean = aggregate(data.scores[1:2], list(group = data.scores$year), mean)
veganCovEllipse <- function(cov, center = c(0,0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi / npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame() #Write 1. variable 3 times
for(g in levels(data.scores$year)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores[data.scores$year == g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2), wt = rep(1 / length(NMDS1), length(NMDS1)))$cov, center = c(mean(NMDS1), mean(NMDS2)))))
                                ,year = g))
}
### Environmental factors ###
data.ef <- as.data.frame(ef$vectors$arrows * ((sqrt(ef$vectors$r)) / 0.9))
data.ef$variables <- rownames(data.ef)
### Plot ###
ggplot() +
  geom_point(aes(x = NMDS1, y = NMDS2, shape = treatment, colour = year),#Write 1. variable
             data = data.scores, size = 4)+
  geom_path(aes(x = NMDS1, y = NMDS2, colour = year),#Write 1. variable
            data = df_ell, size = 1, linetype = 1)+
  geom_segment(aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               data = data.ef, 
               arrow = arrow(length = unit(0.2, "cm")), color = "black")+
  annotate("text", x = data.ef$NMDS1, y = data.ef$NMDS2, label = data.ef$variables)+
  annotate("text", x = -.4, y = .4, label = "2D stress = 0.19") +
  scale_colour_grey()+
  coord_equal()+
  guides(shape = guide_legend(title = "Measures"), 
         colour = guide_legend(reverse = T, title = "Year")) +
  themeMB()
ggsave("figure_ordination_(800dpi_16x10cm).tiff",
       dpi = 800, width = 16, height = 10, units = "cm")
#, path = "Z:/Documents/0_Uni/Projekt_7_Inn_Bachelorarbeit/3_Aufnahmen_und_Ergebnisse/2018_River_Res_Appl/ouput/figures")
