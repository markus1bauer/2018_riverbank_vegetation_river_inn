# Riverbank vegetation at River Inn
# Show figure 6: target species coverage ####
# Markus Bauer
# 2022-02-14


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)

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
      treatment = col_factor(levels = c("Gravel supply", "Sand supply", "Embankment removal")),
      habitatType = col_factor(),
      substrate = col_factor()
    )
) %>%
  select(
    plotTemp, plot, conf.low, conf.high, block, year, barrier_distance, treatment,
    gravel_cover, sand_cover, reed_cover
  ) %>%
  pivot_longer(names_to = "aimType", values_to = "aim_cover", cols = c("gravel_cover", "sand_cover", "reed_cover")) %>%
  mutate(aimType = factor(aimType, levels = c("gravel_cover", "sand_cover", "reed_cover")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten #######################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### interaction: aimType:year:treatment ###
sites$aimType <- recode_factor(sites$aimType,
  gravelCover = "Gravel vegetation",
  sandCover = "Sand vegetation", reedCover = "Reed"
)
pd <- position_dodge(.6)
ggplot(sites, aes(year, aim_cover, fill = treatment, ymin = conf.low, ymax = conf.high)) +
  geom_boxplot(position = pd, width = 0.5, color = "black") +
  facet_grid(~aimType) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(-100, 300, 20)) +
  scale_fill_grey(start = 0.3, end = 0.9) +
  labs(x = "", y = expression(paste("Cumulated coverage [%]")), fill = "") +
  theme_mb()

### Save ###
ggsave("figure_target_cover_800dpi_16x8.5cm.tiff",
  dpi = 800, width = 16, height = 8.5, units = "cm",
  path = here("ouputs", "figures")
)
