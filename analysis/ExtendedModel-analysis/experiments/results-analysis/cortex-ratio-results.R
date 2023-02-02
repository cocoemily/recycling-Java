#Cortex Ratios
library(tidyverse)

theme_set(theme_bw())

#### CORTEX RATION CIs ####
cr = read_csv("~/eclipse-workspace/recycling-Java/results/cortex-ratio-CI-results.csv")
# 
# ggplot(cr) +
#   geom_freqpoly(aes(x = end_left), color = "blue") +
#   geom_vline(aes(xintercept = median(cr$end_left)), color = "blue") +
#   geom_freqpoly(aes(x = end_mean), color = "black") +
#   geom_vline(aes(xintercept = median(cr$end_mean)), color = "black") +
#   geom_freqpoly(aes(x = end_right), color = "red") +
#   geom_vline(aes(xintercept = median(cr$end_right)), color = "red") +
#   facet_wrap(~mu)

ggplot(cr) +
  geom_boxplot(aes(x = mu, y = end_mean, group = mu)) +
  facet_grid(flake_preference  + size_preference ~ strict_selection, labeller = label_both, scales = "free")
  

#### GRID-BASED CORTEX RATIOS ####
cr.grid = read_csv("~/eclipse-workspace/recycling-Java/results/all-gridded-CR.csv") 


