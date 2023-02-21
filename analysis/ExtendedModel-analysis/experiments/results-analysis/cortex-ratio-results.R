#Cortex Ratios
library(tidyverse)
library(ggpubr)
library(rcompanion)
library(fitdistrplus)
library(pscl)

theme_set(theme_bw())

#### CORTEX RATION CIs ####
cr = read_csv("~/eclipse-workspace/recycling-Java/results/cortex-ratio-CI-results.csv")

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")

p1 = ggplot(cr) +
  geom_hline(aes(yintercept = 1), color = "grey80", ) +
  geom_boxplot(aes(x = mu, y = end_mean, group = mu)) +
  facet_grid(flake_preference  + size_preference ~ strict_selection, 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs
             ), scales = "free") + 
  labs(y = "Cortex Ratio")
plot(p1)

ggsave(filename = "../figures/average-cortex-ratios.tiff", p1, 
       dpi = 300, width = 6, height = 5)



#### GRID-BASED CORTEX RATIOS ####
cr.grid = read_csv("~/eclipse-workspace/recycling-Java/results/all-gridded-CR.csv") 

summary(cr.grid$cortex.ratio)

ggplot(cr.grid %>% filter(flake_preference == T & size_preference == T) %>% filter(cortex.ratio < 5)) +
  geom_hline(aes(yintercept = 1), color = "grey80", ) +
  geom_boxplot(aes(x = mu, y = cortex.ratio, group = mu)) +
  facet_grid(row ~ col)

