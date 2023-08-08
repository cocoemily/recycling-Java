#Cortex Ratios
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(rcompanion)
library(fitdistrplus)
library(pscl)

theme_set(theme_bw())

#### CORTEX RATION CIs ####
cr = read_csv("~/eclipse-workspace/recycling-Java/results/cortex-ratio-CI.csv")

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
mu.labs = c("mu = 1", "mu = 2", "mu = 3")
names(mu.labs) = c("1", "2", "3")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)

p1 = ggplot(cr %>% filter(overlap == 1)) +
  geom_hline(aes(yintercept = 1), color = "grey80", ) +
  geom_boxplot(aes(x = mu, y = mean, group = as.factor(mu), color = as.factor(mu))) +
  facet_grid(num_agents ~ flake_preference  + size_preference + strict_selection, 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs, 
               num_agents = occup.labs
             ), scales = "free") + 
  labs(y = "cortex ratio") +
  scale_color_colorblind(labels = mu.labs) +
  theme(legend.position = "bottom", legend.title = element_blank())
plot(p1)

ggsave(filename = "../figures/supplementary-figures/average-cortex-ratios.tiff", p1, 
       dpi = 300, width = 10, height = 6)


p2 = ggplot(cr %>% filter(flake_preference == T)) +
  geom_hline(aes(yintercept = 1), color = "grey80", ) +
  geom_boxplot(aes(x = size_preference, y = end_mean, group = size_preference)) +
  facet_grid(strict_selection ~ blank_prob, labeller = label_both, scales = "free") + 
  labs(y = "Cortex Ratio")
plot(p2)

