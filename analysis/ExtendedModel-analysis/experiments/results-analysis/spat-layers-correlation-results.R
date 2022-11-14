library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(VGAM)

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")

mid.cor.ri = layer.cor %>% filter(time == "mid") %>% 
  gather(key = "comparison", value = "correlation", starts_with("ri"))

end.cor.ri = layer.cor %>% filter(time == "end") %>% 
  gather(key = "comparison", value = "correlation", starts_with("ri"))

ggplot(mid.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)

ggplot(end.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)


mid.cor.ri.exp1 = layer.cor %>% filter(time == "mid") %>% 
  filter(exp == 1) %>%
  gather(key = "comparison", value = "correlation", starts_with("ri"))

ggplot(mid.cor.ri.exp1) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)

##TODO need to create a script that looks at relationships between different correlations within each square for each model run
