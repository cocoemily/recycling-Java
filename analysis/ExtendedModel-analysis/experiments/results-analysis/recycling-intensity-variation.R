library(tidyverse)
library(ggpubr)
library(ggthemes)
library(plyr)
library(fitdistrplus)
library(MASS)
library(rstatix)

theme_set(theme_bw())

size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "non-strict selection")
names(strict.labs) = c("TRUE", "FALSE")
tech.labs = c("two technology types", "many technology types")
names(tech.labs) = c("1", "2")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)
blank.labs = c("blank probability: 0.25", "blank probability: 0.5", "blank probability: 0.75")
names(blank.labs) = c(0.25, 0.5, 0.75)
mu.labs = c("mu = 1", "mu = 2", "mu = 3")
names(mu.labs) = c(1, 2, 3)

ri.ci = read_csv("~/eclipse-workspace/recycling-Java/results/recycling-intensity-CI.csv")
layer.var = read_csv("~/eclipse-workspace/recycling-Java/results/layer-coefficients-of-variation.csv")

ri.var = layer.var[,c(1:13, 17, 23)]
rm(list = c("layer.var"))

ggplot(ri.var) +
  geom_boxplot(aes(x = as.factor(scavenge_prob), y = recycling.intensity.cv, 
                   fill = as.factor(scavenge_prob), group = as.factor(scavenge_prob))) +
  facet_grid(overlap~blank_prob, labeller = labeller(
    overlap = tech.labs, 
    blank_prob = blank.labs
  )) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")



