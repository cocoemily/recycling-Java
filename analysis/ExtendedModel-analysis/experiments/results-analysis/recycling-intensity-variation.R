library(tidyverse)
library(ggpubr)
library(ggthemes)
library(plyr)
library(fitdistrplus)
library(MASS)
library(rstatix)
library(rcompanion)

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
mu.labs = c("\u00b5 = 1", "\u00b5 = 2", "\u00b5 = 3")
names(mu.labs) = c(1, 2, 3)
carry.labs = c("carry capacity: 10", "carry capacity: 20")
names(carry.labs) = c(10, 20)
use.labs = c("manufacture events: 15", "manufacture events: 30") 
names(use.labs) = c(15, 30)

ri.ci = read_csv("~/eclipse-workspace/recycling-Java/results/recycling-intensity-CI.csv")
layer.var = read_csv("~/eclipse-workspace/recycling-Java/results/layer-coefficients-of-variation.csv")

ri.var = layer.var[,c(1:13, 17, 23)] %>% filter(overlap == 1)
#rm(list = c("layer.var"))

plotNormalHistogram(ri.var$recycling.intensity.cv)


stat.test <- ri.var %>%
  group_by(blank_prob, overlap) %>%
  wilcox_test(recycling.intensity.cv ~ scavenge_prob) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()


plot1 = ggplot(ri.var) +
  geom_boxplot(aes(x = as.factor(scavenge_prob), y = recycling.intensity.cv, 
                   fill = as.factor(scavenge_prob), group = as.factor(scavenge_prob))) +
  facet_grid(num_agents~blank_prob, labeller = labeller(
    num_agents = occup.labs, 
    blank_prob = blank.labs
  )) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "scavenging probability", y = "recycling incidence COV", 
       fill = "scavenging probability") +
  theme(legend.position = "bottom")
plot(plot1)

ggsave(
  filename = "../figures/recycling-intensity-variation_by-probs.tiff", 
  plot1, 
  dpi = 300, width = 8, height = 4.5
)

plot2 = ggplot(layer.var) +
  geom_boxplot(aes(x = as.factor(blank_prob), y = recycling.intensity.cv, 
                   fill = as.factor(blank_prob), group = as.factor(blank_prob))) +
  facet_grid(overlap~scavenge_prob, labeller = labeller(
    num_agents = occup.labs, 
    blank_prob = blank.labs
  )) +
  theme(legend.position = "bottom")
plot(plot2)


plot3 = ggplot(ri.var) +
  geom_boxplot(aes(x = as.factor(mu), y = recycling.intensity.cv, 
                   color = as.factor(mu), group = as.factor(mu))) +
  facet_grid(max_use_intensity~max_artifact_carry, labeller = labeller(
    max_use_intensity = use.labs, 
    max_artifact_carry = carry.labs
  )) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_colorblind(labels = mu.labs) +
  labs(x = "mu", y = "recycling incidence COV")
plot(plot3)

ggsave(
  filename = "../figures/supplementary-figures/recycling-intensity-variation_SUPP1.tiff", 
  plot3, 
  dpi = 300, width = 7, height = 4.5
)


ggplot(ri.var) +
  geom_boxplot(aes(x = as.factor(flake_preference), y = recycling.intensity.cv))

stat.test2 <- ri.var %>%
  wilcox_test(recycling.intensity.cv ~ flake_preference) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

stat.test3 = ri.var %>% filter(flake_preference == T) %>%
  group_by(strict_selection) %>%
  wilcox_test(recycling.intensity.cv ~ size_preference) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()


plot4 = ggplot(ri.var %>% filter(flake_preference == T)) +
  geom_boxplot(aes(x = as.factor(strict_selection), y = recycling.intensity.cv, 
                   color = as.factor(strict_selection), group = as.factor(strict_selection))) +
  facet_grid(~size_preference, labeller = labeller(
    size_preference = size.labs
  )) +
  theme(legend.position = "bottom") +
  scale_color_colorblind(labels = strict.labs) +
  scale_x_discrete(labels = strict.labs) +
  labs(y = "recycling incidence COV") +
  theme(axis.title.x = element_blank(), legend.title = element_blank())
plot(plot4)

ggsave(
  filename = "../figures/supplementary-figures/recycling-intensity-variation_SUPP2.tiff", 
  plot4, 
  dpi = 300, width = 7, height = 3
)


ggplot(ri.var) +
  geom_boxplot(aes(x = as.factor(mu), y = recycling.intensity.cv, 
                   fill = as.factor(mu), group = as.factor(mu))) +
  facet_grid(scavenge_prob~blank_prob, labeller = label_both) +
  scale_fill_colorblind() +
  theme(legend.position = "bottom")

ggplot(ri.var) +
  geom_boxplot(aes(x = interaction(as.factor(flake_preference), as.factor(size_preference)), y = recycling.intensity.cv, 
                   fill = interaction(as.factor(flake_preference), as.factor(size_preference)), group = interaction(as.factor(flake_preference), as.factor(size_preference)))) +
  facet_grid(scavenge_prob~blank_prob, labeller = label_both) +
  scale_fill_colorblind() +
  theme(legend.position = "bottom")



