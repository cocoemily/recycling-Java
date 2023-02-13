##ANALYSIS OF VARIATION IN LAYER DATA
library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(VGAM)
library(lme4)
library(leaps)
library(vegan)
library(QuantPsyc)
library(ggthemes)

theme_set(theme_bw())

#### ANALYSIS OF VARIATION OF OUTPUT VARIABLES BETWEEN GRID SQUARES ACROSS MODEL RUNS ####
layer.var = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-variation-output.csv")
layer.var = layer.var %>% group_by(row, col) %>%
  mutate(square = cur_group_id())

two.tech.layers = layer.var %>% filter(overlap == 1)
many.tech.layers = layer.var %>% filter(overlap == 2)

summary(layer.var$nodule.count.sd)
summary(layer.var$flake.count.sd)
summary(layer.var$cortex.ratio.sd)
summary(layer.var$recycling.intensity.sd)
summary(layer.var$num.discards.sd) 
summary(layer.var$num.scavenge.sd)
summary(layer.var$num.encounters.sd)
summary(layer.var$num.occupation.sd)
summary(layer.var$num.manufacture.sd)
summary(layer.var$num.retouch.sd)


##### TWO TECHNOLOGIES regressions to find most impactable variable on variation #####
#plotNormalHistogram(layer.var$recycling.intensity.sd)
fit1 = lm(recycling.intensity.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit1)
ri = as.data.frame(t(lm.beta(fit1)))
ri$metric = "recycling intensity"

fit2 = lm(nodule.count.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit2)
lm.beta(fit2)
nc = as.data.frame(t(lm.beta(fit2)))
nc$metric = "nodule count"

fit3 = lm(flake.count.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit3)
lm.beta(fit3)
fc = as.data.frame(t(lm.beta(fit3)))
fc$metric = "flake count"


fit4 = lm(cortex.ratio.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit4)
lm.beta(fit4)
cr = as.data.frame(t(lm.beta(fit4)))
cr$metric = "cortex ratio"

fit5 = lm(num.encounters.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit5)
lm.beta(fit5)
ne = as.data.frame(t(lm.beta(fit5)))
ne$metric = "number of encounters"

fit6 = lm(num.retouch.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit6)
lm.beta(fit6)
nr = as.data.frame(t(lm.beta(fit6)))
nr$metric = "number of retouches"

fit7 = lm(num.discards.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit7)
lm.beta(fit7)
nd = as.data.frame(t(lm.beta(fit7)))
nd$metric = "discard events"

fit8 = lm(num.scavenge.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = two.tech.layers)
summary(fit8)
lm.beta(fit8)
ns = as.data.frame(t(lm.beta(fit8)))
ns$metric = "scavenging events"

allsc = rbind(ri, cr, fc, nc, ne, nr, nd, ns) %>%
  gather(key = "variable", value = "stand_coef", 1:11) %>%
  mutate(abs_sc = abs(stand_coef))

allsc$variable = factor(allsc$variable, 
                        levels= c(
                          "mu", "blank_prob", "scavenge_prob", 
                          "max_artifact_carry", "max_use_intensity", "max_flake_size",
                          "min_suitable_flake_size", 
                          "flake_preferenceTRUE", "size_preferenceTRUE", "strict_selectionTRUE", 
                          "square"
                        )) 
allsc$metric = factor(allsc$metric, 
                      levels = c(
                        "recycling intensity", 
                        "cortex ratio", 
                        "flake count", "nodule count", 
                        "discard events", "scavenging events", 
                        "number of retouches", "number of encounters"
                      ))

two.sc = ggplot(allsc) +
  geom_hline(yintercept = 0, color = I("red")) +
  geom_point(aes(x = variable, y = abs_sc, color = metric, group = metric, shape = variable))+
  coord_flip() +
  facet_wrap(~metric, ncol =1, strip.position = "right") +
  theme(strip.text = element_text(size = 7), axis.text.y = element_text(size = 6)) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(16, 0, 2, 3, 4, 24, 25, 7, 9, 10, 8), 
                     labels = rev(c("square", "strict selection: TRUE", "size preference: TRUE",
                                    "flake preference: TRUE", "min. selectable flake size", "max. flake size",
                                    "max. use intensity", "max. artifact carry", "scavenging probability",
                                    "blank probability", "mu"))) +
  guides(color = "none") +
  scale_x_discrete(labels =
                     rev(c("square", "strict selection: TRUE", "size preference: TRUE",
                       "flake preference: TRUE", "min. selectable flake size", "max. flake size",
                       "max. use intensity", "max. artifact carry", "scavenging probability",
                       "blank probability", "mu"))) +
  labs(y = "absolute value of standardized coefficient")

ggsave(filename = "../figures/TWO-TECH_stand-coeffs-layer-variation.tiff", 
       two.sc, 
       dpi = 300, height = 9)

##### MANY TECHNOLOGIES regressions to find most impactable variable on variation #####
#plotNormalHistogram(layer.var$recycling.intensity.sd)
fit1 = lm(recycling.intensity.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit1)
ri = as.data.frame(t(lm.beta(fit1)))
ri$metric = "recycling intensity"

fit2 = lm(nodule.count.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit2)
lm.beta(fit2)
nc = as.data.frame(t(lm.beta(fit2)))
nc$metric = "nodule count"

fit3 = lm(flake.count.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit3)
lm.beta(fit3)
fc = as.data.frame(t(lm.beta(fit3)))
fc$metric = "flake count"


fit4 = lm(cortex.ratio.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit4)
lm.beta(fit4)
cr = as.data.frame(t(lm.beta(fit4)))
cr$metric = "cortex ratio"

fit5 = lm(num.encounters.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit5)
lm.beta(fit5)
ne = as.data.frame(t(lm.beta(fit5)))
ne$metric = "number of encounters"

fit6 = lm(num.retouch.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit6)
lm.beta(fit6)
nr = as.data.frame(t(lm.beta(fit6)))
nr$metric = "number of retouches"

fit7 = lm(num.discards.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit7)
lm.beta(fit7)
nd = as.data.frame(t(lm.beta(fit7)))
nd$metric = "discard events"

fit8 = lm(num.scavenge.sd ~ max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size + square, 
          data = many.tech.layers)
summary(fit8)
lm.beta(fit8)
ns = as.data.frame(t(lm.beta(fit8)))
ns$metric = "scavenging events"

allsc = rbind(ri, cr, fc, nc, ne, nr, nd, ns) %>%
  gather(key = "variable", value = "stand_coef", 1:11) %>%
  mutate(abs_sc = abs(stand_coef))

allsc$variable = factor(allsc$variable, 
                        levels= c(
                          "mu", "blank_prob", "scavenge_prob", 
                          "max_artifact_carry", "max_use_intensity", "max_flake_size",
                          "min_suitable_flake_size", 
                          "flake_preferenceTRUE", "size_preferenceTRUE", "strict_selectionTRUE", 
                          "square"
                        )) 
allsc$metric = factor(allsc$metric, 
                      levels = c(
                        "recycling intensity", 
                        "cortex ratio", 
                        "flake count", "nodule count", 
                        "discard events", "scavenging events", 
                        "number of retouches", "number of encounters"
                      ))

two.sc = ggplot(allsc) +
  geom_hline(yintercept = 0, color = I("red")) +
  geom_point(aes(x = variable, y = abs_sc, color = metric, group = metric, shape = variable))+
  coord_flip() +
  facet_wrap(~metric, ncol =1, strip.position = "right") +
  theme(strip.text = element_text(size = 7), axis.text.y = element_text(size = 6)) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(16, 0, 2, 3, 4, 24, 25, 7, 9, 10, 8), 
                     labels = rev(c("square", "strict selection: TRUE", "size preference: TRUE",
                                    "flake preference: TRUE", "min. selectable flake size", "max. flake size",
                                    "max. use intensity", "max. artifact carry", "scavenging probability",
                                    "blank probability", "mu"))) +
  guides(color = "none") +
  scale_x_discrete(labels =
                     rev(c("square", "strict selection: TRUE", "size preference: TRUE",
                           "flake preference: TRUE", "min. selectable flake size", "max. flake size",
                           "max. use intensity", "max. artifact carry", "scavenging probability",
                           "blank probability", "mu"))) +
  labs(y = "absolute value of standardized coefficient")

ggsave(filename = "../figures/MANY-TECH_stand-coeffs-layer-variation.tiff", 
       two.sc, 
       dpi = 300, height = 9)
