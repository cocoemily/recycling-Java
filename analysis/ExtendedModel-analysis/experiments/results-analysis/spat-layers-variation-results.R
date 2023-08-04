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
library(Dict)

theme_set(theme_bw())

terms_labs = c(
  "overlap1TRUE" = "two technologies", 
  "overlap2TRUE" = "many technologies",
  "mu" = "mu",
  "num_agents" = "number of agents",
  "strict_selectionTRUE" = "strict selection: TRUE",
  "size_preferenceTRUE" = "size preference: TRUE",
  "flake_preferenceTRUE" = "flake preference: TRUE",
  "scavenge_prob" = "scavenging probability",
  "min_suitable_flake_size" = "min. selectable flake size",
  "max_use_intensity" = "max. use intensity",
  "max_flake_size" = "max. flake size",
  "max_artifact_carry" = "max. artifact carry",
  "blank_prob" = "blank probability",
  "(Intercept)" = "(intercept)",
  "scavenge_prob:blank_prob" = "blank probability:scavenging probability",
  "max_flake_size:min_suitable_flake_size" = "max. flake size:min. selectable flake size"
)

#### ANALYSIS OF VARIATION OF OUTPUT VARIABLES BETWEEN GRID SQUARES ACROSS MODEL RUNS ####
layer.var = read_csv("~/eclipse-workspace/recycling-Java/results/layer-coefficients-of-variation.csv")

summary(layer.var$nodule.count.cv)
summary(layer.var$flake.count.cv)
summary(layer.var$cortex.ratio.cv)
summary(layer.var$recycling.intensity.cv)
summary(layer.var$num.discards.cv) 
summary(layer.var$num.scavenge.cv)
summary(layer.var$num.encounters.cv)
summary(layer.var$num.retouch.cv)


##### regressions to find most impactable variable on variation #####
#plotNormalHistogram(layer.var$recycling.intensity.cv)
layer.var$overlap1 = ifelse(layer.var$overlap == 1, T, F)
layer.var$overlap2 = ifelse(layer.var$overlap == 2, T, F)

fit1 = lm(recycling.intensity.cv ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit1)
ri = as.data.frame(t(lm.beta(fit1)))
ri$metric = "recycling intensity"

fit2 = lm(nodule.count.cv ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit2)
lm.beta(fit2)
nc = as.data.frame(t(lm.beta(fit2)))
nc$metric = "nodule count"

fit3 = lm(flake.count.cv ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit3)
lm.beta(fit3)
fc = as.data.frame(t(lm.beta(fit3)))
fc$metric = "flake count"


fit4 = lm(cortex.ratio.cv ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit4)
lm.beta(fit4)
cr = as.data.frame(t(lm.beta(fit4)))
cr$metric = "cortex ratio"

fit5 = lm(num.encounters.cv ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit5)
lm.beta(fit5)
ne = as.data.frame(t(lm.beta(fit5)))
ne$metric = "number of encounters"

fit6 = lm(num.retouch.cv  ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit6)
lm.beta(fit6)
nr = as.data.frame(t(lm.beta(fit6)))
nr$metric = "number of retouches"

fit7 = lm(num.discards.cv ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit7)
lm.beta(fit7)
nd = as.data.frame(t(lm.beta(fit7)))
nd$metric = "discard events"

fit8 = lm(num.scavenge.cv ~ overlap2 + max_use_intensity + max_artifact_carry +
            max_flake_size + blank_prob + scavenge_prob + mu + num_agents + size_preference + 
            flake_preference + strict_selection + min_suitable_flake_size, 
          data = layer.var)
summary(fit8)
lm.beta(fit8)
ns = as.data.frame(t(lm.beta(fit8)))
ns$metric = "scavenging events"

allsc = rbind(ri, cr, fc, nc, ne, nr, nd, ns) %>%
  gather(key = "variable", value = "stand_coef", 1:11) %>%
  mutate(abs_sc = abs(stand_coef))

allsc$variable = factor(allsc$variable, 
                        levels= c("overlap2TRUE", "num_agents",
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
  scale_shape_manual(values = c(5, 11, 16, 0, 2, 3, 4, 24, 25, 7, 9, 10, 8), 
                     labels = terms_labs) +
  scale_x_discrete(labels = terms_labs) +
  guides(color = "none") +
  labs(y = "absolute value of standardized coefficient")
plot(two.sc)

ggsave(filename = "../figures/supplementary-figures/stand-coeffs-layer-variation.tiff", 
       two.sc, 
       dpi = 300, height = 9)
