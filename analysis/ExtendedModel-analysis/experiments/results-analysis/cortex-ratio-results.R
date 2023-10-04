#Cortex Ratios
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(rcompanion)
library(fitdistrplus)
library(pscl)
library(ggpmisc)

#source("ExtendedModel-analysis/experiments/model-analysis/mu-visualization.R")

theme_set(theme_bw())

#### CORTEX RATIO CIs ####
cr = read_csv("~/eclipse-workspace/recycling-Java/results/cortex-ratio-CI.csv")

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
mu.labs = c("\u00b5 = 1", "\u00b5 = 2", "\u00b5 = 3")
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
  labs(y = "cortex ratio", x = "\u00b5") +
  scale_color_colorblind(labels = mu.labs) +
  theme(legend.position = "bottom", legend.title = element_blank())
plot(p1)

p1.mu = ggarrange(p1, mu_horz, ncol = 1, heights = c(1, 0.4))

ggsave(filename = "../figures/supplementary-figures/average-cortex-ratios.tiff", 
       p1, 
       dpi = 300, width = 10, height = 6)


p2 = ggplot(cr %>% filter(flake_preference == T)) +
  geom_hline(aes(yintercept = 1), color = "grey80", ) +
  geom_boxplot(aes(x = mu, y = mean, group = as.factor(mu), color = as.factor(mu))) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = label_both, scales = "free") + 
  labs(y = "Cortex Ratio") +
  scale_color_colorblind(labels = mu.labs) +
  theme(legend.position = "bottom", legend.title = element_blank())
plot(p2)


####CR and RI####
ri.ci = read_csv("~/eclipse-workspace/recycling-Java/results/recycling-intensity-CI.csv")
parameters = colnames(ri.ci[,c(2:14)])

joined = cr %>% left_join(ri.ci, by = c("run", parameters), suffix = c(".cr", ".ri"))


ri.cr.plot = ggplot(joined %>% filter(overlap == 1) %>% filter(flake_preference == T), 
                    aes(x = mean.cr, y = mean.ri, color = as.factor(mu), group = as.factor(mu))) +
  geom_point(size = 0.1, alpha = 0.1) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), vstep = 0.075, size = 3) +
  facet_grid(num_agents ~ size_preference + strict_selection, 
             labeller = labeller(
               num_agents = occup.labs, 
               strict_selection = strict.labs, 
               size_preference = size.labs, 
               flake_preference = flake.labs
             )) +
  scale_color_colorblind(labels = mu.labs) +
  labs(y = "recycling incidence", x = "cortex ratio") +
  theme(legend.position = "bottom", legend.title = element_blank())
plot(ri.cr.plot)

ggsave(filename = "../figures/ri-cr_plot.tiff",
       ri.cr.plot,
       dpi = 300, width = 10, height = 5.5)


ggplot(joined %>% filter(overlap == 1) %>% filter(flake_preference == T), 
       aes(x = mean.cr, y = mean.ri, color = as.factor(mu), group = as.factor(mu))) +
  geom_point(size = 0.1, alpha = 0.1) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), vstep = 0.075, size = 3) +
  facet_grid(scavenge_prob ~ blank_prob, 
             labeller = label_both) +
  scale_color_colorblind(labels = mu.labs) +
  labs(y = "recycling incidence", x = "cortex ratio") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(joined %>% filter(overlap == 1), 
       aes(x = mean.cr, y = mean.ri, color = interaction(flake_preference, size_preference), group = interaction(flake_preference, size_preference))) +
  geom_point(size = 0.1, alpha = 0.1) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), vstep = 0.075, size = 3) +
  facet_grid(scavenge_prob ~ blank_prob, 
             labeller = label_both) +
  scale_color_colorblind(labels = mu.labs) +
  labs(y = "recycling incidence", x = "cortex ratio") +
  theme(legend.position = "bottom", legend.title = element_blank())


####CR and assemblage density####
count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")
parameters = colnames(count.data[,c(10:22)])

assemb.count = count.data %>%
  group_by_at(c(parameters, "run")) %>%
  reframe(count = sum(total_count),
          ri = sum(count_recycled)/sum(total_count), 
          log.count = log(count))

cr.ad = cr %>% full_join(assemb.count, by = c(parameters, "run")) %>%
  mutate(log.count = log(count))

crad.plot = ggplot(cr.ad %>% filter(overlap == 1) %>% filter(flake_preference == T)
                   , aes(x = log.count, y = mean)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), color = "grey40", size = 2.5, label.y = "bottom") +
  facet_grid(num_agents ~ mu, 
             labeller = labeller(
               num_agents = occup.labs,
               mu = mu.labs
             )) +
  labs(x = "log(artifact count)", y = "cortex ratio")
plot(crad.plot)

crad.plot.mu = ggarrange(crad.plot, mu_horz, ncol = 1, heights = c(1, 0.4))
plot(crad.plot.mu)

ggsave(filename = "../figures/supplementary-figures/cr_assemblage-density.tiff", 
       crad.plot, 
       dpi = 300, width = 10, height = 6)

summary(lm(ri ~ log.count + mean, data = cr.ad))
summary(lm(mean ~ log.count + ri, data = cr.ad))
