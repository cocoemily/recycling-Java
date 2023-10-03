##object counts

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(ggpmisc)
library(jtools)
library(pscl)
library(MASS)
library(moments)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")
parameters = colnames(count.data[,c(10:22)])


flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)
mu.labs = c("\u00b5 = 1", "\u00b5 = 2", "\u00b5 = 3")
names(mu.labs) = c(1,2,3)
blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")

####all artifacts ####
##### artifact count box plots ####
p1 = ggplot(count.data %>% filter(overlap == 1)) +
  geom_boxplot(aes(x = as.factor(num_agents), y = total_count, fill = obj_type)) +
  facet_grid(flake_preference + size_preference + strict_selection ~ mu , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs, 
               mu = mu.labs
             )) +
  scale_x_discrete(labels = occup.labs) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "time", y = "number of objects") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6), 
        legend.position = "bottom")
#plot(p1)


##### average artifact counts ####
test.p1 = ggplot(count.data %>% filter(overlap == 1), 
                 aes(x = as.factor(num_agents), y = total_count, fill = obj_type, group = obj_type)) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9),
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference + strict_selection ~ mu , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs, 
               mu = mu.labs
             )) +
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(labels = occup.labs) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6), 
        legend.position = "bottom") +
  labs(y = "average number of objects", x = "time")
#plot(test.p1)


ggsave(filename = "../figures/supplementary-figures/all-object-counts.tiff", 
       plot = ggarrange(p1, test.p1, 
                        common.legend = T, labels = "AUTO", 
                        ncol = 2, nrow = 1), 
       dpi = 300, width = 12, height = 10)



pp1 = ggplot(count.data %>% filter(overlap == 1)) +
  geom_boxplot(aes(x = as.factor(num_agents), y = total_count)) +
  facet_wrap(~mu, 
             labeller = label_both) +
  scale_x_discrete(labels = occup.labs) +
  #scale_fill_brewer(palette = "Paired") +
  labs(x = "time", y = "number of objects") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6), 
        legend.position = "bottom")
plot(pp1)


####recycled objects ####
my_colors <- RColorBrewer::brewer.pal(4, "Paired")[3:4]

##### artifact count box plots ####
p1 = ggplot(count.data %>% filter(overlap == 1)) +
  geom_boxplot(aes(x = as.factor(num_agents), y = count_recycled, fill = obj_type)) +
  facet_grid(flake_preference + size_preference + strict_selection ~ mu , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs, 
               mu = mu.labs
             )) +
  scale_x_discrete(labels = occup.labs) +
  scale_fill_manual(values = my_colors) +
  labs(x = "time", y = "number of recycled objects") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6), 
        legend.position = "bottom")
#plot(p1)


##### average artifact counts ####
test.p1 = ggplot(count.data %>% filter(overlap == 1), 
                 aes(x = as.factor(num_agents), y = count_recycled, fill = obj_type, group = obj_type)) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9),
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference + strict_selection ~ mu , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs, 
               mu = mu.labs
             )) +
  scale_fill_manual(values = my_colors) +
  scale_x_discrete(labels = occup.labs) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6), 
        legend.position = "bottom") +
  labs(y = "average number of recycled objects", x = "time")
#plot(test.p1)


ggsave(filename = "../figures/supplementary-figures/all-recycled-object-counts.tiff", 
       plot = ggarrange(p1, test.p1, 
                        common.legend = T, labels = "AUTO", 
                        ncol = 2, nrow = 1), 
       dpi = 300, width = 10, height = 8)


####assemblage sizes####
#detach(package:plyr)
assemblages = count.data %>% 
  group_by_at(c(parameters, "run", "row", "col")) %>%
  summarize(obj_count = sum(total_count), 
            ri = sum(count_recycled)/sum(total_count))

calc.skew = assemblages %>% filter(overlap == 1) %>%
  group_by(num_agents, blank_prob, scavenge_prob, mu) %>%
  summarize(skew.obj = skewness(obj_count), 
            skew.ri = skewness(ri))

summary(lm(skew.obj ~ blank_prob*scavenge_prob + num_agents + mu, data = calc.skew))
summary(lm(skew.ri ~ blank_prob*scavenge_prob + num_agents + mu, data = calc.skew))

assem.dist = ggplot(assemblages %>% filter(overlap == 1)) +
  geom_density(aes(obj_count, fill = as.factor(num_agents), group = as.factor(num_agents))) + 
  facet_grid(blank_prob ~ scavenge_prob , 
             labeller = labeller(
               blank_prob = blank.labs, 
               scavenge_prob = scvg.labs,
             )) +
  scale_color_brewer(palette = "Set1") +
  labs(fill = "number of agents", x = "object count") +
  theme(legend.position = "bottom")

#plot(assem.dist)

ggsave(filename = "../figures/supplementary-figures/assemblage-size-distribtuions.tiff", 
       assem.dist, 
       dpi = 300, width = 8, height = 6)

ggplot(assemblages %>% filter(overlap == 1)) +
  geom_density(aes(ri, color = as.factor(num_agents), group = as.factor(num_agents))) + 
  facet_grid(blank_prob ~ scavenge_prob , 
             labeller = labeller(
               blank_prob = blank.labs, 
               scavenge_prob = scvg.labs,
             )) +
  scale_color_brewer(palette = "Pastel1")
  
  
  
#### RI and assemblage density #####
parameters = colnames(count.data[,c(10:22)])
sub.count = count.data %>% filter(overlap == 1) %>%
  group_by_at(c(parameters, "run")) %>%
  reframe(obj_count = sum(total_count),
          ri = sum(count_recycled)/sum(total_count), 
          log.count = log(obj_count))

ggplot(sub.count, aes(x = log.count, y = ri)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40")

density.ri = count.data %>% 
  group_by_at(c(parameters, "run")) %>%
  reframe(obj_count = sum(total_count),
          ri = sum(count_recycled)/sum(total_count), 
          log.count = log(obj_count))

ggplot(density.ri, aes(x = obj_count, y = ri)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), color = "grey40") +
  facet_grid(num_agents~overlap)

p = ggplot(sub.count, aes(x = log.count, y = ri)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), color = "grey40") +
  facet_grid(num_agents ~ mu, 
             labeller = labeller(
               num_agents = occup.labs,
               mu = mu.labs
             )) +
  labs(x = "log(artifact count)", y = "recycling incidence")
ggsave(filename = "../figures/supplementary-figures/ri_assemblage-density_movement-occup.tiff", 
       p, 
       dpi = 300, width = 10, height = 6.5)

p.s = ggplot(sub.count, aes(x = log.count, y = ri)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), color = "grey40", size = 2) +
  facet_wrap(~ flake_preference + size_preference + strict_selection, 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs
             ), nrow = 1) +
  labs(x = "log(artifact count)", y = "recycling incidence")
#plot(p.s)
ggsave(filename = "../figures/supplementary-figures/ri_assemblage-density_selection.tiff", 
       p.s, 
       dpi = 300, width = 12, height = 4)

ggplot(sub.count, aes(x = log.count, y = ri)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  #geom_abline(intercept = 0, color = "red", linetype = "dashed") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq","R2")), vstep = 0.075, size = 2.5) +
  scale_color_colorblind() +
  facet_grid(blank_prob ~ scavenge_prob, 
             labeller = labeller(
               blank_prob = blank.labs, 
               scavenge_prob = scvg.labs
             )) +
  labs(x = "log(artifact count)", y = "recycling incidence")


p1 = ggplot(sub.count %>% filter(num_agents == 100), 
            aes(x = log.count, y = ri, color = as.factor(mu), group = as.factor(mu))) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  #geom_abline(intercept = 0, color = "red", linetype = "dashed") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), vstep = 0.1, size = 3) +
  scale_color_colorblind(labels = mu.labs) +
  facet_grid( blank_prob ~ scavenge_prob, 
             labeller = labeller(
               blank_prob = blank.labs, 
               scavenge_prob = scvg.labs, 
               num_agents = occup.labs
             )) +
  labs(x = "log(artifact count)", y = "recycling incidence", color = "") +
  theme(strip.text = element_text(size = 7))
#plot(p1)
ggsave(filename = "../figures/ri_assemblage-density_probs.tiff", 
       p1, 
       dpi = 300, width = 8, height = 6.5)

p1.o = ggplot(sub.count %>% filter(num_agents == 200), 
            aes(x = log.count, y = ri, color = as.factor(mu), group = as.factor(mu))) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  #geom_abline(intercept = 0, color = "red", linetype = "dashed") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), vstep = 0.1, size = 3) +
  scale_color_colorblind(labels = mu.labs) +
  facet_grid( blank_prob ~ scavenge_prob, 
              labeller = labeller(
                blank_prob = blank.labs, 
                scavenge_prob = scvg.labs, 
                num_agents = occup.labs
              )) +
  labs(x = "log(artifact count)", y = "recycling incidence", color = "") +
  theme(strip.text = element_text(size = 7))
#plot(p1)
ggsave(filename = "../figures/supplementary-figures/ri_assemblage-density_probs.tiff", 
       p1.o, 
       dpi = 300, width = 8, height = 6.5)

#### RI and retouched artifact proporiton #####
sub.count = count.data %>% filter(overlap == 1) %>%
  mutate(ri = count_recycled/total_count, 
         retprop = count_retouched/total_count)

p2 = ggplot(sub.count %>% filter(num_agents == 100), aes(x = retprop, y = ri)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  facet_grid( ~ mu, 
              labeller = labeller(
                num_agents = occup.labs,
                mu = mu.labs
              )) +
  labs(x = "retouched artifact proportion", y = "recycling incidence")
ggsave(filename = "../figures/supplementary-figures/ri_retouched-prop.tiff", 
       p2, 
       dpi = 300, width = 8, height = 4)

