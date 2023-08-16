##object counts

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(ggpmisc)
library(jtools)
library(pscl)
library(MASS)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")
#parameters = colnames(count.data[,c(10:22)])


flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)
mu.labs = c("mu = 1", "mu = 2", "mu = 3")
names(mu.labs) = c(1,2,3)

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


#### RI and assemblage density #####
sub.count = count.data %>% filter(overlap == 1) %>%
  mutate(ri = count_recycled/total_count, 
         log.count = log(total_count))

p = ggplot(sub.count, aes(x = log.count, y = ri)) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  facet_grid(num_agents ~ mu, 
             labeller = labeller(
               num_agents = occup.labs,
               mu = mu.labs
             )) +
  labs(x = "log(artifact count)", y = "recycling intensity")
ggsave(filename = "../figures/supplementary-figures/ri_assemblage-density.tiff", 
       p, 
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
  labs(x = "retouched artifact proportion", y = "recycling intensity")
ggsave(filename = "../figures/supplementary-figures/ri_retouched-prop.tiff", 
       p2, 
       dpi = 300, width = 8, height = 4)
