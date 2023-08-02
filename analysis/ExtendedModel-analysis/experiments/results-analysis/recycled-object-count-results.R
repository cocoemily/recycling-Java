##recycled object counts
library(tidyverse)
library(ggthemes)
library(jtools)
library(ggpubr)
library(raster)
library(sp)
library(rgdal)
library(tmap)
library(spdep)
library(pscl)
library(Dict)
library(MASS)
library(QuantPsyc)

theme_set(theme_bw())

#count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-recycled-object-counts.csv")
count.data = read_csv("/scratch/ec3307/recycling-Java/results/all-recycled-object-counts.csv")

parameters = colnames(count.data[,6:17])
#param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = param_list[,c(6:17)]
colnames(param_list) = parameters

#### recycled object counts ####
run.avg = count.data %>%
  group_by_at(c("obj_type", "time", parameters)) %>%
  summarize(avg_recycled = mean(count_recycled))

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")

p1 = ggplot(run.avg %>% filter(overlap == 2)) +
  #geom_boxplot(aes(x = time, y = avg_recycled, group = time), alpha = 0.25, color = "grey20") +
  geom_boxplot(aes(x = time, y = avg_recycled, fill = obj_type)) +
  facet_grid(flake_preference + size_preference ~ strict_selection ,
             labeller = labeller(flake_preference = flake.labs,
                                 size_preference = size.labs,
                                 strict_selection = strict.labs)) +
  scale_x_discrete(limits = rev) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "number of recycled objects")
plot(p1)

p2 = ggplot(run.avg %>% filter(overlap == 1)) +
  geom_boxplot(aes(x = time, y = avg_recycled, fill = obj_type)) +
  facet_grid(flake_preference + size_preference ~ strict_selection ,
             labeller = labeller(flake_preference = flake.labs,
                                 size_preference = size.labs,
                                 strict_selection = strict.labs)) +
  scale_x_discrete(limits = rev) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "number of recycled objects")
plot(p2)

ggsave(filename = "../figures/supplementary-figures/recycled-object-counts.tiff",
       plot = ggarrange(p2, p1,
                        common.legend = T, labels = "AUTO"),
       dpi = 300, width = 10, height = 6)


test.p1 = ggplot(run.avg %>% filter(overlap == 1), aes(x = time, y = avg_recycled, fill = obj_type, group =obj_type)) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9),
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) +
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) -
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference ~ strict_selection ,
             labeller = labeller(flake_preference = flake.labs,
                                 size_preference = size.labs,
                                 strict_selection = strict.labs)) +
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(limits = rev) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of recycled objects")
plot(test.p1)

test.p2 = ggplot(run.avg %>% filter(overlap == 2), aes(x = time, y = avg_recycled, fill = obj_type, group =obj_type)) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9),
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) +
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) -
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference ~ strict_selection ,
             labeller = labeller(flake_preference = flake.labs,
                                 size_preference = size.labs,
                                 strict_selection = strict.labs)) +
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(limits = rev) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of recycled objects")
plot(test.p2)

ggsave(filename = "../figures/supplementary-figures/run-averaged_recycled-object-counts.tiff",
       plot = ggarrange(test.p1, test.p2,
                        common.legend = T, labels = "AUTO"),
       dpi = 300, width = 10, height = 6)

ggsave(filename = "../figures/supplementary-figures/all-recycled-object-count-data.tiff",
       plot = ggarrange(p2, p1, test.p1, test.p2,
                        common.legend = T, labels = "AUTO", ncol = 2, nrow = 2),
       dpi = 300, width = 10, height = 10)


mu.p1 = ggplot(run.avg %>% filter(overlap == 1),
               aes(x = time, y = avg_recycled, fill = interaction(obj_type, as.factor(mu)), group = interaction(obj_type, as.factor(mu)))) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9), color = "grey40",
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference ~ strict_selection, 
             labeller = labeller(flake_preference = flake.labs, 
                                 size_preference = size.labs, 
                                 strict_selection = strict.labs)) +
  scale_fill_manual(
    values = c("#999999", "#000000", "#f5d999", "#E69F00", "#bbe1f6", "#56B4E9"), 
    labels = c("mu = 1 (flakes)", "mu = 1 (nodules)", "mu = 2 (flakes)", "mu = 2 (nodules)", "mu = 3 (flakes)", "mu = 3 (nodules)")
  ) +
  scale_x_discrete(limits = rev, labels = c("middle", "end")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of recycled objects", y = "time")
plot(mu.p1)

mu.p2 = ggplot(run.avg %>% filter(overlap == 2),
               aes(x = time, y = avg_recycled, fill = interaction(obj_type, as.factor(mu)), group = interaction(obj_type, as.factor(mu)))) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9), color = "grey40",
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference ~ strict_selection, 
             labeller = labeller(flake_preference = flake.labs, 
                                 size_preference = size.labs, 
                                 strict_selection = strict.labs)) +
  scale_fill_manual(
    values = c("#999999", "#000000", "#f5d999", "#E69F00", "#bbe1f6", "#56B4E9"), 
    labels = c("mu = 1 (flakes)", "mu = 1 (nodules)", "mu = 2 (flakes)", "mu = 2 (nodules)", "mu = 3 (flakes)", "mu = 3 (nodules)")
  ) +
  scale_x_discrete(limits = rev, labels = c("middle", "end")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of recycled objects", y = "time")
plot(mu.p2)

ggsave(filename = "../figures/supplementary-figures/run-averaged_recycled-object-counts-by-mu.tiff", 
       plot = ggarrange(mu.p1, mu.p2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

#### artifact counts by mu, all ####
mu.all1 = ggplot(run.avg %>% filter(overlap == 1), 
                 aes(x = as.factor(time), y = avg_recycled, fill = as.factor(mu), group = as.factor(mu))) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9), color = "grey40",
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference ~ strict_selection, 
             labeller = labeller(flake_preference = flake.labs, 
                                 size_preference = size.labs, 
                                 strict_selection = strict.labs)) +
  scale_fill_colorblind(labels = c("mu = 1", "mu = 2", "mu = 3"))+
  scale_x_discrete(limits = rev, labels = c("middle", "end")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of recycled objects", x = "time")
plot(mu.all1)

mu.all2 = ggplot(run.avg %>% filter(overlap == 2), 
                 aes(x = as.factor(time), y = avg_recycled, fill = as.factor(mu), group = as.factor(mu))) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9), color = "grey40",
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  facet_grid(flake_preference + size_preference ~ strict_selection, 
             labeller = labeller(flake_preference = flake.labs, 
                                 size_preference = size.labs, 
                                 strict_selection = strict.labs)) +
  scale_fill_colorblind(labels = c("mu = 1", "mu = 2", "mu = 3")) +
  scale_x_discrete(limits = rev, labels = c("middle", "end")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of recycled objects", x = "time")
plot(mu.all2)

ggsave(filename = "../figures/supplementary-figures/run-averaged_recycled-object-counts-by-mu_combined.tiff", 
       plot = ggarrange(mu.all1, mu.all2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)


#### rate of increase by mu ####
mu_end_increase = function(o = 1) {
  test = ggplot(run.avg %>% filter(overlap == o) %>% filter(time == "end"),
                aes(x = as.factor(mu), y = avg_recycled, fill = as.factor(mu)), group = as.factor(mu)) +
    stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
    stat_summary(geom = "errorbar", position = position_dodge(0.9), color = "grey40",
                 width = 0.05,
                 # fun.data = "mean_cl_normal") +
                 fun.max = function(x) mean(x) + 
                   qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
                 fun.min = function(x) mean(x) - 
                   qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
    stat_summary(fun = "mean", geom = "smooth", group = 1, color = "red", show.legend = F) +
    facet_grid(flake_preference + size_preference ~ strict_selection, 
               labeller = labeller(flake_preference = flake.labs, 
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs)) +
    scale_fill_colorblind(labels = c("mu = 1", "mu = 2", "mu = 3")) +
    theme(legend.title = element_blank(),
          strip.text = element_text(size = 8),
          axis.text = element_text(size = 6)) +
    labs(y = "average number of recycled objects", x = "mu")
  #plot(test)
  
  avg = ggplot_build(test)$data[[1]]
  slope = ggplot_build(test)$data[[3]]
  cal.slope = slope %>% group_by(PANEL) %>%
    summarize(slope1 = (y[2] - y[1]) / (x[2] - x[1]), 
              slope2 = (y[3] - y[2]) / (x[3] - x[2]), 
              max_y = max(y)) %>%
    mutate(
      flake_preference = c(rep(FALSE, 2), rep(TRUE, 4)), 
      size_preference = c(rep(FALSE, 4), rep(TRUE, 2)),
      strict_selection = c(F, T, F, T, F, T)
    ) %>%
    pivot_longer(c(slope1, slope2)) %>%
    mutate(x = ifelse(name == "slope1", 1.5, 2.5)) %>%
    mutate(value = round(value, digits = 2))
  test2 = test +
    geom_text(data = cal.slope %>% filter(value != 0), 
              aes(x = x, y = max_y, label = paste0("m = ", value)), inherit.aes = F, 
              nudge_y = 1, size = 2)
  return(test2)
}

rinc1 = mu_end_increase(1)
rinc2 = mu_end_increase(2)

#source("all-object-count-results.R") #run to get inc1 and inc2 for below plot

ggsave(filename = "../figures/supplementary-figures/slope-comparison-by-mu_combined.tiff", 
       plot = ggarrange(inc1, rinc1, inc2, rinc2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 10)


obj.avg = run.avg %>%
  group_by_at(c("time", parameters)) %>%
  summarize(all_avg = ceiling(sum(avg_recycled))) %>%
  filter(time == "end")
obj.avg$time = as.numeric(factor(obj.avg$time, levels = c("middle", "end")))

fitdistrplus::descdist(obj.avg$all_avg, discrete = T)

ar1 = glm(all_avg ~ ., data = obj.avg[c(2:4, 6:14)], family = "poisson")
summary(ar1)
df = as.data.frame(abs(lm.beta(ar1)))
colnames(df) = c("beta")
df$var = rownames(df)
df2 = df[order(df$beta),]
