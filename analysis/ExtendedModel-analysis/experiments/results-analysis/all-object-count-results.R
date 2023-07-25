##recycled object counts

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(jtools)
library(pscl)
library(MASS)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")

ad = count.data %>% filter(model_year == 200000 | model_year == 350000) %>%
  pivot_longer(cols = flake.count:nodule.count, names_to = "object", values_to = "count")
ad$object = ifelse(str_detect(ad$object, "flake"), "flakes", "nodules")
parameters = colnames(ad[,1:12])

run.avg = ad %>%
  group_by_at(c("object", "model_year", parameters)) %>%
  summarize(avg_count = mean(count))

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")

#### artifact count box plots ####
p1 = ggplot(run.avg %>% filter(overlap == 1)) +
  geom_boxplot(aes(x = as.factor(model_year), y = avg_count, fill = object)) +
  facet_grid(flake_preference + size_preference ~ strict_selection , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs
             )) +
  scale_x_discrete(labels = c("middle", "end"), limits = rev) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "time", y = "number of objects") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))
plot(p1)

ggplot(run.avg %>% filter(overlap == 1) %>% filter(model_year == 200000)) +
  geom_boxplot(aes(x = as.factor(size_preference), y = avg_count)) +
  # facet_grid(flake_preference + size_preference ~ strict_selection , 
  #            labeller = labeller(
  #              flake_preference = flake.labs, 
  #              size_preference = size.labs, 
  #              strict_selection = strict.labs
  #            )) +
  #scale_x_discrete(labels = c("middle", "end"), limits = rev) +
  scale_fill_brewer(palette = "Paired") +
  #labs(x = "time", y = "number of objects") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))


p2 = ggplot(run.avg %>% filter(overlap == 2)) +
  geom_boxplot(aes(x = as.factor(model_year), y = avg_count, fill = object)) +
  facet_grid(flake_preference + size_preference ~ strict_selection , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs
             )) +
  scale_x_discrete(labels = c("middle", "end"), limits = rev) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "time", y = "number of objects") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))
plot(p2)

ggsave(filename = "../figures/supplementary-figures/artifact-counts.tiff", 
       ggarrange(p1, p2, common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)


#### average artifact counts ####
test.p1 = ggplot(run.avg %>% filter(overlap == 1), aes(x = as.factor(model_year), y = avg_count, fill = object, group = object)) +
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
  scale_x_discrete(limits = rev, labels = c("middle", "end")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of objects", x = "time")
plot(test.p1)

ggplot(run.avg %>% filter(overlap == 1)  %>% filter(model_year == 200000), aes(x = as.factor(flake_preference), y = avg_count)) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
  stat_summary(geom = "errorbar", position = position_dodge(0.9),
               width = 0.05,
               # fun.data = "mean_cl_normal") +
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
  # facet_grid(flake_preference + size_preference ~ strict_selection , 
  #            labeller = labeller(flake_preference = flake.labs, 
  #                                size_preference = size.labs, 
  #                                strict_selection = strict.labs)) +
  scale_fill_brewer(palette = "Paired") +
  #scale_x_discrete(limits = rev, labels = c("middle", "end")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))
  #labs(y = "average number of objects", x = "time")

test.p2 = ggplot(run.avg %>% filter(overlap == 2), aes(x = as.factor(model_year), y = avg_count, fill = object, group = object)) +
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
  scale_x_discrete(limits = rev, labels = c("middle", "end")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "average number of objects", x = "time")
plot(test.p2)


ggsave(filename = "../figures/supplementary-figures/run-averaged_object-counts.tiff", 
       plot = ggarrange(test.p1, test.p2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

ggsave(filename = "../figures/supplementary-figures/all-object-count-data.tiff", 
       plot = ggarrange(p2, p1, test.p1, test.p2, 
                        common.legend = T, labels = "AUTO", ncol = 2, nrow = 2), 
       dpi = 300, width = 10, height = 10)


#### artifact counts by mu, split flakes/nodules ####
mu.p1 = ggplot(run.avg %>% filter(overlap == 1), 
                aes(x = as.factor(model_year), y = avg_count, fill = interaction(object, as.factor(mu)), group = interaction(object, as.factor(mu)))) +
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
  labs(y = "average number of objects", x = "time")
plot(mu.p1)

mu.p2 = ggplot(run.avg %>% filter(overlap == 2), 
               aes(x = as.factor(model_year), y = avg_count, fill = interaction(object, as.factor(mu)), group = interaction(object, as.factor(mu)))) +
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
  labs(y = "average number of objects", x = "time")
plot(mu.p2)

ggsave(filename = "../figures/supplementary-figures/run-averaged_object-counts-by-mu.tiff", 
       plot = ggarrange(mu.p1, mu.p2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

#### artifact counts by mu, all ####
mu.all1 = ggplot(run.avg %>% filter(overlap == 1), 
               aes(x = as.factor(model_year), y = avg_count, fill = as.factor(mu), group = as.factor(mu))) +
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
  labs(y = "average number of objects", x = "time")
plot(mu.all1)

mu.all2 = ggplot(run.avg %>% filter(overlap == 2), 
               aes(x = as.factor(model_year), y = avg_count, fill = as.factor(mu), group = as.factor(mu))) +
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
  labs(y = "average number of objects", x = "time")
plot(mu.all2)

ggsave(filename = "../figures/supplementary-figures/run-averaged_object-counts-by-mu_combined.tiff", 
       plot = ggarrange(mu.all1, mu.all2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

#### rate of increase by mu ####
mu_end_increase = function(o = 1) {
  test = ggplot(run.avg %>% filter(overlap == o) %>% filter(model_year == 200000),
                aes(x = as.factor(mu), y = avg_count, fill = as.factor(mu)), group = as.factor(mu)) +
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
              nudge_y = 1000, size = 2)
  return(test2)
}

inc1 = mu_end_increase(1)
plot(inc1)
inc2 = mu_end_increase(2)
plot(inc2)


obj.avg = run.avg %>%
  group_by_at(c("model_year", parameters)) %>%
  summarize(all_avg = ceiling(sum(avg_count))) %>%
  filter(model_year == 200000)

fitdistrplus::descdist(obj.avg$all_avg, discrete = T)

ao1 = glm(all_avg ~ ., data = obj.avg[c(2:4, 6:14)], family = "poisson")
summary(ao1)
df = as.data.frame(abs(lm.beta(ao1)))
colnames(df) = c("beta")
df$var = rownames(df)
df2 = df[order(df$beta),]

