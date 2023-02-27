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
  labs(x = "time") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))
plot(p1)

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
  labs(x = "time") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))

ggsave(filename = "../figures/supplementary-figures/artifact-counts.tiff", 
       ggarrange(p1, p2, common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

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

