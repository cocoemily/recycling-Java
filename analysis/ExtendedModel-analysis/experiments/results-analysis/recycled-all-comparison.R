library(tidyverse)
library(ggthemes)
library(ggpubr)

theme_set(theme_bw())

####OLD DATAFRAME####
all = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")
recycled = read_csv("~/eclipse-workspace/recycling-Java/results/all-recycled-object-counts.csv")
parameters = colnames(recycled[,6:17])

all.time = all %>% filter(model_year == 200000 | model_year == 350000) %>%
  pivot_longer(cols = flake.count:nodule.count, names_to = "objects", values_to = "total_count") %>%
  mutate(time = ifelse(model_year == 200000, "end", "middle"), 
         obj_type = ifelse(objects == "flake.count", "flake", "nodule")) 

rcycl.run = recycled %>% 
  group_by_at(c("obj_type", "run", "time", parameters)) %>%
  summarize(count_recycled = sum(count_recycled), 
            count_retouched = sum(count_retouched))

joined = all.time %>% left_join(rcycl.run) %>%
  select_at(c(parameters, "run", "time", "obj_type", "total_count", "count_recycled", "count_retouched"))

joined = joined %>%
  mutate(recycle.prop = count_recycled/total_count) 
  

run.avg = joined %>%
  group_by_at(c("obj_type", "time", parameters)) %>%
  summarize(avg_rcycl_prop = mean(recycle.prop))


flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")


mu.p1 = ggplot(run.avg %>% filter(overlap == 1),
               aes(x = time, y = avg_rcycl_prop, fill = interaction(obj_type, as.factor(mu)), group = interaction(obj_type, as.factor(mu)))) +
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
  labs(y = "average proportion of recycled objects", y = "time")
plot(mu.p1)

mu.p2 = ggplot(run.avg %>% filter(overlap == 2),
               aes(x = time, y = avg_rcycl_prop, fill = interaction(obj_type, as.factor(mu)), group = interaction(obj_type, as.factor(mu)))) +
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
  labs(y = "average proportion of recycled objects", y = "time")
plot(mu.p2)

ggsave(filename = "../figures/supplementary-figures/run-averaged_recycled-props-by-mu.tiff", 
       plot = ggarrange(mu.p1, mu.p2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)


mu.all1 = ggplot(run.avg %>% filter(overlap == 1),
               aes(x = time, y = avg_rcycl_prop, fill = as.factor(mu), group = as.factor(mu))) +
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
  labs(y = "average proportion of recycled objects", y = "time")
plot(mu.all1)

mu.all2 = ggplot(run.avg %>% filter(overlap == 2),
               aes(x = time, y = avg_rcycl_prop, fill = as.factor(mu), group = as.factor(mu))) +
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
  labs(y = "average proportion of recycled objects", y = "time")
plot(mu.all2)

ggsave(filename = "../figures/supplementary-figures/run-averaged_recycled-props.tiff", 
       plot = ggarrange(mu.all1, mu.all2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)



p1 = ggplot(run.avg %>% filter(overlap == 2)) +
  #geom_boxplot(aes(x = time, y = avg_recycled, group = time), alpha = 0.25, color = "grey20") +
  geom_boxplot(aes(x = time, y = avg_rcycl_prop, fill = as.factor(mu))) +
  facet_grid(flake_preference + size_preference ~ strict_selection ,
             labeller = labeller(flake_preference = flake.labs,
                                 size_preference = size.labs,
                                 strict_selection = strict.labs)) +
  scale_x_discrete(limits = rev) +
  scale_fill_colorblind() +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "proportion of recycled objects")
plot(p1)

p2 = ggplot(run.avg %>% filter(overlap == 1)) +
  geom_boxplot(aes(x = time, y = avg_rcycl_prop, fill = as.factor(mu))) +
  facet_grid(flake_preference + size_preference ~ strict_selection ,
             labeller = labeller(flake_preference = flake.labs,
                                 size_preference = size.labs,
                                 strict_selection = strict.labs)) +
  scale_x_discrete(limits = rev) +
  scale_fill_colorblind() +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  labs(y = "proportion of recycled objects")
plot(p2)

ggsave(filename = "../figures/supplementary-figures/recycled-props.tiff",
       plot = ggarrange(p2, p1,
                        common.legend = T, labels = "AUTO"),
       dpi = 300, width = 10, height = 6)


ggplot(all.time %>% filter(overlap == 2) %>% filter(size_preference == T) %>% filter(time == "end"), 
       aes(x = as.factor(min_suitable_flake_size), y = total_count, fill = as.factor(min_suitable_flake_size))) +
  geom_boxplot() +
  facet_grid(~strict_selection, labeller = labeller(strict_selection = strict.labs))

ggplot(rcycl.run %>% filter(overlap == 2) %>% filter(size_preference == T) %>% filter(time == "end"), 
       aes(x = as.factor(min_suitable_flake_size), y = count_recycled, fill = as.factor(min_suitable_flake_size))) +
  geom_boxplot() +
  facet_grid(~strict_selection, labeller = labeller(strict_selection = strict.labs))

summary((rcycl.run %>% filter(overlap == 2) %>% filter(size_preference == T) %>% filter(time == "end") %>% filter(strict_selection == 1) %>% filter(min_suitable_flake_size == 2))$count_recycled)

####NEW DATAFRAME####
library(ggpmisc)

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")
mu.labs = c("mu = 1", "mu = 2", "mu = 3")
names(mu.labs) = c("1", "2", "3")


all = read_csv("~/eclipse-workspace/recycling-Java/results/object-counts-with-skew.csv")
parameters = colnames(all[,c(7:18)])

ri = all %>% 
  group_by_at(c(parameters, "row", "col", "run", "time")) %>%
  summarize(
    ri = count_recycled/total_count,
    count = sum(total_count),
  )
rm(list = c("all"))

sd(ri$ri)

ri.dens = ri %>% dplyr::filter(run %in% c("run1", "run2", "run3", "run4", "run5"))

dens = ggplot(ri.dens, aes(x = log(count), y = ri)) +
  geom_point(size = 0.5, alpha = 0.25, color = "cadetblue") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  # geom_smooth(method = "lm") +
  facet_grid(mu ~ blank_prob + scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs, 
    mu = mu.labs
  )) +
  labs(y = "recycling intensity", x = "log(artifact count)")
#plot(dens)
ggsave(filename = "../figures/supplementary-figures/ri_log-count_by-probs.tiff", 
       plot = dens, 
       dpi = 300, width = 16, height = 6)

ri.avg.end = ri %>% filter(time == "end") %>%
  group_by_at(c(parameters, "run")) %>%
  summarize(ri.avg = mean(ri),
            ri.var = var(ri),
            ri.sd = sd(ri))

ri.avg.mid = ri %>% filter(time == "middle") %>%
  group_by_at(c(parameters, "run")) %>%
  summarize(ri.avg = mean(ri),
            ri.var = var(ri),
            ri.sd = sd(ri))

rm(list = c("all", "ri"))


p1 = ggplot(ri.avg.end) +
  geom_hline(aes(yintercept = 1), color = "grey80", ) +
  geom_boxplot(aes(x = mu, y = ri.avg, group = mu)) +
  facet_grid(flake_preference  + size_preference ~ strict_selection, 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs
             ), scales = "free")
plot(p1)


p2 = ggplot(ri.avg.end) + 
  #geom_hline(aes(yintercept = 1), color = "grey80", ) +
  geom_boxplot(aes(x = mu, y = ri.sd, group = mu, fill = as.factor(mu)), color = "grey40") +
  #stat_compare_means(comparisons = mu.comparisons, method = "wilcox.test", label = "p.signif")
  facet_grid(flake_preference  + size_preference ~ strict_selection,
             labeller = labeller(
               flake_preference = flake.labs,
               size_preference = size.labs,
               strict_selection = strict.labs
             )) +
  scale_fill_colorblind() +
  labs(y = "standard deviation of recycling intensity") + 
  theme(legend.position = "none")
plot(p2)

ggsave(filename = "../figures/ri-sd-by-mobility-selection.tiff", 
       plot = p2, 
       dpi = 300, width = 7, height = 6)


ri.avg.end$time = "end"
ri.avg.mid$time = "middle"
ri.all = rbind(ri.avg.end, ri.avg.mid)
ri.all$time = factor(ri.all$time, levels = c("middle", "end"))

p3 = ggplot(ri.all) + 
  geom_boxplot(aes(x = time, y = ri.sd, group = time)) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  labs(y = "standard deviation of recycling intensity") + 
  theme(legend.position = "none")
plot(p3)

ggsave(filename = "../figures/ri-sd-by-probs.tiff", 
       plot = p3, 
       dpi = 300, width = 7, height = 6)

p4 = ggplot(ri.all) + 
  geom_boxplot(aes(x = time, y = ri.avg, group = time)) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  labs(y = "standard deviation of recycling intensity") + 
  theme(legend.position = "none")
plot(p4)
