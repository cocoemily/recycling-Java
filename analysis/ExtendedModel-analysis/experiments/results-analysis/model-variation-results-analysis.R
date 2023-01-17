library(tidyverse)
library(ggthemes)

theme_set(theme_bw())

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")
#model_var = read_csv("~/eclipse-workspace/recycling-Java/results/all-model-variation.csv", n_max = 300000)
model_var = read_csv("/scratch/ec3307/recycling-Java/results/all-model-variation.csv")

rcycl.obj = model_var[,c(parameters, "num.rcycl.obj.made", "model_year")]
blank.events = model_var[,c(parameters, "num.blank.events", "model_year")]
retouch.events = model_var[,c(parameters, "num.retouch.events", "model_year")]
#discard.events = model_var[,c(parameters, "num.discard.events", "model_year")]
#scavenge.events = model_var[,c(parameters, "num.scav.events", "model_year")]

rm(model_var)

#### Variation of recycled objects made ####
r1 = ggplot(rcycl.obj) +
  geom_smooth(aes(x = model_year, y = num.rcycl.obj.made, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "mu", x = "model year", y = "variance of number of recycled objects made")
ggsave(filename = "rcycl-obj-var_mu.png", plot=r1, dpi = 300)

r2 = ggplot(rcycl.obj) +
  geom_smooth(aes(x = model_year, y = num.rcycl.obj.made, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of recycled objects made")
ggsave(filename = "rcycl-obj-var_overlap.png", plot=r2, dpi = 300)

r3 = ggplot(rcycl.obj) +
  geom_smooth(aes(x = model_year, y = num.rcycl.obj.made, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both)+
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "variance of number of recycled objects made")
ggsave(filename = "rcycl-obj-var_selection.png", plot=r3, dpi = 300)

#### Variation of blank creation events ####
b1 = ggplot(blank.events) +
  geom_smooth(aes(x = model_year, y = num.blank.events, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "mu", x = "model year", y = "variance of number of blank events")
ggsave(filename = "blank-events-var_mu.png", plot=b1, dpi = 300)

b2 = ggplot(blank.events) +
  geom_smooth(aes(x = model_year, y = num.blank.events, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of blank events")
ggsave(filename = "blank-events-var_overlap.png", plot=b2, dpi = 300)

b3 = ggplot(blank.events) +
  geom_smooth(aes(x = model_year, y = num.blank.events, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both) +
  scale_x_reverse() +
  scale_color_colorblind()  +
  labs(color = "flake preference", x = "model year", y = "variance of number of blank events")
ggsave(filename = "blank-events-var_selection.png", plot=b3, dpi = 300)

#### Variation of retouch events ####
rt1 = ggplot(retouch.events) +
  geom_smooth(aes(x = model_year, y = num.retouch.events, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse() +
  scale_color_colorblind()  +
  labs(color = "mu", x = "model year", y = "variance of number of retouch events")
ggsave(filename = "retouch-events-var_mu.png", plot=rt1, dpi = 300)

rt2 = ggplot(retouch.events) +
  geom_smooth(aes(x = model_year, y = num.retouch.events, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of retouch events")
ggsave(filename = "retouch-events-var_overlap.png", plot=rt2, dpi = 300)

rt3 = ggplot(retouch.events) +
  geom_smooth(aes(x = model_year, y = num.retouch.events, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both) +
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "variance of number of retouch events")
ggsave(filename = "retouch-events-var_selection.png", plot=rt3, dpi = 300)

#lots of variation -- currently checking these
# summary(model_var$num.discard.events)
# summary(model_var$num.scav.events)
