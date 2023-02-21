library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")
#model_var = read_csv("~/eclipse-workspace/recycling-Java/results/all-model-variation.csv", n_max = 300000)
model_var = read_csv("/scratch/ec3307/recycling-Java/results/all-model-variation.csv")

rcycl.obj = model_var[,c(parameters, "num.rcycl.obj.made", "model_year")]
blank.events = model_var[,c(parameters, "num.blank.events", "model_year")]
retouch.events = model_var[,c(parameters, "num.retouch.events", "model_year")]
discard.events = model_var[,c(parameters, "num.discard.events", "model_year")]
scavenge.events = model_var[,c(parameters, "num.scav.events", "model_year")]

rm(model_var)

#### Variation of recycled objects made ####
r1 = ggplot(rcycl.obj) +
  geom_smooth(aes(x = model_year, y = num.rcycl.obj.made, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "mu", x = "model year", y = "variance of number of recycled objects made")
#ggsave(filename = "rcycl-obj-var_mu.png", plot=r1, dpi = 300)

r2 = ggplot(rcycl.obj) +
  geom_smooth(aes(x = model_year, y = num.rcycl.obj.made, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of recycled objects made")
#ggsave(filename = "rcycl-obj-var_overlap.png", plot=r2, dpi = 300)

r3 = ggplot(rcycl.obj) +
  geom_smooth(aes(x = model_year, y = num.rcycl.obj.made, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both)+
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "variance of number of recycled objects made")
#ggsave(filename = "rcycl-obj-var_selection.png", plot=r3, dpi = 300)

ggsave(filename = "rcycl-obj-var.tiff",
  ggarrange(r1, r2, r3, ncol = 3, nrow = 1, legend = "bottom", labels = "AUTO"), 
  dpi = 300, width = 10, height = 6)

#### Variation of blank creation events ####
b1 = ggplot(blank.events) +
  geom_smooth(aes(x = model_year, y = num.blank.events, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "mu", x = "model year", y = "variance of number of blank events")
#ggsave(filename = "blank-events-var_mu.png", plot=b1, dpi = 300)

b2 = ggplot(blank.events) +
  geom_smooth(aes(x = model_year, y = num.blank.events, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of blank events")
#ggsave(filename = "blank-events-var_overlap.png", plot=b2, dpi = 300)

b3 = ggplot(blank.events) +
  geom_smooth(aes(x = model_year, y = num.blank.events, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind()  +
  labs(color = "flake preference", x = "model year", y = "variance of number of blank events")
#ggsave(filename = "blank-events-var_selection.png", plot=b3, dpi = 300)

ggsave(filename = "blank-events-var.tiff",
       ggarrange(b1, b2, b3, ncol = 3, nrow = 1, legend = "bottom", labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

#### Variation of retouch events ####
rt1 = ggplot(retouch.events) +
  geom_smooth(aes(x = model_year, y = num.retouch.events, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind()  +
  labs(color = "mu", x = "model year", y = "variance of number of retouch events")
#ggsave(filename = "retouch-events-var_mu.png", plot=rt1, dpi = 300)

rt2 = ggplot(retouch.events) +
  geom_smooth(aes(x = model_year, y = num.retouch.events, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of retouch events")
#ggsave(filename = "retouch-events-var_overlap.png", plot=rt2, dpi = 300)

rt3 = ggplot(retouch.events) +
  geom_smooth(aes(x = model_year, y = num.retouch.events, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "variance of number of retouch events")
#ggsave(filename = "retouch-events-var_selection.png", plot=rt3, dpi = 300)

ggsave(filename = "retouch-events-var.tiff",
       ggarrange(rt1, rt2, rt3, ncol = 3, nrow = 1, legend = "bottom", labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

#### Variation of discard events ####
d1 = ggplot(discard.events) +
  geom_smooth(aes(x = model_year, y = num.discard.events, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind()  +
  labs(color = "mu", x = "model year", y = "variance of number of discard events")
#ggsave(filename = "discard-events-var_mu.png", plot=d1, dpi = 300)

d2 = ggplot(discard.events) +
  geom_smooth(aes(x = model_year, y = num.discard.events, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of discard events")
#ggsave(filename = "discard-events-var_overlap.png", plot=d2, dpi = 300)

d3 = ggplot(discard.events) +
  geom_smooth(aes(x = model_year, y = num.discard.events, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "variance of number of discard events")
#ggsave(filename = "discard-events-var_selection.png", plot=d3, dpi = 300)

ggsave(filename = "discard-events-var.tiff",
       ggarrange(d1, d2, d3, ncol = 3, nrow = 1, legend = "bottom", labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

#### Variation of scavenging events ####
s1 = ggplot(scavenge.events) +
  geom_smooth(aes(x = model_year, y = num.scav.events, color = as.factor(mu), group = as.factor(mu))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind()  +
  labs(color = "mu", x = "model year", y = "variance of number of scavenging events")
#ggsave(filename = "scavenge-events-var_mu.png", plot=s1, dpi = 300)

s2 = ggplot(scavenge.events) +
  geom_smooth(aes(x = model_year, y = num.scav.events, color = as.factor(overlap), group = as.factor(overlap))) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "overlap", x = "model year", y = "variance of number of scavenging events")
#ggsave(filename = "scavenge-events-var_overlap.png", plot=s2, dpi = 300)

s3 = ggplot(scavenge.events) +
  geom_smooth(aes(x = model_year, y = num.scav.events, color = as.factor(flake_preference), group = as.factor(flake_preference))) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "variance of number of scavenging events")
#ggsave(filename = "scavenge-events-var_selection.png", plot=d3, dpi = 300)

ggsave(filename = "scavenge-events-var.tiff",
       ggarrange(s1, s2, s3, ncol = 3, nrow = 1, legend = "bottom", labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

