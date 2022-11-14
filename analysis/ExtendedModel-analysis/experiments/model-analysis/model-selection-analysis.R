library(tidyverse)
library(ggthemes)
library(ggpubr)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")
grouping_params = c("mu", "overlap")

exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")

flakepref = alldata[which(alldata$size_preference == FALSE & alldata$strict_selection == FALSE ), ]
sizepref.flakes = alldata[which(alldata$flake_preference == TRUE & alldata$strict_selection == FALSE ), ]
sizepref.nodules = alldata[which(alldata$flake_preference == FALSE & alldata$strict_selection == FALSE ), ]
strict.flakes = alldata[which(alldata$size_preference == FALSE & alldata$flake_preference == TRUE ), ]
strict.nodules = alldata[which(alldata$size_preference == FALSE & alldata$flake_preference == FALSE ), ]

##TODO add statistical tests of differences

#### FLAKE PREFERENCE ####
ks.test(
  x = flakepref[which(flakepref$flake_preference == T),]$total.RI, 
  y = flakepref[which(flakepref$flake_preference == F),]$total.RI,
  alternative = "two.sided", 
  simulate.p.value = T 
)

p1 = ggplot(flakepref, aes(x = flake_preference, y = total.RI, color = flake_preference, group = flake_preference)) +
  geom_boxplot() + 
  stat_compare_means(method = "wilcox", ref.group = "FALSE") +
  scale_color_colorblind() +
  theme(legend.position = "none")
#plot(p1)
ggsave(p1, filename = "flake-preference-wilcox.tif", dpi = 300)


#### SIZE PREFERENCE ON FLAKES ####
ks.test(
  x = sizepref.flakes[which(sizepref.flakes$size_preference == T),]$total.RI, 
  y = sizepref.flakes[which(sizepref.flakes$size_preference == F),]$total.RI,
  alternative = "two.sided", 
  simulate.p.value = T 
)

p2 = ggplot(sizepref.flakes, aes(x = size_preference, y = total.RI, color = size_preference, group = size_preference)) +
  geom_boxplot() + 
  stat_compare_means(method = "wilcox", ref.group = "FALSE") +
  scale_color_colorblind() +
  theme(legend.position = "none")
#plot(p2)


#### SIZE PREFERENCE ON NODULES ####
ks.test(
  x = sizepref.nodules[which(sizepref.nodules$size_preference == T),]$total.RI, 
  y = sizepref.nodules[which(sizepref.nodules$size_preference == F),]$total.RI,
  alternative = "two.sided", 
  simulate.p.value = T 
)

p3 = ggplot(sizepref.nodules, aes(x = size_preference, y = total.RI, color = size_preference, group = size_preference)) +
  geom_boxplot() + 
  stat_compare_means(method = "wilcox", ref.group = "FALSE") +
  scale_color_colorblind() +
  theme(legend.position = "none")
#plot(p3)

ggsave(cowplot::plot_grid(p2, p3, labels = "auto"), filename = "size-preference-wilcox.tif", dpi = 300)

#### STRICT SELECTION ON FLAKES ####
ks.test(
  x = strict.flakes[which(strict.flakes$strict_selection == T),]$total.RI, 
  y = strict.flakes[which(stict.flakes$strict_selection == F),]$total.RI,
  alternative = "two.sided", 
  simulate.p.value = T 
)

p4 = ggplot(strict.flakes, aes(x = strict_selection, y = total.RI, color = strict_selection, group = strict_selection)) +
  geom_boxplot() + 
  stat_compare_means(method = "wilcox", ref.group = "FALSE") +
  scale_color_colorblind() +
  theme(legend.position = "none")
#plot(p4)


#### STRICT SELECTION ON NODULES ####
ks.test(
  x = strict.nodules[which(strict.nodules$strict_selection == T),]$total.RI, 
  y = strict.nodules[which(stict.nodules$strict_selection == F),]$total.RI,
  alternative = "two.sided", 
  simulate.p.value = T 
)

p5 = ggplot(strict.nodules, aes(x = strict_selection, y = total.RI, color = strict_selection, group = strict_selection)) +
  geom_boxplot() + 
  stat_compare_means(method = "wilcox", ref.group = "FALSE") +
  scale_color_colorblind() +
  theme(legend.position = "none")
#plot(p5)

ggsave(cowplot::plot_grid(p4, p5, labels = "auto"), filename = "strict-selection-wilcox.tif", dpi = 300)
