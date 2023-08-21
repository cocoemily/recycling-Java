##hotspot location analysis

library(tidyverse)
library(ggpubr)
library(ggthemes)

theme_set(theme_minimal())

ret.enct = read_csv("~/eclipse-workspace/recycling-Java/results/retouch-encounter-overlap-locations.csv")
ret.enct = ret.enct[,-1]
ri.cr = read_csv("~/eclipse-workspace/recycling-Java/results/ri-cr-overlap-locations.csv")
ri.cr = ri.cr[,-1]
ri.disc = read_csv("~/eclipse-workspace/recycling-Java/results/ri-discards-overlap-locations.csv")
ri.disc = ri.disc[,-1]
ri.enct = read_csv("~/eclipse-workspace/recycling-Java/results/ri-encounters-overlap-locations.csv")
ri.enct = ri.enct[,-1]
ri.flk = read_csv("~/eclipse-workspace/recycling-Java/results/ri-flake-counts-overlap-locations.csv")
ri.flk = ri.flk[,-1]
ri.nod = read_csv("~/eclipse-workspace/recycling-Java/results/ri-nodule-counts-overlap-locations.csv")
ri.nod = ri.nod[,-1]
ri.rp = read_csv("~/eclipse-workspace/recycling-Java/results/ri-retprop-overlap-locations.csv")
ri.rp = ri.rp[,-1]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")


plot_hotspot_overlap_locations = function(data) {
  loc.counts = data %>% 
    group_by_at(c(parameters, "x", "y")) %>%
    summarize(count = n())
  summary(loc.counts$count)
  
  ggplot(loc.counts) +
    geom_tile(aes(x = as.factor(y), y = as.factor(x), fill = count), color = "black") +
    #geom_text(aes(x = as.factor(y), y = as.factor(x), label = count)) +
    scale_fill_gradient(low = "white", high = "firebrick") +
    scale_y_discrete(limits=rev) +
    facet_grid(overlap ~ num_agents + mu, labeller = label_both)
}

plot_hotspot_overlap_locations(ri.disc)
plot_hotspot_overlap_locations(ri.enct)

plot_hotspot_overlap_locations(ri.flk)
plot_hotspot_overlap_locations(ri.nod)

plot_hotspot_overlap_locations(ri.rp)
plot_hotspot_overlap_locations(ret.enct)
