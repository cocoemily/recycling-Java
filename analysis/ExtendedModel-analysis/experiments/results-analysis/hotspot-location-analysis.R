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
ri.scvg = read_csv("~/eclipse-workspace/recycling-Java/results/ri-scavenges-overlap-locations.csv")
ri.scvg = ri.scvg[,-1]
ri.flk = read_csv("~/eclipse-workspace/recycling-Java/results/ri-flake-counts-overlap-locations.csv")
ri.flk = ri.flk[,-1]
ri.nod = read_csv("~/eclipse-workspace/recycling-Java/results/ri-nodule-counts-overlap-locations.csv")
ri.nod = ri.nod[,-1]
ri.rp = read_csv("~/eclipse-workspace/recycling-Java/results/ri-retprop-overlap-locations.csv")
ri.rp = ri.rp[,-1]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")


loc.counts1 = ri.disc %>% 
  group_by_at(c(parameters, "run")) %>%
  summarize(count = n())
summary(loc.counts1$count)

loc.counts2 = ri.enct %>% 
  group_by_at(c(parameters, "run")) %>%
  summarize(count = n())
summary(loc.counts2$count)



plot_hotspot_locations = function(data) {
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

plot_hotspot_locations(ri.disc)
plot_hotspot_locations(ri.enct)
plot_hotspot_locations(ri.scvg)

plot_hotspot_locations(ri.flk)
plot_hotspot_locations(ri.nod)

plot_hotspot_locations(ri.rp)
plot_hotspot_locations(ret.enct)


#####hotspot locations#####
disc = read_csv("~/eclipse-workspace/recycling-Java/results/discards-hotspots-locations.csv")
disc = disc[,-1]
scvg = read_csv("~/eclipse-workspace/recycling-Java/results/scavenges-hotspots-locations.csv")
scvg = scvg[,-1]
enct = read_csv("~/eclipse-workspace/recycling-Java/results/encounters-hotspots-locations.csv")
enct = enct[,-1]
flk = read_csv("~/eclipse-workspace/recycling-Java/results/flake-counts-hotspots-locations.csv")
flk = flk[,-1]
nod = read_csv("~/eclipse-workspace/recycling-Java/results/nodule-counts-hotspots-locations.csv")
nod = nod[,-1]
cr = read_csv("~/eclipse-workspace/recycling-Java/results/cr-hotspots-locations.csv")
cr = cr[,-1]
retprop = read_csv("~/eclipse-workspace/recycling-Java/results/retprop-hotspots-locations.csv")
retprop = retprop[,-1]
ri = read_csv("~/eclipse-workspace/recycling-Java/results/ri-hotspots-locations.csv")
ri = ri[,-1]

plot_hotspot_locations(disc)
plot_hotspot_locations(enct)
plot_hotspot_locations(scvg)
plot_hotspot_locations(flk)
plot_hotspot_locations(nod)
plot_hotspot_locations(cr)
plot_hotspot_locations(retprop)
plot_hotspot_locations(ri)


