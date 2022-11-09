### Analysis of model outcomes by mobility

library(tidyverse)
library(ggthemes)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")

mu.1 = alldata[which(alldata$mu == 1),]
mu.2 = alldata[which(alldata$mu == 2),]
mu.3 = alldata[which(alldata$mu == 3),]

alldata = alldata %>% group_by(model_year) %>%
  mutate(RI.lower = min(total.RI, na.rm = T), 
         RI.upper = max(total.RI, na.rm = T))

ggsave(filename = "/scratch/ec3307/recycling-Java/results/recycling-intensity-over-time.tif",
  ggplot(alldata) +
  geom_linerange(aes(x = model_year, ymin = RI.lower, ymax = RI.upper), alpha = 0.1, size = 0.01) +
  geom_smooth(aes(x = model_year, y = total.RI)) +
  facet_grid(overlap~mu, labeller = label_both) +
  scale_x_reverse(), 
  dpi = 300
)


###need to know distributions to do regressions

