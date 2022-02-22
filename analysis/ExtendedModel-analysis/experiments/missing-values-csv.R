library(tidyverse)
library(readr)
library(naniar)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

alldata = alldata[alldata$size != "size",]

alldata = alldata[!is.na(alldata$max_artifact_carry),]

facet_data = alldata[,c(parameters, outputs)]

missing = facet_data %>% group_by_at(parameters) %>% miss_var_summary %>%
  filter(n_miss != 0)

write.csv(missing, file = "missing-values-by-experiment.csv")
