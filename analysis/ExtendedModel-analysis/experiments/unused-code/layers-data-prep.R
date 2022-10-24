library(tidyverse)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_layers_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

exp = as.data.frame(exp[,c(1,6:18)])
colnames(exp) = c("exp", parameters)

layers = alldata %>% left_join(exp, by = parameters)

readr::write_csv(layers[c(1:1000),], "test-layer-join.csv")