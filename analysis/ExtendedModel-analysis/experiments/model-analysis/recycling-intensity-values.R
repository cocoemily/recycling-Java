library(tidyverse)
library(ggthemes)
library(scales)
library(ggpubr)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")
grouping_params = c("mu", "overlap")


enddata = alldata[which(alldata$model_year == 200000),]
summary(enddata$total.RI)
