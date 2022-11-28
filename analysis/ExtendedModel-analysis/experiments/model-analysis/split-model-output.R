#SPLIT MODEL RUNS BY EXPERIMENT
library(tidyverse)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

# allruns = alldata[which(alldata$model_year == 500000),]
# exp_test = distinct(allruns[parameters])
# print(nrow(exp))
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters1.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters2.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters3.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters4.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters5.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters6.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters7.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters8.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters9.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters10.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters11.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters12.csv")
#exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters13.csv")

colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")

for(i in 1:nrow(exp)) {
  oneexp = alldata[which(
    alldata[parameters[1]] == c(exp[i, parameters[1]]) &
      alldata[parameters[2]] == c(exp[i, parameters[2]]) &
      alldata[parameters[3]] == c(exp[i, parameters[3]]) &
      alldata[parameters[4]] == c(exp[i, parameters[4]]) &
      alldata[parameters[5]] == c(exp[i, parameters[5]]) &
      alldata[parameters[6]] == c(exp[i, parameters[6]]) &
      alldata[parameters[7]] == c(exp[i, parameters[7]]) &
      alldata[parameters[8]] == c(exp[i, parameters[8]]) &
      alldata[parameters[9]] == c(exp[i, parameters[9]]) &
      alldata[parameters[10]] == c(exp[i, parameters[10]]) &
      alldata[parameters[11]] == c(exp[i, parameters[11]]) &
      alldata[parameters[12]] == c(exp[i, parameters[12]]) &
      alldata[parameters[13]] == c(exp[i, parameters[13]])
  ),]
  
  write_csv(oneexp, paste0("/scratch/ec3307/recycling-Java/output/model-output/exp", i, "-model-output.csv"))
}
