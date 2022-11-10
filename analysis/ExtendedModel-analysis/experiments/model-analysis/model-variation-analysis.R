#analysis of variation among model runs
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(parallel)
library(foreach)
library(doParallel)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

# allruns = alldata[which(alldata$model_year == 500000),]
# exp_test = distinct(allruns[parameters])
# print(nrow(exp))
exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")



modelyears = unique(alldata$model_year)

if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}
print(ncores)
registerDoParallel(ncores)
Sys.setenv(OMP_NUM_THREADS = "1")

foreach (row=1:nrow(exp)) %dopar% { 
  allvar = data.frame(
    num.scav.events.var = numeric(), 
    total.recycled.var = numeric(), 
    num.deposits.var = numeric(), 
    total.encounters.var = numeric(), 
    total.discards.var = numeric(), 
    total.manu.events.var = numeric(), 
    total.retouches.var = numeric(), 
    total.CR.var = numeric(), 
    total.RI.var = numeric(), 
    exp = numeric(), 
    model_year = numeric()
  )
  
  oneexp = alldata[which(
    alldata[parameters[1]] == c(exp[row, parameters[1]]) &
      alldata[parameters[2]] == c(exp[row, parameters[2]]) &
      alldata[parameters[3]] == c(exp[row, parameters[3]]) &
      alldata[parameters[4]] == c(exp[row, parameters[4]]) &
      alldata[parameters[5]] == c(exp[row, parameters[5]]) &
      alldata[parameters[6]] == c(exp[row, parameters[6]]) &
      alldata[parameters[7]] == c(exp[row, parameters[7]]) &
      alldata[parameters[8]] == c(exp[row, parameters[8]]) &
      alldata[parameters[9]] == c(exp[row, parameters[9]]) &
      alldata[parameters[10]] == c(exp[row, parameters[10]]) &
      alldata[parameters[11]] == c(exp[row, parameters[11]]) &
      alldata[parameters[12]] == c(exp[row, parameters[12]]) &
      alldata[parameters[13]] == c(exp[row, parameters[13]])
  ),]
  
  for(year in 1:length(modelyears)) {
    oneyear = oneexp[which(oneexp$model_year == modelyears[year]),]
    var = c(
      var(oneyear$num.scav.events, na.rm = T), 
      var(oneyear$total.recycled, na.rm = T), 
      var(oneyear$num.deposits, na.rm = T), 
      var(oneyear$total.encounters, na.rm = T), 
      var(oneyear$total.discards, na.rm = T), 
      var(oneyear$total.manu.events, na.rm = T), 
      var(oneyear$total.retouches, na.rm = T), 
      var(oneyear$total.CR, na.rm = T), 
      var(oneyear$total.RI, na.rm = T), 
      row, 
      modelyears[year]
    )
    #print(var)
    
    allvar[nrow(allvar) + 1, ] = var
  }
  
  write_csv(allvar, file = paste0("/scratch/ec3307/recycling-Java/output/model-output/exp", row, "_variation.csv"), num_threads=1)
  
}

# ggsave(filename = "total-RI-variation.png", 
#        plot = ggplot(allvar, aes(x = model_year, y = total.RI.var, group = exp, color = exp)) +
#          geom_line() +
#          scale_color_colorblind()
# )
