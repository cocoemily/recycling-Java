#analysis of variation among model runs
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(parallel)
library(foreach)
library(doParallel)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

# allruns = alldata[which(alldata$model_year == 500000),]
# exp_test = distinct(allruns[parameters])
# print(nrow(exp))
exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")

#dirs = list.dirs("../output/test-layer-data")
dirs = list.dirs("/scratch/ec3307/recycling-Java/output")
##remove folders refering to artifact data
dirs = dirs[-c(1:3)]
dirs = dirs[-length(dirs)]

if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}
print(ncores)
registerDoParallel(ncores)
Sys.setenv(OMP_NUM_THREADS = "1")

foreach (d=1:length(dirs)) %dopar% { 
  data = read_csv(paste0(dirs[d], "/model-data.csv"), num_threads=1)
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
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
  
  modelyears = unique(data$model_year)
  
  for(year in 1:length(modelyears)) {
    oneyear = data[which(data$model_year == modelyears[year]),]
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
  
  write_csv(allvar, file = paste0("/scratch/ec3307/recycling-Java/output/model-output/model-output/", filename, "_variation.csv"), num_threads=1)
  
}
