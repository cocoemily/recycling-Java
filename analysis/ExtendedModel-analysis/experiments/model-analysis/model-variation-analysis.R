#analysis of variation among model runs
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(parallel)
library(foreach)
library(doParallel)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

#dirs = list.dirs("../output/test-data")
dirs = list.dirs("/scratch/ec3307/recycling-Java/output")
dirs = dirs[grepl("exp", dirs)]

if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}
print(ncores)
registerDoParallel(ncores)
Sys.setenv(OMP_NUM_THREADS = "1")

foreach (d=1:length(dirs)) %dopar% { 
  data = readr::read_csv(paste0(dirs[d], "/model-data.csv"))
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  outputs = colnames(data[,21:33])
  
  allvar = data[1,c(parameters, outputs, "model_year")]
  allvar = allvar[0,]
  
 modelyears = unique(data$model_year)
  
  for(year in 1:length(modelyears)) {
    oneyear = data[which(data$model_year == modelyears[year]),]
    params = c(oneyear[1, parameters])
    vars = sapply(oneyear[,outputs], FUN=var)
    var = c(
      params, 
      vars,
      modelyears[year]
    )
    
    allvar[nrow(allvar) + 1, ] = var
  }
  
  readr::write_csv(allvar, file = paste0("/scratch/ec3307/recycling-Java/output/model-output/", filename, "_variation.csv"))
  
}
