## ANALYSIS OF ASSOCIATION BETWEEN OUTPUT VARIABLES WITHIN FINAL GRID SQUARES
library(readr)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

#param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

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
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1, show_col_types = F)
  
  dirsplit = str_split(dirs[d], "\\/")[[1]]
  #print(dirsplit)
  expnum = str_extract(dirsplit[length(dirsplit)], "[0-9]+")
  print(expnum)
  
  years = unique(data$model_year)
  results = data[which(data$model_year == years[1]), 
                 c(parameters, "run", "model_year", "flake.count", "nodule.count")]
  results = results[0,]
  
  for(y in 1:length(years)) {
    allgrids = data[which(data$model_year == years[y]), 
                    c(parameters, "run", "model_year", "row", "col", "flake.count", "nodule.count")]
    if(years[y] == 200000) {
      allgrids = allgrids[1:5000,]
    }
    
    for(i in unique(allgrids$run)) {
      grid = allgrids[which(allgrids$run == i),]
      
      exp = c(grid[1, c(parameters, "run", "model_year")])
      exp[[15]] =  sum(grid$flake.count)
      exp[[16]] = sum(grid$nodule.count)
      
      results[nrow(results) + 1, ] <- exp
    }
    
  }
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  write_csv(results, file = paste0("/scratch/ec3307/recycling-Java/output/layer-output/exp", expnum, "_layer-obj-counts.csv"), num_threads=1)
  
}
