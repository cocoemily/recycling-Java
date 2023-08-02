#Counts of recycled nodules and flakes + retouched flakes
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(moments)

#param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(param_list) = c("exp", "run", "size", "start_year", "timestep", "max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size",  "strict_selection", "erosion_ratio", "geo_freq", "total_steps", "num_agents")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size",  "strict_selection")

param_list = param_list[, c("exp", parameters)]

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
  data = read_csv(paste0(dirs[d], "/artifacts-data.csv"), num_threads=1)
  
  expnum = str_extract(dirs[d], "[0-9]+")
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  exp_values = param_list[which(param_list$exp == as.numeric(expnum)), ]
  
  runs = unique(data$run)
  run.list = list()
  for(i in runs) {
    gridded.end = data %>% filter(run == i) %>%
      group_by(row, col) %>%
      mutate(skew = skewness(initial_discard)) %>%
      group_by(row, col, obj_type) %>%
      summarize(total_count = n(), 
                count_recycled = sum(recycled), 
                count_retouched = sum(stage > 0, na.rm = T), 
                skew = first(skew))
    
    gridded.end[,parameters[1]] = c(exp_values[1, c(parameters[1])])
    gridded.end[,parameters[2]] = c(exp_values[1, c(parameters[2])])
    gridded.end[,parameters[3]] = c(exp_values[1, c(parameters[3])])
    gridded.end[,parameters[4]] = c(exp_values[1, c(parameters[4])])
    gridded.end[,parameters[5]] = c(exp_values[1, c(parameters[5])])
    gridded.end[,parameters[6]] = c(exp_values[1, c(parameters[6])])
    gridded.end[,parameters[7]] = c(exp_values[1, c(parameters[7])])
    gridded.end[,parameters[8]] = c(exp_values[1, c(parameters[8])])
    gridded.end[,parameters[9]] = c(exp_values[1, c(parameters[9])])
    gridded.end[,parameters[10]] = c(exp_values[1, c(parameters[10])])
    gridded.end[,parameters[11]] = c(exp_values[1, c(parameters[11])])
    gridded.end[,parameters[12]] = c(exp_values[1, c(parameters[12])])
    gridded.end$run = i
    
    run.list[[length(run.list) + 1]] <- gridded.end
  }
  
  
  allresults = do.call("rbind", run.list[1:length(run.list)])
  
  write_csv(allresults, file = paste0("/scratch/ec3307/recycling-Java/output/artifact-output/", filename, "_object-counts.csv"), num_threads=1)
  
}