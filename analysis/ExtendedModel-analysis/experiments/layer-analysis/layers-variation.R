library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

colnames(param_list) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")
param_list = param_list[, c("exp", parameters)]

#dirs = list.dirs("../output/test-layer-data/")
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
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1)
  print(dirs[d])
  
  #end_data = data[which(data$model_year == 200000), ]
  end_data = data[which(data$model_year == 350000), ]
  rm(data)
  
  #analysis of variation at each grid square over the 50 runs
  grid = expand.grid(0:9, 0:9)
  colnames(grid) = c("row", "col")
  
  dirsplit = str_split(dirs[d], "\\/")[[1]]
  expnum = str_extract(dirsplit[length(dirsplit)], "[0-9]+")
  exp_values = param_list[which(param_list$exp == as.numeric(expnum)), ]
  print(exp_values)
  
  variation_vals = data.frame(
    nodule.count.sd = NA, 
    flake.count.sd = NA,
    #assemblage.vol.sd = NA,
    cortex.ratio.sd = NA,
    recycling.intensity.sd = NA,
    num.discards.sd = NA,
    num.scavenge.sd = NA,
    num.encounters.sd = NA, 
    num.manufacture.sd = NA, 
    num.retouch.sd = NA, 
    num.occupation.sd = NA
  )
  var_outputs = colnames(variation_vals)
  
  grid = cbind(grid, rep(exp_values), rep(variation_vals), row = 0, col = 0)
  
  for(i in 1:nrow(grid)){
    square_data = end_data[which(end_data$row == grid$row[i] & end_data$col == grid$col[i]),]
    
    grid[i, var_outputs] = c(
      sd(square_data$nodule.count, na.rm = T), 
      sd(square_data$flake.count, na.rm = T), 
      #sd(square_data$assemblage.vol, na.rm = T), 
      sd(square_data$cortex.ratio, na.rm = T), 
      sd(square_data$recycling.intensity, na.rm = T), 
      sd(square_data$num.discards, na.rm = T), 
      sd(square_data$num.scavenge, na.rm = T), 
      sd(square_data$num.encounters, na.rm = T), 
      sd(square_data$num.manufacture, na.rm = T), 
      sd(square_data$num.retouch, na.rm = T), 
      sd(square_data$num.occupation, na.rm = T)
    )
    
    grid[i, 27:28] = c(square_data$row[1], square_data$col[1])
    
  }
  

    
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  write_csv(grid, file = paste0("/scratch/ec3307/recycling-Java/output/layer-output/", filename, "_layer-variation.csv"), num_threads=1)
}
