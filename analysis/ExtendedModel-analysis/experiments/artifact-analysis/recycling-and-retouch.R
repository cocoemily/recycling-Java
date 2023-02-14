#Cooccurrence of retouched artifacts and recycled artifacts
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

#param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size",  "strict_selection")

colnames(param_list) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")
param_list = param_list[, c("exp", parameters)]


#files = list.files("../output/test-data/")
files = list.files("/scratch/ec3307/recycling-Java/output/artifact-data/")
files = files[-length(files)]


if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}
print(ncores)
registerDoParallel(ncores)
Sys.setenv(OMP_NUM_THREADS = "1")


foreach (f=1:length(files)) %dopar% {
  expnum = str_extract(files[f], "[0-9]+")
  
  #data = read_csv(paste0("../output/test-data/", files[f], "/artifacts-data.csv"))
  data = read_csv(paste0("/scratch/ec3307/recycling-Java/output/artifact-data/", files[f]), num_threads=1)
  print(files[f])
  
  exp_values = param_list[which(param_list$exp == as.numeric(expnum)), ]
  
  #mid_data = data[which(data$model_year == 350000), ]
  end_data = data[which(data$model_year == 200000), ]
  
  gridded = end_data %>% group_by(row, col) %>%
    mutate(square = cur_group_id())
  gridded$init_rownum = rownames(gridded)
  
  sub1 = detect_index(gridded$square, function(x) x == 100)
  index1 = as.numeric(gridded[sub1, "init_rownum"])
  subset = gridded[sub1:nrow(gridded),]
  sub2 = detect_index(subset$square, function(x) x == 1)
  index2 = as.numeric(subset[sub2, "init_rownum"])
  onerun = gridded[1:index2,]
  
  onerun.grid = onerun %>% group_by(row, col) %>%
    summarize(count_recycled = sum(recycled), 
              count_retouched = sum(stage > 0, na.rm = T)) %>%
    mutate(!!parameters[1] := c(exp_values[1, c(parameters[1])]), 
           !!parameters[2] := c(exp_values[1, c(parameters[2])]), 
           !!parameters[3] := c(exp_values[1, c(parameters[3])]), 
           !!parameters[4] := c(exp_values[1, c(parameters[4])]), 
           !!parameters[5] := c(exp_values[1, c(parameters[5])]), 
           !!parameters[6] := c(exp_values[1, c(parameters[6])]), 
           !!parameters[7] := c(exp_values[1, c(parameters[7])]), 
           !!parameters[8] := c(exp_values[1, c(parameters[8])]), 
           !!parameters[9] := c(exp_values[1, c(parameters[9])]), 
           !!parameters[10] := c(exp_values[1, c(parameters[10])]), 
           !!parameters[11] := c(exp_values[1, c(parameters[11])]), 
           !!parameters[12] := c(exp_values[1, c(parameters[12])]))
  
  #colnames(onerun.grid) = c("row", "col", "count_recycled", "count_retouched", parameters)
  
  filename = str_split(files[f], "_")[[1]][1]
  write_csv(onerun.grid, file = paste0("/scratch/ec3307/recycling-Java/output/artifact-data/output/", filename, "_ONERUN_recycled-retouched-object-counts.csv"), num_threads=1)
  
}