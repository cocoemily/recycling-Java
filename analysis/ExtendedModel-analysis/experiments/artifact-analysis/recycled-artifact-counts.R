#Counts of recycled nodules and flakes + retouched flakes
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
  
  mid_data = data[which(data$model_year == 350000), ]
  end_data = data[which(data$model_year == 200000), ]
  
  runs = unique(data$run)
  run.list = list()
  for(i in runs) {
    
    gridded.end = end_data %>% filter(run == i) %>%
      group_by(row, col, obj_type) %>%
      summarize(total_count = n(), 
                count_recycled = sum(recycled), 
                count_retouched = sum(stage > 0, na.rm = T))
    
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
    gridded.end$time = "end"
    
    gridded.mid = mid_data %>% filter(run == i) %>%
      group_by(row, col, obj_type) %>%
      summarize(total_count = n(), 
                count_recycled = sum(recycled), 
                count_retouched = sum(stage > 0, na.rm = T))
    
    gridded.mid[,parameters[1]] = c(exp_values[1, c(parameters[1])])
    gridded.mid[,parameters[2]] = c(exp_values[1, c(parameters[2])])
    gridded.mid[,parameters[3]] = c(exp_values[1, c(parameters[3])])
    gridded.mid[,parameters[4]] = c(exp_values[1, c(parameters[4])])
    gridded.mid[,parameters[5]] = c(exp_values[1, c(parameters[5])])
    gridded.mid[,parameters[6]] = c(exp_values[1, c(parameters[6])])
    gridded.mid[,parameters[7]] = c(exp_values[1, c(parameters[7])])
    gridded.mid[,parameters[8]] = c(exp_values[1, c(parameters[8])])
    gridded.mid[,parameters[9]] = c(exp_values[1, c(parameters[9])])
    gridded.mid[,parameters[10]] = c(exp_values[1, c(parameters[10])])
    gridded.mid[,parameters[11]] = c(exp_values[1, c(parameters[11])])
    gridded.mid[,parameters[12]] = c(exp_values[1, c(parameters[12])])
    gridded.mid$run = i
    gridded.mid$time = "middle"
    
    run.list[[length(run.list) + 1]] <- gridded.mid
    run.list[[length(run.list) + 1]] <- gridded.end
  }
  
  
  allresults = do.call("rbind", run.list[1:length(run.list)])
  
  filename = str_split(files[f], "_")[[1]][1]
  write_csv(allresults, file = paste0("/scratch/ec3307/recycling-Java/output/artifact-data/output/", filename, "_recycled-object-counts.csv"), num_threads=1)
  
}