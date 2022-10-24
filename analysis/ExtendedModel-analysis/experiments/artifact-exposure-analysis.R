#testing artifact exposure time for recycled and non-recycled artifacs

library(tidyverse)
# library(fitdistrplus)
# library(rcompanion)
library(parallel)
library(foreach)
library(doParallel)

#param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

colnames(param_list) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")
param_list = param_list[, c("exp", parameters)]


#files = list.files("../output/test-artifact-data/")
files = list.files("/scratch/ec3307/recycling-Java/output/artifact-data/")


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
  
  #data = read_csv(paste0("../output/test-artifact-data/", files[f]))
  data = read_csv(paste0("/scratch/ec3307/recycling-Java/output/artifact-data/", files[f]), num_threads=1)
  
  exp_values = param_list[which(param_list$exp == as.numeric(expnum)), ]
  
  #unique(data$model_year) ## artifact data collected at middle and end of model run
  
  mid_data = data[which(data$model_year == 350000), ]
  end_data = data[which(data$model_year == 200000), ]

  #statistical differences between initial discard of recycled and non-recycled objects
  rcycl.mid = mid_data[which(mid_data$recycled == T), ]
  nrcycl.mid = mid_data[which(mid_data$recycled == F), ]
  
  mid.conf.val = NULL
  
  if(nrow(rcycl.mid) != 0 && nrow(nrcycl.mid) != 0) {
    midresults = wilcox.test(rcycl.mid$initial_discard,
                             nrcycl.mid$initial_discard,
                             alternative = "greater")
    
    print(midresults$p.value)
    mid.conf.val = ifelse(midresults$p.value < 0.05, TRUE, FALSE)
  }
  
  rcycl.end = end_data[which(end_data$recycled == T), ]
  nrcycl.end = end_data[which(end_data$recycled == F), ]
  
  end.conf.val = NULL
  
  if(nrow(rcycl.end) != 0 && nrow(nrcycl.mid) != 0) {
    endresults = wilcox.test(rcycl.end$initial_discard,
                             nrcycl.end$initial_discard,
                             alternative = "greater")
    
    print(endresults$p.value)
    end.conf.val = ifelse(endresults$p.value < 0.05, TRUE, FALSE)
    
  }
  
  exposure_results = data.frame(exp_values[1,], mid.conf.val, end.conf.val)
  filename = str_split(files[f], "_")[[1]][1]
  write_csv(exposure_results, file = paste0("/scratch/ec3307/recycling-Java/output/artifact-data/output/", filename, "_results.csv"), num_threads=1)
  
}

