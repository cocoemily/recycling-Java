#testing artifact exposure time for recycled and non-recycled artifacs
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(moments)

#param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
#param_list = read_csv("/scratch/ec3307/updated-recycling-Java/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
#colnames(param_list) = c("exp", "run", "size", "start_year", "timestep", "max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size",  "strict_selection", "erosion_ratio", "geo_freq", "total_steps", "num_agents")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size",  "strict_selection")

#param_list = param_list[, c("exp", parameters)]

#dirs = list.dirs("../output/test-data")
dirs = list.dirs("/scratch/ec3307/updated-recycling-Java/recycling-Java/output")
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
  if(file.exists(paste0(dirs[d], "/artifacts-data.csv"))) {
    data = read_csv(paste0(dirs[d], "/artifacts-data.csv"), num_threads=1, col_types = cols())
    
    expnum = str_extract(dirs[d], "[0-9]+")
    filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
    print(filename)
    
    #exp_values = param_list[which(param_list$exp == as.numeric(expnum)), ]
    
    if(nrow(data) > 0) {
      #statistical differences between initial discard of recycled and non-recycled objects
      rcycl.end = data[which(data$recycled == T), ]
      nrcycl.end = data[which(data$recycled == F), ]
      
      exposure_results = data.frame(exp = NA, run = NA, recycled.signif.greater = NA)
      exposure_results = exposure_results[c(-1),]
      
      if(nrow(rcycl.end) != 0 && nrow(nrcycl.end) != 0) {
        for(r in unique(data$run)) {
          end.conf.val = NA
          rcycl.end.run = rcycl.end[which(rcycl.end$run == r),]
          nrcycl.end.run = nrcycl.end[which(nrcycl.end$run == r),]
          
          if(nrow(rcycl.end.run) != 0 && nrow(nrcycl.end.run) != 0) {
            endresults = wilcox.test(rcycl.end.run$initial_discard,
                                     nrcycl.end.run$initial_discard,
                                     alternative = "greater")
            
            end.conf.val = ifelse(endresults$p.value < 0.05, TRUE, FALSE)
            
            
          }
          #print(paste0(r, " output is ", end.conf.val))
          exposure_results[nrow(exposure_results) + 1, ] = c(filename, r, end.conf.val)
        }
      }
      
    }
  }
  #write_csv(exposure_results, file = paste0(filename, "_exposure-results.csv"), num_threads=1)
  write_csv(exposure_results, file = paste0("/scratch/ec3307/updated-recycling-Java/recycling-Java/output/artifact-output/", filename, "_exposure-results.csv"), num_threads=1)
}

