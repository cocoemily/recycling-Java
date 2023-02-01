#CORTEX RATIOS BY GRID SQUARE
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(sp)
library(rgdal)
library(tmap)
library(spdep)
library(cowplot)

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
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1)
  #print(dirs[d])
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  end_data = data[which(data$model_year == 200000), c(parameters, "model_year", "row", "col", "cortex.ratio", "flake.count", "nodule.count")]
  end_data = end_data[1:5000,]
  mid_data = data[which(data$model_year == 350000), c(parameters, "model_year", "row", "col", "cortex.ratio", "flake.count", "nodule.count")]
  
  end_data$run = rep(seq(1,50, by=1), each=100)
  mid_data$run = rep(seq(1,50, by=1), each=100)
  
  sub_end = end_data[which(end_data$run <= 5), ]
  sub_mid = mid_data[which(mid_data$run <= 5), ]
  
  ad = rbind(sub_mid, sub_end)

  readr::write_csv(ad, paste0("/scratch/ec3307/recycling-Java/output/layer-output/", filename, "_gridded-CR.csv"), num_threads=1)
}
