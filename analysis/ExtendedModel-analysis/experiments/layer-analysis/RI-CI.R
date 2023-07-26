#Recycling Intensity Confidence Intervals
library(tidyverse)
library(ggthemes)
library(parallel)
library(foreach)
library(doParallel)

theme_set(theme_bw())

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "mu", "overlap", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

#dirs = list.dirs("../output/test-layer-data")
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
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  results = data[1,c(parameters)]
  
  end_data = data[which(data$model_year == 200000), ]
  end_data = end_data[which(!is.na(end_data$recycling.intensity)),]
  mid_data = data[which(data$model_year == 350000), ]
  mid_data = mid_data[which(!is.na(mid_data$recycling.intensity)),]
  
  avg = mean(end_data$recycling.intensity, na.rm = T)
  med = median(end_data$recycling.intensity, na.rm = T)
  
  error = qt(0.975,df=length(end_data$recycling.intensity)-1)*sd(end_data$recycling.intensity)/sqrt(length(end_data$recycling.intensity))
  left = avg - error
  right = avg + error
  
  results$end_mean = avg
  results$end_media = med
  results$end_left = left
  results$end_right = right
  
  avg = mean(mid_data$recycling.intensity, na.rm = T)
  med = median(mid_data$recycling.intensity, na.rm = T)
  
  error = qt(0.975,df=length(mid_data$recycling.intensity)-1)*sd(mid_data$recycling.intensity)/sqrt(length(mid_data$recycling.intensity))
  left = avg - error
  right = avg + error
  
  results$mid_mean = avg
  results$mid_media = med
  results$mid_left = left
  results$mid_right = right
  
  write_csv(results, file = paste0("/scratch/ec3307/recycling-Java/output/layer-output/", filename, "_RI-CI-results.csv"), num_threads=1)
  
}
