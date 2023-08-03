#Recycling Intensity Confidence Intervals
library(tidyverse)
library(ggthemes)
library(parallel)
library(foreach)
library(doParallel)

theme_set(theme_bw())

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "mu", "overlap", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

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
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1)
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  results = data[1, c("run", parameters)]
  results$mean = 0
  results$median = 0
  results$lower = 0
  results$upper = 0
  results = results[0,]
  
  for(run in unique(data$run)) {
    rundata = data[which(data$run == run),]
    
    avg = mean(rundata$recycling.intensity, na.rm = T)
    med = median(rundata$recycling.intensity, na.rm = T)
    
    n = length(rundata$recycling.intensity)
    error = qt(0.975,df=n-1) * sd(rundata$recycling.intensity, na.rm = T)/sqrt(n)
    lower = avg - error
    upper = avg + error
    
    results[nrow(results) + 1,] = c(run, rundata[1, c(parameters)], avg, med, lower, upper)
  }
  
  write_csv(results, file = paste0("/scratch/ec3307/updated-recycling-Java/recycling-Java/output/layer-output/", filename, "_RI-CI-results.csv"), num_threads=1)
  
}
