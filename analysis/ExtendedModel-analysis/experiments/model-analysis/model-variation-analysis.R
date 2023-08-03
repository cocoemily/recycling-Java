#analysis of variation among model runs
#calculation of coefficient of variation for every output parameter
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

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
  data = readr::read_csv(paste0(dirs[d], "/model-data.csv"))
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  outputs = colnames(data[,22:34])
  output.names = sapply(outputs, FUN = function(x) paste0("COV.", x))
  
  allCV = data[1,c(parameters, outputs, "model_year")]
  allCV = allCV[0,]
  colnames(allCV) = c(parameters, output.names, "model_year")
  
  modelyears = unique(data$model_year)
  
  for(year in 2:length(modelyears)) {
    oneyear = data[which(data$model_year == modelyears[year]),]
    params = c(oneyear[1, parameters])
    CVs = sapply(oneyear[,outputs], FUN=function(x) sd(x, na.rm = T) / mean(x, na.rm = T))
    names(CVs) = c(output.names)
    
    CV = c(
      params,
      CVs, 
      modelyears[year]
    )
    
    allCV[nrow(allCV) + 1, ] = CV
  }
  
  readr::write_csv(allvar, file = paste0("/scratch/ec3307/updated-recycling-Java/recycling-Java/output/model-output/", filename, "_COV.csv"))
  
}
