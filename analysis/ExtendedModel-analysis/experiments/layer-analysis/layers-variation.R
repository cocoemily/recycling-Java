library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

#dirs = list.dirs("../output/test-data/")
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
  
  #analysis of variation across all grid squares within each run
  glist = list()
  
  for(run in unique(data$run)) {
    rundata = data[which(data$run == run),]
    
    dirsplit = str_split(dirs[d], "\\/")[[1]]
    expnum = str_extract(dirsplit[length(dirsplit)], "[0-9]+")
    exp_values = data[1,c(parameters)]
    
    variation_vals = data.frame(
      nodule.count.cv = NA, 
      flake.count.cv = NA,
      cortex.ratio.cv = NA,
      recycling.intensity.cv = NA,
      num.discards.cv = NA,
      num.scavenge.cv = NA,
      num.encounters.cv = NA, 
      num.retouch.cv = NA
    )
    var_outputs = colnames(variation_vals)
    
    grid = cbind(exp_values, variation_vals)
    grid$exp = expnum
    grid$run = run
    
    grid[1, var_outputs] = c(
      sd(rundata$nodule.count, na.rm = T)/mean(rundata$nodule.count, na.rm = T), 
      sd(rundata$flake.count, na.rm = T)/mean(rundata$flake.count, na.rm = T), 
      sd(rundata$cortex.ratio, na.rm = T)/mean(rundata$cortex.ratio, na.rm = T), 
      sd(rundata$recycling.intensity, na.rm = T)/mean(rundata$recycling.intensity, na.rm = T), 
      sd(rundata$num.discards, na.rm = T)/mean(rundata$num.discards, na.rm = T), 
      sd(rundata$num.scavenge, na.rm = T)/mean(rundata$num.scavenge, na.rm = T), 
      sd(rundata$num.encounters, na.rm = T)/mean(rundata$num.encounters, na.rm = T), 
      sd(rundata$num.retouch, na.rm = T)/mean(rundata$num.retouch, na.rm = T)
    )
    
    glist[[length(glist)+1]] = grid
  }
  
  COVresults = do.call("rbind", glist[1:length(glist)])
  
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  write_csv(COVresults, file = paste0("/scratch/ec3307/recycling-Java/output/layer-output/", filename, "_layer-COV.csv"), num_threads=1)
}
