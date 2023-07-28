#Retouch intensity analysis
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(MASS)

#param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

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
  
  end = data[which(data$model_year == 200000),]
  
  output = end[,c("run", "row", "col")]
  output = cbind(output, exp_values)
  output$W_shape = 0
  output$W_scale = 0 
  output = output[0,]
  
  for(run in unique(end$run)) {
    onerun = end[which(end$run == run),]
    
    for(i in unique(onerun$row)) {
      for(j in unique(onerun$col)) {
        onesquare = onerun[which(onerun$row == i & onerun$col == j & onerun$obj_type == "flake"),]
        
        if(nrow(onesquare) > 1) {
          artifact.stages = onesquare$stage
          
          #need to think about this and how to best model size of artifacts
          artifact.sizes = 3000 - onesquare$stage
          
          if(min(artifact.stages) != max(artifact.stages)) {
            
            #hist(artifact.sizes)
            tryCatch({
              dist = fitdistr(artifact.sizes, densfun = "weibull", method = "Nelder-Mead")
              wdist = dist$estimate
              
              output[nrow(output) + 1,] <- c(
                onesquare[1, "run"], 
                onesquare[1, "row"], 
                onesquare[1, "col"], 
                exp_values,
                wdist["shape"], 
                wdist["scale"]
              )
            }, error=function(e){cat("ERROR :",conditionMessage(e), "for", run, "at (", i, ",", j, ")" , "\n")})
          }
        }
        
      }
    }
  }
  
  filename = str_split(files[f], "_")[[1]][1]
  write_csv(output, file = paste0("/scratch/ec3307/recycling-Java/output/artifact-data/output/", filename, "_weibull-dist.csv"), num_threads=1)
  
}
