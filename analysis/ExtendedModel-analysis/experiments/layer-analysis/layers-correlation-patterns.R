## ANALYSIS OF ASSOCIATION BETWEEN OUTPUT VARIABLES WITHIN FINAL GRID SQUARES
library(readr)
library(tidyverse)
#library(rcompanion)
#library(fitdistrplus)
library(Rfit)

library(parallel)
library(foreach)
library(doParallel)

param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

colnames(param_list) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")
param_list = param_list[, c("exp", parameters)]

#dirs = list.dirs("../output/test-layer-data")
dirs = list.dirs("/scratch/ec3307/recycling-Java/output")
##remove folders refering to artifact data
dirs = dirs[-c(1:3)]
dirs = dirs[-length(dirs)]

if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}
print(ncores)
registerDoParallel(ncores)
Sys.setenv(OMP_NUM_THREADS = "1")

foreach (d=1:length(dirs)) %dopar% { 
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1, show_col_types = F)
  
  dirsplit = str_split(dirs[d], "\\/")[[1]]
  #print(dirsplit)
  expnum = str_extract(dirsplit[length(dirsplit)], "[0-9]+")
  exp_values = param_list[which(param_list$exp == as.numeric(expnum)), ]
  #print(expnum)
  print(exp_values)
  
  start_data = data[which(data$model_year == 500000), ]
  end_data = data[which(data$model_year == 200000), ] #duplicated data? possibly due to outputting at the end of model run?
  mid_data = data[which(data$model_year == 350000), ]
  #rm(data)
 
  grid = expand.grid(0:9, 0:9)
  colnames(grid) = c("row", "col")
  
  cor_vals = data.frame(
    ri.coef = NA,
    ri.se = NA,
    cr.coef = NA, 
    cr.se = NA,
    num.disc.coef = NA, 
    num.disc.se = NA, 
    num.scvg.coef = NA, 
    num.scvg.se = NA, 
    num.enct.coef = NA, 
    num.enct.se = NA, 
    num.manu.coef = NA,
    num.manu.se = NA,
    num.ret.coef = NA,
    num.ret.se = NA,
    num.occp.coef = NA, 
    num.occp.se = NA, 
    obj.cnt.coef = NA, 
    obj.cnt.se = NA,
    adj.rsq = NA
  )
  cor_outputs = colnames(cor_vals)
  
  end_grid = cbind(grid, rep(exp_values), rep(cor_vals))
  
  for(i in 1:nrow(end_grid)){
    square_data = end_data[which(end_data$row == grid$row[i] & end_data$col == grid$col[i]),]
    square_data = square_data[seq(1, nrow(square_data), 2), ] #get rid of end data duplicates
    square_data$obj.cnt = square_data$nodule.count + square_data$flake.count
    
    fit1 = lm(recycling.intensity ~ ., data = square_data[,28:36])
    coefs = coefficients(fit1)
    se = sqrt(diag(vcov(fit1)))
    adj.rsq = summary(fit1)$adj.r.squared
    
    end_grid[i, cor_outputs] = c(
      as.numeric(coefs[1]), 
      as.numeric(se[1]), 
      as.numeric(coefs[2]), 
      as.numeric(se[2]),
      as.numeric(coefs[3]), 
      as.numeric(se[3]),
      as.numeric(coefs[4]), 
      as.numeric(se[4]),
      as.numeric(coefs[5]), 
      as.numeric(se[5]),
      as.numeric(coefs[6]), 
      as.numeric(se[6]),
      as.numeric(coefs[7]), 
      as.numeric(se[7]),
      as.numeric(coefs[8]), 
      as.numeric(se[8]),
      as.numeric(coefs[9]), 
      as.numeric(se[9]),
      adj.rsq
    )
    
    end_grid[i, 26:27] = c(square_data$row[1], square_data$col[1])
  }
  
    filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
    write_csv(results, file = paste0("/scratch/ec3307/recycling-Java/output/layer-output/exp", expnum, "_layer-multi-cor-results.csv"), num_threads=1)
    
}
