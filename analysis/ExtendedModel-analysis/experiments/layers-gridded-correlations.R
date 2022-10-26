## ANALYSIS OF ASSOCIATION BETWEEN OUTPUT VARIABLES WITHIN LAYER GRID SQUARES
library(readr)
library(parallel)
library(foreach)
library(doParallel)

param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

colnames(param_list) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")
param_list = param_list[, c("exp", parameters)]

#dirs = list.dirs("../output/test-layer-data/")
dirs = list.dirs("/scratch/ec3307/recycling-Java/output/")
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
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1)
  print(dirs[d])
  dirsplit = str_split(dirs[d], "\\/")[[1]]
  expnum = str_extract(dirsplit[length(dirsplit)], "[0-9]+")
  exp_values = param_list[which(param_list$exp == as.numeric(expnum)), ]
  print(expnum)
  print(exp_values)
  end_data = data[which(data$model_year == 200000), ]
  mid_data = data[which(data$model_year == 350000), ]
  
  grid = expand.grid(0:9, 0:9)
  colnames(grid) = c("row", "col")
  
  cor_vals = data.frame(
    cr.obj.cnt.cor = NA, 
    ri.obj.cnt.cor = NA, 
    ri.cr.cor = NA, 
    ri.num.disc.cor = NA, 
    ri.num.scvg.cor = NA, 
    ri.num.enct.cor = NA, 
    ri.num.manu.cor = NA, 
    ri.num.ret.cor = NA, 
    ri.num.occp.cor = NA
  )
  cor_outputs = colnames(cor_vals)
  
  end_grid = cbind(grid, rep(exp_values), rep(cor_vals))
  
  for(i in 1:nrow(end_grid)){
    square_data = end_data[which(end_data$row == grid$row[i] & end_data$col == grid$col[i]),] 
    square_data$obj.cnt = square_data$nodule.count + square_data$flake.count
    
    end_grid[i, cor_outputs] = c(
      cor(square_data$cortex.ratio, square_data$obj.cnt, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$obj.cnt, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$cortex.ratio, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$num.discards, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$num.scavenge, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$num.encounters, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$num.manufacture, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$num.retouch, use = "complete.obs", method = "spearman"), 
      cor(square_data$recycling.intensity, square_data$num.occupation, use = "complete.obs", method = "spearman")
    )
  }
    
    mid_grid = cbind(grid, rep(exp_values), rep(cor_vals))
    
    for(i in 1:nrow(mid_grid)){
      square_data = mid_data[which(mid_data$row == grid$row[i] & mid_data$col == grid$col[i]),] 
      square_data$obj.cnt = square_data$nodule.count + square_data$flake.count
      
      mid_grid[i, cor_outputs] = c(
        cor(square_data$cortex.ratio, square_data$obj.cnt, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$obj.cnt, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$cortex.ratio, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$num.discards, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$num.scavenge, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$num.encounters, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$num.manufacture, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$num.retouch, use = "complete.obs", method = "spearman"), 
        cor(square_data$recycling.intensity, square_data$num.occupation, use = "complete.obs", method = "spearman")
      )
    
    }
    mid_grid$time = "mid"
    end_grid$time = "end"
    
    results = rbind(mid_grid, end_grid)
    
    #filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
    write_csv(results, file = paste0("/scratch/ec3307/recycling-Java/output/layer-output/exp", expnum, "_layer-gridded-cor.csv"), num_threads=1)
    
}
