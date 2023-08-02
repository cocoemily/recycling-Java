## ANALYSIS OF ASSOCIATION BETWEEN OUTPUT VARIABLES WITHIN LAYER GRID SQUARES
library(readr)
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
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1, show_col_types = F)
  
  dirsplit = str_split(dirs[d], "\\/")[[1]]
  expnum = str_extract(dirsplit[length(dirsplit)], "[0-9]+")
  exp_values = data[1, parameters]

  grid = expand.grid(0:9, 0:9)
  colnames(grid) = c("row", "col")
  
  cor_vals = data.frame(
    cr.obj.cnt.cor = NA, 
    ri.obj.cnt.cor = NA, 
    ri.cr.cor = NA, 
    ri.num.disc.cor = NA, 
    ri.num.scvg.cor = NA, 
    ri.num.enct.cor = NA,
    ri.num.ret.cor = NA
  )
  cor_outputs = colnames(cor_vals)
  
  end_grid = cbind(grid, rep(exp_values), rep(cor_vals))
  
  for(i in 1:nrow(end_grid)){
    square_data = data[which(data$row == grid$row[i] & data$col == grid$col[i]),] 
    square_data$obj.cnt = square_data$nodule.count + square_data$flake.count
    
    cr.obj.cnt = NA
    ri.obj.cnt = NA
    ri.cr = NA
    ri.disc = NA
    ri.scavenge = NA
    ri.enct = NA
    ri.ret = NA
    
    if(sd(square_data$recycling.intensity, na.rm = T) != 0) {
      if(sd(square_data$obj.cnt, na.rm = T) != 0) {
        ri.obj.cnt = cor(square_data$recycling.intensity, square_data$obj.cnt, use = "complete.obs", method = "spearman")
      }
      if(sd(square_data$cortex.ratio, na.rm = T) != 0) {
        ri.cr = cor(square_data$recycling.intensity, square_data$cortex.ratio, use = "complete.obs", method = "spearman")
      }
      if(sd(square_data$num.discards, na.rm = T) != 0) {
        ri.disc = cor(square_data$recycling.intensity, square_data$num.discards, use = "complete.obs", method = "spearman")
      }
      if(sd(square_data$num.scavenge, na.rm = T) != 0) {
        ri.scavenge = cor(square_data$recycling.intensity, square_data$num.scavenge, use = "complete.obs", method = "spearman")
      }
      if(sd(square_data$num.encounters, na.rm = T) != 0) {
        ri.enct =  cor(square_data$recycling.intensity, square_data$num.encounters, use = "complete.obs", method = "spearman")
      }
      if(sd(square_data$num.retouch, na.rm = T) != 0) {
        ri.ret =  cor(square_data$recycling.intensity, square_data$num.retouch, use = "complete.obs", method = "spearman")
      }
    }
    
    if(sd(square_data$obj.cnt, na.rm = T) != 0 && sd(square_data$cortex.ratio, na.rm = T) != 0) {
      cr.obj.cnt = cor(square_data$cortex.ratio, square_data$obj.cnt, use = "complete.obs", method = "spearman")
    }
    
    end_grid[i, cor_outputs] = c(
      cr.obj.cnt,
      ri.obj.cnt,
      ri.cr,
      ri.disc,
      ri.scavenge,
      ri.enct,
      ri.ret
    )
  }
  
  
  #filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  write_csv(end_grid, file = paste0("/scratch/ec3307/recycling-Java/output/layer-output/exp", expnum, "_layer-gridded-cor.csv"), num_threads=1)
  
}
