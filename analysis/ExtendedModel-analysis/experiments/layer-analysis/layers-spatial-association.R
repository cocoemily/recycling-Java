#SPATIAL ASSOCIATIONS FROM LAYERS
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(sp)
library(sf)
library(rgdal)
library(tmap)
library(spdep)
library(cowplot)

param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

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
  data = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1)
  #print(dirs[d])
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  years = unique(data$model_year)
  #plist = list()
  glist = list()
  for(y in 2:length(years)) {
    grid = data[which(data$model_year == years[y]),]
    grid = grid[1:100,]
    
    grid.spat = grid
    coordinates(grid.spat) = ~col+row
    gridded(grid.spat) = TRUE
    grid.spat = as(grid.spat, "SpatialPolygonsDataFrame") 
    
    grid.spat$recycling.intensity2 = ifelse(is.nan(grid.spat$recycling.intensity), 0, grid.spat$recycling.intensity)
    grid.spat$cortex.ratio2 = ifelse(is.nan(grid.spat$cortex.ratio), 0, grid.spat$cortex.ratio)
    grid.spat$row = grid$row
    grid.spat$col = grid$col
    
    nb = poly2nb(grid.spat, queen = T)
    lw = nb2listw(nb, zero.policy = T)

    grid.spat$Gi.stat.RI = localG_perm(grid.spat$recycling.intensity2, lw, nsim = 100, zero.policy = T)
    grid.spat$Gi.stat.CR = localG_perm(grid.spat$recycling.intensity2, lw, nsim = 100, zero.policy = T)
    grid.spat$Gi.stat.flk.count = localG_perm(grid.spat$flake.count, lw, nsim = 100, zero.policy = T)
    grid.spat$Gi.stat.nod.count = localG_perm(grid.spat$nodule.count, lw, nsim = 100, zero.policy = T)
    grid.spat$Gi.stat.num.discard = localG_perm(grid.spat$num.discards, lw, nsim = 100, zero.policy = T)
    grid.spat$Gi.stat.num.scavenge = localG_perm(grid.spat$num.scavenge, lw, nsim = 100, zero.policy = T)
    grid.spat$Gi.stat.num.encounters = localG_perm(grid.spat$num.encounters, lw, nsim = 100, zero.policy = T)
    grid.spat$Gi.stat.num.retouch = localG_perm(grid.spat$num.retouch, lw, nsim = 100, zero.policy = T)
    
    glist[[y]] = grid.spat
    
  }
  
  localGresults = do.call("rbind", glist[2:length(glist)])
  localGresults$exp = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]

  localG.df = as.data.frame(localGresults)
  
  print("writing local Gi results")
  readr::write_csv(localG.df, paste0("/scratch/ec3307/recycling-Java/output/layer-output/", filename, "_layer-local-G.csv"), num_threads=1)
  
  #### CHANGE IN VALUES FROM START TO MID AND FROM MID TO FINISH ####
  startgrid = glist[[2]]
  midgrid = glist[[151]]
  endgrid = glist[[301]]
  
  #head(midgrid)
  #head(endgrid)

  midgrid$change.RI = midgrid$recycling.intensity2 - startgrid$recycling.intensity2
  midgrid$change.CR = midgrid$cortex.ratio2 - startgrid$cortex.ratio2
  midgrid$change.nod.cnt = midgrid$nodule.count - startgrid$nodule.count
  midgrid$change.flk.cnt = midgrid$flake.count - startgrid$flake.count
  midgrid$change.num.discards = midgrid$num.discards - startgrid$num.discards
  midgrid$change.num.scavenge = midgrid$num.scavenge - startgrid$num.scavenge
  midgrid$change.num.encounter = midgrid$num.encounters - startgrid$num.encounters
  midgrid$change.num.retouch = midgrid$num.retouch - startgrid$num.retouch
  
  endgrid$change.RI = endgrid$recycling.intensity2 - midgrid$recycling.intensity2
  endgrid$change.CR = endgrid$cortex.ratio2 - midgrid$cortex.ratio2
  endgrid$change.nod.cnt = endgrid$nodule.count - midgrid$nodule.count
  endgrid$change.flk.cnt = endgrid$flake.count - midgrid$flake.count
  endgrid$change.num.discards = endgrid$num.discards - midgrid$num.discards
  endgrid$change.num.scavenge = endgrid$num.scavenge - midgrid$num.scavenge
  endgrid$change.num.encounter = endgrid$num.encounters - midgrid$num.encounters
  endgrid$change.num.retouch = endgrid$num.retouch - midgrid$num.retouch
 
  endgrid$timeperiod = "endgrid"
  midgrid$timeperiod = "midgrid"
  
  results = rbind(midgrid, endgrid)
  results$exp = filename
  
  print("writing change results")
  readr::write_csv(results, paste0("/scratch/ec3307/recycling-Java/output/layer-output/", filename, "_layer-spatial-change.csv"), num_threads=1)
}
