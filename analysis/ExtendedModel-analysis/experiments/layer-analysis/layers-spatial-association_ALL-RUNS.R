#SPATIAL ASSOCIATIONS FROM LAYERS -- all runs
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
  #print(dirs[d])
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  years = unique(data$model_year)
  #plist = list()
  glist = list()
  for(y in 2:length(years)) {
    allgrids = data[which(data$model_year == years[y]), 
                    c(parameters, "model_year", "row", "col", "recycling.intensity", "cortex.ratio", "flake.count", "nodule.count", "num.discards", "num.scavenge", "num.encounters", "num.retouch")]
    if(years[y] == 200000) {
      allgrids = allgrids[1:5000,]
    }
    
    allgrids$run = rep(seq(1,50, by=1), each=100)
    
    for(i in 1:5) {
      grid = allgrids[which(allgrids$run == i),]
      
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
      
      glist[[length(glist)+1]] = grid.spat
    }
    
  }
  
  localGresults = do.call("rbind", glist[1:length(glist)])
  localGresults$exp = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  
  localG.df = as.data.frame(localGresults)
  
  print("writing local Gi results")
  readr::write_csv(localG.df, paste0("/scratch/ec3307/recycling-Java/output/layer-output/", filename, "_all-runs-layer-local-G.csv"), num_threads=1)
}
