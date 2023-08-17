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

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "num_agents", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

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
  ldata = read_csv(paste0(dirs[d], "/layers-data.csv"), num_threads=1)
  #print(dirs[d])
  
  filename = str_split(dirs[d], "/")[[1]][length(str_split(dirs[d], "/")[[1]])]
  print(filename)
  
  #adata = read_csv(paste0("/scratch/ec3307/recycling-Java/output/artifact-data/", filename, "_artifacts-data.csv"), num_threads=1)
  adata = read_csv(paste0(dirs[d], "/artifacts-data.csv"))
  
  allgrids = ldata[,c("run", parameters, "model_year", "row", "col", "recycling.intensity", "cortex.ratio", "flake.count", "nodule.count", "num.discards", "num.scavenge", "num.encounters", "num.retouch")]
  
  expnum = str_extract(filename[length(filename)], "[0-9]+")
  exp_values = ldata[1, parameters]
  
  grid = expand.grid(0:9, 0:9)
  colnames(grid) = c("row", "col")
  
  cor_vals = data.frame(
    ##other correlations
    ret.prop.num.enct.cor = NA
  )
  cor_outputs = colnames(cor_vals)
  
  end_grid = cbind(grid, rep(exp_values), rep(cor_vals))
  
  for(i in 1:nrow(end_grid)){
    artifacts = adata[which(adata$row == end_grid$row[i] & adata$col == end_grid$col[i]),] %>%
      mutate(retouched = ifelse(stage > 0, TRUE, FALSE)) %>%
      group_by(run, row, col) %>%
      summarize(total_count = n(),
                count_recycled = sum(recycled), 
                count_retouched = sum(retouched, na.rm = T), 
                recycle_prop = count_recycled/total_count, 
                retouch_prop = count_retouched/total_count)
    
    
    square_data = ldata[which(ldata$row == grid$row[i] & ldata$col == grid$col[i]),] 
    sqdata2 = square_data %>% left_join(artifacts, by = c("row", "col", "run"))
    
    rp.ne = NA

    if(sd(sqdata2$retouch_prop, na.rm = T) != 0 && sd(sqdata2$num.encounters, na.rm =T) != 0) {
      rp.ne = cor(sqdata2$retouch_prop, sqdata2$num.encounters, use = "complete.obs", method = "spearman")
    }
    
    end_grid[i, cor_outputs] = c(
      rp.ne
    )
  }
  
  write_csv(end_grid, file = paste0("/scratch/ec3307/updated-recycling-Java/recycling-Java/output/layer-output/exp", expnum, "_layer-ret-enct-gridded-cor.csv"), num_threads=1)
}
