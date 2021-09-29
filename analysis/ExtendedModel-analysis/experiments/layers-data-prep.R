library(tidyverse)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size",
               "max_nodules_size", "blank_prob", "scavenge_prob", "overlap",
               "mu", "size_preference", "flake_preference",
               "min_suitable_flake_size", "strict_selection")

df = data.frame(exp = numeric(),
                max_use_intensity = numeric(),
                max_artifact_carry = numeric(),
                max_flake_size = numeric(),
                max_nodules_size = numeric(),
                blank_prob = numeric(),
                scavenge_prob = numeric(),
                overlap = numeric(),
                mu = numeric(),
                size_preference = character(),
                flake_preference = character(),
                min_suitable_flake_size = numeric(),
                strict_selection = character(),
                ri.cr.cor = numeric(), 
                ri.nc.cor = numeric(),
                ri.fc.cor = numeric(),
                ri.nd.cor = numeric(),
                ri.ns.cor = numeric(),
                ri.ne.cor = numeric(),
                ri.nm.cor = numeric(),
                ri.nr.cor = numeric(),
                ri.no.cor = numeric(),
                ri.av.cor = numeric()
)

#for testing
for(i in 1:3) {
  
  data = as.data.frame(fread(paste0("/Users/emilycoco/eclipse-workspace/recycling-Java/output/exp", i, "/run0_layers-data.csv")))


#for(i in 1:8640) {
  
  #data = as.data.frame(fread(paste0("output/exp", i, "/layers-data.csv")))
  
  
  
  ri.cr.cor = cor(data$recycling.intensity, data$cortex.ratio, use ="complete.obs")
  ri.nc.cor = cor(data$recycling.intensity, data$nodule.count, use ="complete.obs")
  ri.fc.cor = cor(data$recycling.intensity, data$flake.count, use ="complete.obs")
  ri.nd.cor = cor(data$recycling.intensity, data$num.discards, use ="complete.obs")
  ri.ns.cor = cor(data$recycling.intensity, data$num.scavenge, use ="complete.obs")
  ri.ne.cor = cor(data$recycling.intensity, data$num.encounters, use ="complete.obs")
  ri.nm.cor = cor(data$recycling.intensity, data$num.manufacture, use ="complete.obs")
  ri.nr.cor = cor(data$recycling.intensity, data$num.retouch, use ="complete.obs")
  ri.no.cor = cor(data$recycling.intensity, data$num.retouch, use ="complete.obs")
  ri.av.cor = cor(data$recycling.intensity, data$assemblage.vol, use ="complete.obs")
  
  newrow = c(i, unlist(data[1, parameters]), ri.cr.cor, ri.nc.cor, ri.fc.cor, 
             ri.nd.cor, ri.ns.cor, ri.ne.cor, ri.nm.cor, ri.nr.cor, ri.no.cor, 
             ri.av.cor )
  
  df[nrow(df) + 1, ] = newrow
}



