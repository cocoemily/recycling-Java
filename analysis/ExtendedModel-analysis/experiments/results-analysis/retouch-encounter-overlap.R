##retouch and encounters overlap
library(tidyverse)
library(rcompanion)
library(raster)
library(ggpubr)

theme_set(theme_bw())

#layers.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/sub-local-G.csv")
layers.Gi = read_csv("/scratch/ec3307/updated-recycling-Java/recycling-Java/results/all-local-G-results.csv")
parameters = colnames(layers.Gi[c(2:14)])

retouch.Gi = layers.Gi[,c(parameters, "run", "row", "col", "Gi.stat.retouch.prop")]
encounter.Gi = layers.Gi[,c(parameters, "run", "row", "col", "Gi.stat.num.encounters")]

joined = encounter.Gi %>% full_join(retouch.Gi) %>%
  group_by_at(parameters) %>%
  mutate(exp = cur_group_id())

exp = unique(joined$exp)

output = joined[,c(parameters)]
output$run = ""
output$high_ret = 0
output$high_enct = 0
output$ret.enct.overlap = 0
output = output[0,]

locations = list()

for(e in 1:length(exp)) {
  Gi = joined[which(joined$exp == exp[e]),]
  
  for(run in (unique(joined$run))) {
    Gi.run = Gi[which(Gi$run == run),]
    
    ret.rast = rasterFromXYZ(Gi.run[,c(15,16,18)])
    enct.rast = rasterFromXYZ(Gi.run[,c(15,16,17)])
    
    ret.rast[ret.rast < 2] = NA
    enct.rast[enct.rast < 2] = NA
    
    r = overlay(ret.rast, enct.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      
      locations[[length(locations)+1]] = locs
    }
    
    ret.enct.o = 100 - freq(r, value = NA)
    
    output[nrow(output) + 1, ] <-
      c( 
        Gi.run[1, parameters], 
        run,
        100 - freq(ret.rast, value = NA),
        100 - freq(enct.rast, value = NA),
        ret.enct.o
      )
    
  }
}

locations.results = do.call("rbind", locations[1:length(locations)])

write.csv(locations.results, file =  "retouch-encounter-overlap-locations.csv")
write.csv(output, file =  "retouch-encounter-hotspot-overlap.csv")
