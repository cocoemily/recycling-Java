##retouch and encounters overlap
library(tidyverse)
library(rcompanion)
library(raster)
library(ggpubr)

theme_set(theme_bw())

behave.Gi = read_csv("/scratch/ec3307/recycling-Java/results/all-runs-local-G.csv")
artifact.Gi = read_csv("/scratch/ec3307/recycling-Java/results/all-rr-local-G.csv")
#behave.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/sub_local-G.csv")
#artifact.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/all-rr-local-G.csv")
behave.Gi = behave.Gi[which(behave.Gi$model_year == 200000),]

parameters = colnames(artifact.Gi[,c(7:18)])

retouch.Gi = artifact.Gi[,c(parameters, "run", "row", "col", "G.retouch.obj")]
encounter.Gi = behave.Gi[,c(parameters, "run", "row", "col", "Gi.stat.num.encounters")]

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

for(e in 1:length(exp)) {
  Gi = joined[which(joined$exp == exp[e]),]
  
  for(run in (unique(joined$run))) {
    Gi.run = Gi[which(Gi$run == run),]
    
    ret.rast = rasterFromXYZ(Gi.run[,c(14,15,17)])
    enct.rast = rasterFromXYZ(Gi.run[,c(14,15,16)])
    
    ret.rast[ret.rast < 2] = NA
    enct.rast[enct.rast < 2] = NA
    
    r = overlay(ret.rast, enct.rast, fun=sum)
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

write.csv(output, file = "retouch-encounter-overlap.csv")
