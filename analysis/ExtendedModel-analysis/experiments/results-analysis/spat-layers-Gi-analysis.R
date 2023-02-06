library(tidyverse)
library(rcompanion)
library(raster)
library(ggpubr)

theme_set(theme_bw())

#Gi stat analysis
#Greater values represent a greater intensity of clustering and 
#the direction (positive or negative) indicates high or low clusters. 
#Gi values are z scores

#layers.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/sub_local-G.csv")
layers.Gi = read_csv("/scratch/ec3307/recycling-Java/results/all-runs-local-G.csv")
parameters = colnames(layers.Gi[c(1:12)])

exp = unique(layers.Gi$exp)

output = layers.Gi[,c("exp", parameters)]
output$run = 0
output$RI.CR.overlap  = 0
output$RI.flkcnt.overlap = 0
output$RI.nodcnt.overlap = 0
output$RI.numdisc.overlap = 0
output$RI.numscvg.overlap = 0
output$RI.numenct.overlap = 0
output$RI.numret.overlap = 0 
output = output[0,]

for(e in 1:length(exp)) {
  Gi = layers.Gi[which(layers.Gi$exp == exp[e]),]
  Gi.end = Gi[which(Gi$model_year == 200000),]
  
  for(run in 1:length(unique(layers.Gi$run))) {
    Gi.run = Gi.end[which(Gi.end$run == run),]
    
    RI.rast = rasterFromXYZ(Gi.run[,c(25,26,27)])
    CR.rast = rasterFromXYZ(Gi.run[,c(25,26,28)])
    flkcnt.rast = rasterFromXYZ(Gi.run[,c(25,26,29)])
    nodcnt.rast = rasterFromXYZ(Gi.run[,c(25,26,30)])
    numdisc.rast = rasterFromXYZ(Gi.run[,c(25,26,31)])
    numscvg.rast = rasterFromXYZ(Gi.run[,c(25,26,32)])
    numenct.rast = rasterFromXYZ(Gi.run[,c(25,26,33)])
    numret.rast = rasterFromXYZ(Gi.run[,c(25,26,34)])
    
    RI.rast[RI.rast < 2] = NA
    CR.rast[CR.rast < 2] = NA
    flkcnt.rast[flkcnt.rast < 2] = NA
    nodcnt.rast[nodcnt.rast < 2] = NA
    numdisc.rast[numdisc.rast < 2] = NA
    numscvg.rast[numscvg.rast < 2] = NA
    numenct.rast[numenct.rast < 2] = NA
    numret.rast[numret.rast < 2] = NA
    
    r = overlay(RI.rast, CR.rast, fun=sum)
    RI.CR.o = 100 - freq(r, value = NA)
    
    r = overlay(RI.rast, flkcnt.rast, fun=sum)
    RI.flkcnt.o = 100 - freq(r, value = NA)
    
    r = overlay(RI.rast, nodcnt.rast, fun=sum)
    RI.nodcnt.o = 100 - freq(r, value = NA)
    
    r = overlay(RI.rast, numdisc.rast, fun=sum)
    RI.numdisc.o = 100 - freq(r, value = NA)
    
    r = overlay(RI.rast, numscvg.rast, fun=sum)
    RI.numscvg.o = 100 - freq(r, value = NA)
    
    r = overlay(RI.rast, numenct.rast, fun=sum)
    RI.numenct.o = 100 - freq(r, value = NA)
    
    r = overlay(RI.rast, numret.rast, fun=sum)
    RI.numret.o = 100 - freq(r, value = NA)
    
    output[nrow(output) + 1, ] <-
      c(
        exp[e], 
        run,
        Gi.end[1, parameters], 
        RI.CR.o, 
        RI.flkcnt.o, 
        RI.nodcnt.o, 
        RI.numdisc.o, 
        RI.numscvg.o, 
        RI.numenct.o, 
        RI.numret.o
      )
  }
  
}

#write.csv(output, file = "cell-counts-hotspot-overlap.csv")
