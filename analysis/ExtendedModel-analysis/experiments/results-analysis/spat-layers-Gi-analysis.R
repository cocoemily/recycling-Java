library(tidyverse)
library(rcompanion)
library(cowplot)
library(raster)

theme_set(theme_bw())

#Gi stat analysis
#Greater values represent a greater intensity of clustering and 
#the direction (positive or negative) indicates high or low clusters. 
#Gi values are z scores

##want to look at overlap of high values between different outputs

##this will need to be set up to run on HPC


#layers.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/sub_Gi.csv")
layers.Gi = read_csv("/scratch/ec3307/recycling-Java/results/all-layer-local-Gi.csv")
parameters = colnames(layers.Gi[c(4:9, 11:17)])

exp = unique(layers.Gi$exp)

output = layers.Gi[,c("exp", parameters)]
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
  
  
  RI.rast = rasterFromXYZ(Gi.end[,c(36,37,38)])
  CR.rast = rasterFromXYZ(Gi.end[,c(36,37,39)])
  flkcnt.rast = rasterFromXYZ(Gi.end[,c(36,37,40)])
  nodcnt.rast = rasterFromXYZ(Gi.end[,c(36,37,41)])
  numdisc.rast = rasterFromXYZ(Gi.end[,c(36,37,42)])
  numscvg.rast = rasterFromXYZ(Gi.end[,c(36,37,43)])
  numenct.rast = rasterFromXYZ(Gi.end[,c(36,37,44)])
  numret.rast = rasterFromXYZ(Gi.end[,c(36,37,45)])
  
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

write.csv(output, file = "cell-counts-hotspot-overlap.csv")
