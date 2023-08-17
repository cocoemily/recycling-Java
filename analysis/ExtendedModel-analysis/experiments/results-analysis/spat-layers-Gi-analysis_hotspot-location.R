library(tidyverse)
library(rcompanion)
library(raster)
library(ggpubr)

#Gi stat analysis
#Greater values represent a greater intensity of clustering and 
#the direction (positive or negative) indicates high or low clusters. 
#Gi values are z scores

#layers.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/sub-local-G.csv")
layers.Gi = read_csv("/scratch/ec3307/updated-recycling-Java/recycling-Java/results/all-local-G-results.csv")
parameters = colnames(layers.Gi[c(2:14)])

exp = unique(layers.Gi$exp)

####create a location list for every set of hotspots####
loc.ri = list()
loc.cr = list()
loc.fc = list()
loc.nc = list()
loc.nd = list()
loc.ns = list()
loc.ne = list()
loc.nr = list()
loc.rp = list()

for(e in 1:length(exp)) {
  Gi = layers.Gi[which(layers.Gi$exp == exp[e]),]
  
  for(run in unique(layers.Gi$run)) {
    Gi.run = Gi[which(Gi$run == run),]
    
    RI.rast = rasterFromXYZ(Gi.run[,c(28,29,30)])
    CR.rast = rasterFromXYZ(Gi.run[,c(28,29,31)])
    flkcnt.rast = rasterFromXYZ(Gi.run[,c(28,29,32)])
    nodcnt.rast = rasterFromXYZ(Gi.run[,c(28,29,33)])
    numdisc.rast = rasterFromXYZ(Gi.run[,c(28,29,34)])
    numscvg.rast = rasterFromXYZ(Gi.run[,c(28,29,35)])
    numenct.rast = rasterFromXYZ(Gi.run[,c(28,29,36)])
    numret.rast = rasterFromXYZ(Gi.run[,c(28,29,37)])
    retprop.rast = rasterFromXYZ(Gi.run[,c(28,29,38)])
    
    RI.rast[RI.rast < 2] = NA
    CR.rast[CR.rast < 2] = NA
    flkcnt.rast[flkcnt.rast < 2] = NA
    nodcnt.rast[nodcnt.rast < 2] = NA
    numdisc.rast[numdisc.rast < 2] = NA
    numscvg.rast[numscvg.rast < 2] = NA
    numenct.rast[numenct.rast < 2] = NA
    numret.rast[numret.rast < 2] = NA
    retprop.rast[retprop.rast < 2] = NA
    
    #####RI#####
    locs = as.data.frame(rasterToPoints(RI.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri[[length(loc.ri)+1]] = locs
    }
    
    #####CR#####
    locs = as.data.frame(rasterToPoints(CR.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.cr[[length(loc.cr)+1]] = locs
    }
    
    #####flake count#####
    locs = as.data.frame(rasterToPoints(flkcnt.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.fc[[length(loc.fc)+1]] = locs
    }
    
    #####nodule count#####
    locs = as.data.frame(rasterToPoints(nodcnt.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.nc[[length(loc.nc)+1]] = locs
    }
    
    #####discard events#####
    locs = as.data.frame(rasterToPoints(numdisc.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.fc[[length(loc.fc)+1]] = locs
    }
    
    #####overlap RI and scavenge events#####
    locs = as.data.frame(rasterToPoints(numscvg.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.nd[[length(loc.nd)+1]] = locs
    }
    
    #####encounters#####
    locs = as.data.frame(rasterToPoints(numenct.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ne[[length(loc.ne)+1]] = locs
    }
    
    #####retouch events#####
    locs = as.data.frame(rasterToPoints(numret.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.fc[[length(loc.fc)+1]] = locs
    }
    
    #####retouched artifact proportion#####
    locs = as.data.frame(rasterToPoints(retprop.rast))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.rp[[length(loc.rp)+1]] = locs
    }
    
  }
  
}

loc.ri.results = do.call("rbind", loc.ri[1:length(loc.ri)])
loc.cr.results = do.call("rbind", loc.cr[1:length(loc.cr)])
loc.fc.results = do.call("rbind", loc.fc[1:length(loc.fc)])
loc.nc.results = do.call("rbind", loc.nc[1:length(loc.nc)])
loc.nd.results = do.call("rbind", loc.nd[1:length(loc.nd)])
loc.ns.results = do.call("rbind", loc.ns[1:length(loc.ns)])
loc.ne.results = do.call("rbind", loc.ne[1:length(loc.ne)])
loc.nr.results = do.call("rbind", loc.nr[1:length(loc.nr)])
loc.rp.results = do.call("rbind", loc.rp[1:length(loc.rp)])

####write CSVS when not NULL ####
if(!is.null(loc.ri.results)) {
  write.csv(loc.ri.results, file =  "ri-hotspots-locations.csv")
}
if(!is.null(loc.cr.results)) {
  write.csv(loc.cr.results, file =  "cr-hotspots-locations.csv")
}
if(!is.null(loc.fc.results)) {
  write.csv(loc.fc.results, file = "flake-counts-hotspots-locations.csv")
}
if(!is.null(loc.nc.results)) {
  write.csv(loc.nc.results, file = "nodule-counts-hotspots-locations.csv")
}
if(!is.null(loc.nd.results)) {
  write.csv(loc.nd.results, file = "discards-hotspots-locations.csv")
}
if(!is.null(loc.ns.results)) {
  write.csv(loc.ns.results, filename = "scavenges-hotspots-locations.csv")
}
if(!is.null(loc.ne.results)) {
  write.csv(loc.ne.results, file = "encounters-hotspots-locations.csv")
}
if(!is.null(loc.nr.results)) {
  write.csv(loc.nr.results, file = "retouches-hotspots-locations.csv")
}
if(!is.null(loc.rp.results)) {
  write.csv(loc.rp.results, file = "retprop-hotspots-locations.csv")
}

