library(tidyverse)
library(rcompanion)
library(raster)
library(ggpubr)

theme_set(theme_bw())

#Gi stat analysis
#Greater values represent a greater intensity of clustering and 
#the direction (positive or negative) indicates high or low clusters. 
#Gi values are z scores

#layers.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/sub-local-G.csv")
layers.Gi = read_csv("/scratch/ec3307/updated-recycling-Java/recycling-Java/results/all-local-G-results.csv")
parameters = colnames(layers.Gi[c(2:14)])

exp = unique(layers.Gi$exp)

####create a location list for every set of overlaps####
loc.ri.ci = list()
loc.ri.fc = list()
loc.ri.nc = list()
loc.ri.nd = list()
loc.ri.ns = list()
loc.ri.ne = list()
loc.ri.nr = list()
loc.ri.rp = list()

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
    
    #####overlap RI and CI#####
    r = overlay(RI.rast, CR.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.ci[[length(loc.ri.ci)+1]] = locs
    }
    
    #####overlap RI and flake count#####
    r = overlay(RI.rast, flkcnt.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.fc[[length(loc.ri.fc)+1]] = locs
    }
    
    #####overlap RI and nodule count#####
    r = overlay(RI.rast, nodcnt.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.nc[[length(loc.ri.nc)+1]] = locs
    }
    
    #####overlap RI and discard events#####
    r = overlay(RI.rast, numdisc.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.fc[[length(loc.ri.fc)+1]] = locs
    }
    
    #####overlap RI and scavenge events#####
    r = overlay(RI.rast, numscvg.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.ns[[length(loc.ri.ns)+1]] = locs
    }
    
    #####overlap RI and encounters#####
    r = overlay(RI.rast, numenct.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.ne[[length(loc.ri.ne)+1]] = locs
    }
    
    #####overlap RI and retouch events#####
    r = overlay(RI.rast, numret.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.fc[[length(loc.ri.fc)+1]] = locs
    }
    
    #####overlap RI and retouched artifact proportion#####
    r = overlay(RI.rast, retprop.rast, fun=sum)
    locs = as.data.frame(rasterToPoints(r))[,c(1,2)]
    if(nrow(locs) > 0) {
      locs[,c(parameters)] = Gi.run[1, parameters]
      locs$run = run
      loc.ri.rp[[length(loc.ri.rp)+1]] = locs
    }
    
  }
  
}

loc.ri.ci.results = do.call("rbind", loc.ri.ci[1:length(loc.ri.ci)])
loc.ri.fc.results = do.call("rbind", loc.ri.fc[1:length(loc.ri.fc)])
loc.ri.nc.results = do.call("rbind", loc.ri.nc[1:length(loc.ri.nc)])
loc.ri.nd.results = do.call("rbind", loc.ri.nd[1:length(loc.ri.nd)])
loc.ri.ns.results = do.call("rbind", loc.ri.ns[1:length(loc.ri.ns)])
loc.ri.ne.results = do.call("rbind", loc.ri.ne[1:length(loc.ri.ne)])
loc.ri.nr.results = do.call("rbind", loc.ri.nr[1:length(loc.ri.nr)])
loc.ri.rp.results = do.call("rbind", loc.ri.rp[1:length(loc.ri.rp)])

####write CSVS when not NULL ####
if(!is.null(loc.ri.ci.results)) {
  write.csv(loc.ri.ci.results, file =  "ri-cr-overlap-locations.csv")
}
if(!is.null(loc.ri.fc.results)) {
  write.csv(loc.ri.fc.results, file = "ri-flake-counts-overlap-locations.csv")
}
if(!is.null(loc.ri.nc.results)) {
  write.csv(loc.ri.nc.results, file = "ri-nodule-counts-overlap-locations.csv")
}
if(!is.null(loc.ri.nd.results)) {
  write.csv(loc.ri.nd.results, file = "ri-discards-overlap-locations.csv")
}
if(!is.null(loc.ri.ns.results)) {
  write.csv(loc.ri.ns.results, file = "ri-scavenges-overlap-locations.csv")
}
if(!is.null(loc.ri.ne.results)) {
  write.csv(loc.ri.ne.results, file = "ri-encounters-overlap-locations.csv")
}
if(!is.null(loc.ri.nr.results)) {
  write.csv(loc.ri.nr.results, file = "ri-retouches-overlap-locations.csv")
}
if(!is.null(loc.ri.rp.results)) {
  write.csv(loc.ri.rp.results, file = "ri-retprop-overlap-locations.csv")
}

