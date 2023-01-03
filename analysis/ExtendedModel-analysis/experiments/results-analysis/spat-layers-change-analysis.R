library(tidyverse)
library(rcompanion)

#spatial change analysis between start, middle, end of model run

layers.change = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-spatial-change.csv")

start.mid = layers.change[which(layers.change$timeperiod == "midgrid"),c(4:9, 11:17, 36,37,46:55)]
mid.end = layers.change[which(layers.change$timeperiod == "endgrid"),c(4:9, 11:17, 36,37,46:55)]

exp = unique(layers.change$exp)

for(e in 1:length(exp)) {
  sm = start.mid[which(start.mid$exp == exp[e]),]
  
}