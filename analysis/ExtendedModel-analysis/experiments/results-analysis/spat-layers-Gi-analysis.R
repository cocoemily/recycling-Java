library(tidyverse)
library(rcompanion)
library(cowplot)

theme_set(theme_bw())

#Gi stat analysis
#Greater values represent a greater intensity of clustering and 
#the direction (positive or negative) indicates high or low clusters. 

layers.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/all-local-G-results.csv", n_max = 600000)
#layers.Gi = read_csv("/scratch/ec3307/recycling-Java/results/all-local-G-results.csv")

exp = unique(layers.Gi$exp)

for(e in 1:length(exp)) {
  Gi = layers.Gi[which(layers.Gi$exp == exp[e]),]
  
  print(exp[e])
  
}
