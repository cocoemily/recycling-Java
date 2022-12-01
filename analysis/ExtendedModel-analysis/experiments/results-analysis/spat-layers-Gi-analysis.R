library(tidyverse)
library(rcompanion)

#Gi stat analysis
#Greater values represent a greater intensity of clustering and 
#the direction (positive or negative) indicates high or low clusters. 

#very large dataset -- will need to be analyzed on HPC
layers.Gi = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-local-G-output.csv", n_max = 600000)

exp = unique(layers.Gi$exp)

for(e in 1:length(exp)) {
  Gi = layers.Gi[which(layers.Gi$exp == exp[e]),]
}