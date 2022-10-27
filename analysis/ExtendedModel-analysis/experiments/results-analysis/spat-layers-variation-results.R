##ANALYSIS OF VARIATION IN LAYER DATA

library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)

layer.var = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-variation-output.csv")

summary(layer.var$nod.cnt.avg.sd)
summary(layer.var$flk.cnt.avg.sd)
summary(layer.var$cr.avg.sd)
summary(layer.var$ri.avg.sd)
summary(layer.var$num.dis.avg.sd)
summary(layer.var$num.enc.avg.sd)
summary(layer.var$num.manu.avg.sd)
summary(layer.var$num.occp.avg.sd)
summary(layer.var$num.ret.avg.sd)
summary(layer.var$num.scvg.avg.sd)

##number of discard and scavenging events are very highly variably between model runs of same experiment
##recycling intensity has low variation between model runs of same experiment, same with cortex ratios
##flake counts and nodule counts are fairly variable as well between model runs

plotNormalHistogram(layer.var$nod.cnt.avg.sd)
descdist(layer.var$nod.cnt.avg.sd) #lognormal
plotNormalHistogram(log(layer.var$flk.cnt.avg.sd))
plotNormalHistogram(layer.var$cr.avg.sd) ##zero inflated?
plotNormalHistogram(layer.var$ri.avg.sd) ##zero inflated?
plotNormalHistogram(log(layer.var$num.dis.avg.sd))
plotNormalHistogram(layer.var$num.enc.avg.sd) #multimodal
plotNormalHistogram(layer.var$num.manu.avg.sd) #bimodal
plotNormalHistogram(layer.var$num.occp.avg.sd) #multimodal
plotNormalHistogram(layer.var$num.ret.avg.sd) #uniform-ish
plotNormalHistogram(layer.var$num.scvg.avg.sd)


flk.cnt.reg = glm(log(flk.cnt.avg.sd) ~ . , data = layer.var[,c(2:14, 18)])
summary(flk.cnt.reg)

nod.cnt.reg = lm(nod.cnt.avg.sd ~ . , data = layer.var[,c(2:14, 16)])
summary(nod.cnt.reg)

