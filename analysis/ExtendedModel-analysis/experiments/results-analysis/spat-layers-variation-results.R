##ANALYSIS OF VARIATION IN LAYER DATA

library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(VGAM)
library(lme4)

#### ANALYSIS OF VARIATION OF OUTPUT VARIABLES BETWEEN GRID SQUARES ACROSS MODEL RUNS ####
layer.var = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-variation-output.csv")
two.tech.layers = layer.var %>% filter(overlap == 1)
many.tech.layers = layer.var %>% filter(overlap == 2)

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

#### Overall analysis ####
# plotNormalHistogram(layer.var$nod.cnt.avg.sd)
# plotNormalHistogram(log(layer.var$flk.cnt.avg.sd))
# plotNormalHistogram(log(layer.var$cr.avg.sd))
# plotNormalHistogram(layer.var$ri.avg.sd)
# plotNormalHistogram(log(layer.var$num.dis.avg.sd))
# plotNormalHistogram(layer.var$num.enc.avg.sd) #multimodal
# plotNormalHistogram(layer.var$num.manu.avg.sd) #bimodal
# plotNormalHistogram(layer.var$num.occp.avg.sd) #multimodal
# plotNormalHistogram(layer.var$num.ret.avg.sd) #uniform-ish
# plotNormalHistogram(layer.var$num.scvg.avg.sd)

# 
# flk.cnt.reg = glm(log(flk.cnt.avg.sd) ~ . , data = layer.var[,c(2:14, 18)])
# summary(flk.cnt.reg)
# 
# nod.cnt.reg = lm(nod.cnt.avg.sd ~ . , data = layer.var[,c(2:14, 16)])
# summary(nod.cnt.reg)
# 
# 
# ri.reg = vglm(ri.avg.sd ~ ., tobit(Lower = 0), data = layer.var[,c(2:14, 22)])
# summary(ri.reg)


#### Two technology types ####
plotNormalHistogram(sqrt(two.tech.layers$nod.cnt.avg.sd)) #square root transformation
plotNormalHistogram(log(two.tech.layers$flk.cnt.avg.sd)) #lognormal
plotNormalHistogram(log(two.tech.layers$cr.avg.sd)) #lognormal
plotNormalHistogram(two.tech.layers$ri.avg.sd) #tobit regression
plotNormalHistogram(log(two.tech.layers$num.dis.avg.sd)) #lognormal
summary(two.tech.layers$num.dis.avg.sd)
plotNormalHistogram(two.tech.layers$num.enc.avg.sd) #multimodal
plotNormalHistogram(two.tech.layers$num.manu.avg.sd) #bimodal
plotNormalHistogram(two.tech.layers$num.occp.avg.sd) #multimodal
plotNormalHistogram(two.tech.layers$num.ret.avg.sd) #uniform-ish
plotNormalHistogram(two.tech.layers$num.scvg.avg.sd) #??? not sure what to do 
summary(two.tech.layers$num.scvg.avg.sd)

flk.cnt.reg = glm(log(flk.cnt.avg.sd) ~ . , data = two.tech.layers[,c(2:7, 9:14, 18)])
plot(flk.cnt.reg, which = 2)
summary(flk.cnt.reg)
drop1(flk.cnt.reg, test = "F")

nd.cnt.reg = lm(sqrt(nod.cnt.avg.sd) ~ . , data = two.tech.layers[,c(2:7, 9:14, 16)])
plot(nd.cnt.reg, which = 2)
summary(nd.cnt.reg)

cr.rg = lm(log(cr.avg.sd) ~ . , data = two.tech.layers[,c(2:7, 9:14, 20)])
plot(cr.rg, which = 2)
summary(cr.rg)

#Tobit regression not working for recycling intensity
ri.reg = vglm(ri.avg.sd ~ ., family = tobit(imethod = 1), etastart = c(rep(0, 13)), data = two.tech.layers[,c(2:14, 22)])
summary(ri.reg)

num.dis.reg = lm(log(num.dis.avg.sd) ~ ., data = two.tech.layers[,c(2:7, 9:14, 24)]) 
plot(num.dis.reg, which = 2)
summary(num.dis.reg)


#### Many technology types ####
plotNormalHistogram(many.tech.layers$nod.cnt.avg.sd) #normal
plotNormalHistogram(log(many.tech.layers$flk.cnt.avg.sd)) #lognormal
plotNormalHistogram(many.tech.layers$cr.avg.sd) ##zero inflated?
plotNormalHistogram(many.tech.layers$ri.avg.sd) ##zero inflated normal?
plotNormalHistogram(many.tech.layers$num.dis.avg.sd) ##zero inflated?
plotNormalHistogram(many.tech.layers$num.enc.avg.sd) #multimodal
plotNormalHistogram(many.tech.layers$num.manu.avg.sd) #bimodal
plotNormalHistogram(many.tech.layers$num.occp.avg.sd) #multimodal
plotNormalHistogram(many.tech.layers$num.ret.avg.sd) #uniform-ish
plotNormalHistogram(many.tech.layers$num.scvg.avg.sd) #zero inflated



