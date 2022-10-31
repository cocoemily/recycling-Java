##ANALYSIS OF VARIATION IN LAYER DATA

library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)

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

plotNormalHistogram(layer.var$nod.cnt.avg.sd)
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


#### Two technology types ####
plotNormalHistogram(two.tech.layers$nod.cnt.avg.sd) #normal
plotNormalHistogram(log(two.tech.layers$flk.cnt.avg.sd)) #lognormal
plotNormalHistogram(log(two.tech.layers$cr.avg.sd)) 
plotNormalHistogram(two.tech.layers$ri.avg.sd) 
plotNormalHistogram(two.tech.layers$num.dis.avg.sd) ##zero inflated?
plotNormalHistogram(two.tech.layers$num.enc.avg.sd) #multimodal
plotNormalHistogram(two.tech.layers$num.manu.avg.sd) #bimodal
plotNormalHistogram(two.tech.layers$num.occp.avg.sd) #multimodal
plotNormalHistogram(two.tech.layers$num.ret.avg.sd) #uniform-ish
plotNormalHistogram(two.tech.layers$num.scvg.avg.sd) #zero inflated

flk.cnt.reg = glm(log(flk.cnt.avg.sd) ~ . , data = two.tech.layers[,c(2:7, 9:14, 18)])
summary(flk.cnt.reg)

nd.cnt.rg = lm(nod.cnt.avg.sd ~ . , data = two.tech.layers[,c(2:7, 9:14, 16)])
summary(nd.cnt.rg)

cr.rg = lm(log(cr.avg.sd) ~ . , data = two.tech.layers[,c(2:7, 9:14, 20)])
plot(cr.rg, which = 2)
summary(cr.rg)

ri.rg = lm(ri.avg.sd ~ . , data = two.tech.layers[,c(2:7, 9:14, 22)])


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



#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")

mid.cor.ri = layer.cor %>% filter(time == "mid") %>% 
  gather(key = "comparison", value = "correlation", starts_with("ri"))

end.cor.ri = layer.cor %>% filter(time == "end") %>% 
  gather(key = "comparison", value = "correlation", starts_with("ri"))

ggplot(mid.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)

ggplot(end.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)


mid.cor.ri.exp1 = layer.cor %>% filter(time == "mid") %>% 
  filter(exp == 1) %>%
  gather(key = "comparison", value = "correlation", starts_with("ri"))

ggplot(mid.cor.ri.exp1) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)

##need to create a script that looks at relationships between different correlations within each square for each model run
