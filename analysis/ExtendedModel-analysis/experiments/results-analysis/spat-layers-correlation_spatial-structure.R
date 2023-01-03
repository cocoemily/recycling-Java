library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(rstatix)

source("ExtendedModel-analysis/experiments/results-analysis/helper-functions.R")

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")
cor.names = colnames(layer.cor[,17:25])
cor.names = cor.names[-length(cor.names)] #removing occupations correlation because encounters = occupations

layer.cor1 = layer.cor[-c(26:27)]
layer.cor1 = layer.cor1 %>% rename("row" = "row...1", 
                                   "col" = "col...2")
layer.cor = layer.cor1

#####end of model run#####
layer.cor.end = layer.cor[which(layer.cor$time == "end"),]
plotNormalHistogram(layer.cor.end$ri.num.scvg.cor)
fit1 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,21)])
summary(fit1)

fit1.2 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit1.2)

#ri.cr.cor
plotNormalHistogram(layer.cor.end$ri.cr.cor)
fit2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,19)])
summary(fit2)

fit2.2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit2.2)

#cortex ratio and object count
plotNormalHistogram(layer.cor.end$cr.obj.cnt.cor)
fit3 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,17)])
summary(fit3)
fit3.2 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit3.2)

plotNormalHistogram(layer.cor.end$ri.obj.cnt.cor)
fit4 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,18)])
summary(fit4)
fit4.2 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit4.2)


plotNormalHistogram(layer.cor.end$ri.num.disc.cor)
fit5 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,20)])
summary(fit5)
fit5.2 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit5.2)

plotNormalHistogram(layer.cor.end$ri.num.enct.cor)
fit6 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,22)])
summary(fit6)
fit6.2 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit6.2)

plotNormalHistogram(layer.cor.end$ri.num.manu.cor)
fit7 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,23)])
summary(fit7)
fit7.2 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit7.2)

plotNormalHistogram(layer.cor.end$ri.num.ret.cor)
fit8 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,24)])
summary(fit8)
fit8.2 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit8.2)

#####middle of model run#####
layer.cor.mid = layer.cor[which(layer.cor$time == "mid"),]
plotNormalHistogram(layer.cor.mid$ri.num.scvg.cor)
fit1 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,21)])
summary(fit1)

fit1.2 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit1.2)

#ri.cr.cor
plotNormalHistogram(layer.cor.mid$ri.cr.cor)
fit2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,19)])
summary(fit2)

fit2.2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit2.2)

#cortex ratio and object count
plotNormalHistogram(layer.cor.mid$cr.obj.cnt.cor)
fit3 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,17)])
summary(fit3)
fit3.2 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit3.2)

plotNormalHistogram(layer.cor.mid$ri.obj.cnt.cor)
fit4 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,18)])
summary(fit4)
fit4.2 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit4.2)


plotNormalHistogram(layer.cor.mid$ri.num.disc.cor)
fit5 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,20)])
summary(fit5)
fit5.2 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit5.2)

plotNormalHistogram(layer.cor.mid$ri.num.enct.cor)
fit6 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,22)])
summary(fit6)
fit6.2 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit6.2)

plotNormalHistogram(layer.cor.mid$ri.num.manu.cor)
fit7 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,23)])
summary(fit7)
fit7.2 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit7.2)

plotNormalHistogram(layer.cor.mid$ri.num.ret.cor)
fit8 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,24)])
summary(fit8)
fit8.2 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit8.2)

####ANALYSIS OF CORRELATIONS BETWEEN OUTPUTS BY SCAVENGING PROBABILITY
mid.cor.ri = layer.cor %>% filter(time == "mid") %>%
  gather(key = "comparison", value = "correlation", starts_with("ri"))

end.cor.ri = layer.cor %>% filter(time == "end") %>%
  gather(key = "comparison", value = "correlation", starts_with("ri"))


ggplot(mid.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(~scavenge_prob)

ggplot(end.cor.ri) +
  geom_boxplot(aes(x = comparison, y = scavenge_prob, group = comparison, color = comparison)) +
  facet_wrap(~correlation)