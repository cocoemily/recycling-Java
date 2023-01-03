library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(rstatix)
library(lme4)

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

layer.cor.end = layer.cor.end %>% group_by(row, col) %>% 
  mutate(square = cur_group_id())


lmm = lmer(ri.num.scvg.cor ~ overlap + mu + scavenge_prob +
             blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
             max_nodules_size + flake_preference + size_preference + strict_selection +
             min_suitable_flake_size + min_suitable_nodule_size + (1 | square), data = layer.cor.end[,c(1,2,4:16,21, 27)])
summary(lmm)
