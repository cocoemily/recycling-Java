library(tidyverse)
library(data.table)
library(knitr)
library(ggpubr)
library(moments)
library(emmeans)
library(multcomp)
library(rcompanion)
library(forecast)
library(gamlss)
library(pscl)
library(MASS)
library(glmmTMB)
library(rstatix)
library(bbmle)
library(fitdistrplus)
library(lmtest)
library(betareg)
library(sandwich)
library(foreign)
library(car)
library(gplots)
library(rcompanion)
library(parallel)

args = commandArgs(trailingOnly=TRUE)

alldata = read_csv("/scratch/ec3307/recycling-Java/output/joined_sensitivity-data.csv")

alldata = alldata %>% filter(size != "size") %>%
  mutate(total.RI = as.numeric(total.RI), 
         total.CR = as.numeric(total.CR)) %>%
  group_by(max_use_intensity, max_artifact_carry, max_flake_size, max_nodules_size, blank_prob, scavenge_prob, overlap, mu, size_preference, flake_preference, min_suitable_flake_size, min_suitable_nodule_size, strict_selection) %>%
  mutate(exp = cur_group_id()) %>%
  filter(!is.na(total.RI))

u_alldata = alldata %>% mutate(s.total.RI = ifelse(total.RI == 0, (total.RI + 0.0001), total.RI)) %>% mutate(s.total.RI = ifelse(s.total.RI == 1, (s.total.RI - 0.0001), s.total.RI))
obreg = betareg(s.total.RI ~ model_year, data = u_alldata)

#beta regression function
beta_regression = function(variable) {
  modelfun = paste("s.total.RI ~ model_year + ", variable)
  breg = betareg(eval(parse(text = modelfun)), data = u_alldata)
  lrtest(breg, obreg)
  #coeftest(breg, vcov = vcovBS(breg, R=25))
}

beta_regression(args[1])

