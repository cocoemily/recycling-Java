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

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size", 
               "max_nodules_size", "blank_prob", "scavenge_prob", "overlap", 
               "mu", "size_preference", "flake_preference", 
               "minx_suitable_flake_size", "strict_selection")

setwd("..")
alldata = read_csv("output/joined_sensitivity-data.csv")
setwd("analysis")

alldata = alldata %>% filter(size != "size") %>%
  mutate(total.RI = as.numeric(total.RI), 
         total.CR = as.numeric(total.CR)) %>%
  group_by(max_use_intensity, max_artifact_carry, max_flake_size, max_nodules_size, blank_prob, scavenge_prob, overlap, mu, size_preference, flake_preference, min_suitable_flake_size, min_suitable_nodule_size, strict_selection) %>%
  mutate(exp = cur_group_id()) %>%
  filter(!is.na(total.RI))

u_alldata = alldata %>% mutate(s.total.RI = ifelse(total.RI == 0, (total.RI + 0.0001), total.RI)) %>% mutate(s.total.RI = ifelse(s.total.RI == 1, (s.total.RI - 0.0001), s.total.RI))
obreg = betareg(s.total.RI ~ model_year, data = u_alldata)

# start.time <- Sys.time()
# breg_msns = betareg(s.total.RI ~ model_year + min_suitable_nodule_size, data = u_alldata)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)
# 
# start.time <- Sys.time()
# lrtest(breg_msns, obreg)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)
# 
# start.time <- Sys.time()
# coeftest(breg_msns, vcov = vcovBS(breg_msns, R=25))
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)

#test parallel processing
beta_regression = function(variable) {
  modelfun = paste("s.total.RI ~ model_year + ", variable)
  breg = betareg(eval(parse(text = modelfun)), data = u_alldata)
  lrtest(breg, obreg)
  #coeftest(breg, vcov = vcovBS(breg, R=25))
}

mclapply(c("blank_prob", "scavenge_prob"), beta_regression, mc.cores=2)
#mclapply(parameters, beta_regression, mc.cores = 13)

