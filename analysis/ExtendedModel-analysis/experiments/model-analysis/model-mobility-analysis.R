### Analysis of model outcomes by mobility

if(!require(emmeans)){install.packages("emmeans", repos = "https://cran.wustl.edu/")}
if(!require(jtools)){install.packages("jtools", repos = "https://cran.wustl.edu/")}
if(!require(pscl)){install.packages("pscl", repos = "https://cran.wustl.edu/")}
if(!require(MASS)){install.packages("MASS", repos = "https://cran.wustl.edu/")}
if(!require(rstatix)){install.packages("rstatix", repos = "https://cran.wustl.edu/")}
if(!require(sandwich)){install.packages("sandwich", repos = "https://cran.wustl.edu/")}
if(!require(faux)){install.packages("faux", repos = "https://cran.wustl.edu/")}
if(!require(AICcmodavg)){install.packages("AICcmodavg", repos = "https://cran.wustl.edu/")}

library(tidyverse)
library(ggthemes)
library(betareg)
library(lmtest)
library(rcompanion)
library(emmeans)
library(jtools)
library(pscl)
library(MASS)
library(rstatix)
library(sandwich)
library(faux)
library(AICcmodavg)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("overlap", "max_use_intensity", "max_artifact_carry", "max_flake_size", "blank_prob", "scavenge_prob", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = colnames(alldata[c(21:33)])

mu.1 = alldata[which(alldata$mu == 1),]
mu.2 = alldata[which(alldata$mu == 2),]
mu.3 = alldata[which(alldata$mu == 3),]

rm(alldata)


find_best_fit = function(mu.data) {
  mu.po.data = mu.data[,c(parameters, "total.RI")] 
  
  fit1 = glm(total.RI ~ ., data =mu.po.data, family = "poisson")
  fit2 = glm.nb(total.RI ~ ., data = mu.po.data)
  
  print(lrtest(fit1, fit2))
}

find_best_fit(mu1)
find_best_fit(mu2)
find_best_fit(mu3)

