library(betareg)
library(tidyverse)
library(lmtest)
library(MASS)
library(gamlss)

source("/scratch/ec3307/recycling-Java/analysis/ExtendedModel-analysis/sensitivity-analysis/betaselect.R")

alldata = read_csv("/scratch/ec3307/recycling-Java/output/joined_sensitivity-data.csv")

alldata = alldata %>% filter(size != "size") %>%
  mutate(total.RI = as.numeric(total.RI), 
         total.CR = as.numeric(total.CR)) %>%
  group_by(max_use_intensity, max_artifact_carry, max_flake_size, max_nodules_size, blank_prob, scavenge_prob, overlap, mu, size_preference, flake_preference, min_suitable_flake_size, min_suitable_nodule_size, strict_selection) %>%
  mutate(exp = cur_group_id()) %>%
  filter(!is.na(total.RI))

u_alldata = alldata %>% mutate(s.total.RI = ifelse(total.RI == 0, (total.RI + 0.0001), total.RI)) %>% mutate(s.total.RI = ifelse(s.total.RI == 1, (s.total.RI - 0.0001), s.total.RI))

#full model
#breg1 = betareg(s.total.RI ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + factor(overlap) + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection , data=u_alldata)
#AIC(breg1)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

#backwards stepwise regression

x = as.matrix(u_alldata[, parameters])
y = as.matrix(u_alldata$s.total.RI)

step.model = betaselect(x, y, method = "backward")


# #backwards stepwise regression by hand
# aicdata = data.frame(
#   formula = character(), 
#   AIC = numeric()
# )
# 
# 
# #leave one variable out
# modelfun = paste("s.total.RI ~ model_year")
# for(i in 1:length(parameters)) {
#   if(i != args[1]) {
#     modelfun = paste(modelfun, "+", parameters[i])
#   }
# }
  
  




