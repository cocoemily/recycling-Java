library(betareg)
library(tidyverse)
library(lmtest)


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

#full model beta regression analysis
breg1 = betareg(s.total.RI ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection , data=u_alldata)
coeftest(breg1)

breg2 = betareg(s.total.RI ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + strict_selection , data=u_alldata)
coeftest(breg2)

lrtest(breg1, breg2)