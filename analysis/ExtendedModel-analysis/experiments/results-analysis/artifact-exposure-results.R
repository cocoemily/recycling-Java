##ANALYSIS OF ARTIFACT EXPOSURE

library(tidyverse)

exposure.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-exposure-results.csv")

##mid.conf.val = whether (TRUE) or not (FALSE) the recycled artifacts at the middle of model run 
## have ~significantly~ greater initial discard dates than the non-recycled artifacts
##end.conf.val = whether (TRUE) or not (FALSE) the recycled artifacts at the middle of model run 
## have ~significantly~ greater initial discard dates than the non-recycled artifacts


exposure.plot = exposure.data %>% 
  gather(key = "time", value = "signif", mid.conf.val, end.conf.val)

ggplot(exposure.plot) +
  geom_bar(aes(x = time, fill = signif), position = "dodge2") +
  scale_x_discrete(labels = c("end of model run", "middle of model run")) 



##need to play with ordering of these probably
mid.bilog = glm(mid.conf.val ~ max_use_intensity + max_artifact_carry + max_flake_size + max_nodules_size +
                  blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + 
                  min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = exposure.data, 
                family = binomial())
summary(mid.bilog)

end.bilog = glm(end.conf.val ~ max_use_intensity + max_artifact_carry + max_flake_size + max_nodules_size +
                  blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + 
                  min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = exposure.data, 
                family = binomial())
summary(end.bilog)

