##ANALYSIS OF ARTIFACT EXPOSURE

library(tidyverse)
library(ggthemes)

theme_set(theme_bw())

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


##remove strict selection runs 
updated.data = exposure.data %>% filter(strict_selection == FALSE)

up.exposure.plot = updated.data %>% 
  gather(key = "time", value = "signif", mid.conf.val, end.conf.val)

ggplot(up.exposure.plot) +
  geom_bar(aes(x = time, fill = signif), position = "dodge2") +
  scale_x_discrete(labels = c("end of model run", "middle of model run"))

#exposure time of recycled artifacts are always greater than non-recycled artifacts unless the strictest of 
#selection parameters is imposed in the model

#examine what happens under strict choices
strict.exp = exposure.data %>% filter(strict_selection == TRUE)

#results will be likelihood of TRUE values
bilog = glm(mid.conf.val ~ as.factor(max_use_intensity) + as.factor(max_artifact_carry) + 
              as.factor(max_flake_size) + as.factor(max_nodules_size) + 
              as.factor(blank_prob) + as.factor(scavenge_prob) + as.factor(overlap) + 
              as.factor(mu) + as.factor(size_preference) + as.factor(flake_preference) + 
              as.factor(min_suitable_flake_size) + as.factor(min_suitable_nodule_size), 
            data = strict.exp, family = binomial)
summary(bilog)

#significant results:
#log odds increase by 1.752 as max_use_intensity increases
#log odds decrease by -6.290 as max_artifact_carry increases
#log odds decrease by -1.863 as max_flake_size increases
#log odds increase by 4.380 as max_nodule_size increases
#log odds increase as blank probability decreases
#log odds decrease as scavenge probability decreases
#log odds decrease by -6.450 as overlap increases
#log odds increase as mu increases
#log odds decrease when size preference is true
#log odds decrease when min suitable flake size increases

