##ANALYSIS OF ARTIFACT EXPOSURE

library(tidyverse)
library(ggthemes)
library(jtools)

theme_set(theme_bw())

exposure.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-artifact-exposure.csv")

##mid.conf.val = whether (TRUE) or not (FALSE) the recycled artifacts at the middle of model run 
## have ~significantly~ greater initial discard dates than the non-recycled artifacts
##end.conf.val = whether (TRUE) or not (FALSE) the recycled artifacts at the middle of model run 
## have ~significantly~ greater initial discard dates than the non-recycled artifacts

two.tech = exposure.data %>% filter(overlap == 1)
many.tech = exposure.data %>% filter(overlap == 2)


plot_exposure_counts = function (exposure.data) {
  exposure.plot = exposure.data %>% 
    gather(key = "time", value = "signif", mid.conf.val, end.conf.val)
  
  ggplot(exposure.plot) +
    geom_bar(aes(x = time, fill = signif), position = "dodge2") +
    scale_x_discrete(labels = c("end of model run", "middle of model run"))
}

plot_exposure_counts(two.tech)
plot_exposure_counts(many.tech)

##remove strict selection runs 
updated.data = exposure.data %>% filter(strict_selection == FALSE)

up.exposure.plot = updated.data %>% 
  gather(key = "time", value = "signif", mid.conf.val, end.conf.val)

ggplot(up.exposure.plot) +
  geom_bar(aes(x = time, fill = signif), position = "dodge2") +
  scale_x_discrete(labels = c("end of model run", "middle of model run")) +
  facet_wrap(~overlap)
#exposure time of recycled artifacts are always greater than non-recycled artifacts unless the strictest of 
#selection parameters is imposed in the model


#examine what happens under strict choices
strict.exp = exposure.data %>% filter(strict_selection == TRUE) 

two.tech.strict = strict.exp %>% filter(overlap == 1)
tt.strict.plot = two.tech.strict %>% gather(key = "time", value = "signif", mid.conf.val, end.conf.val)
ggplot(tt.strict.plot) +
  geom_bar(aes(x = time, fill = signif), position = "dodge2") +
  facet_grid(flake_preference~size_preference, labeller = label_both) +
  scale_x_discrete(labels = c("end of model run", "middle of model run"))

many.tech.strict = strict.exp %>% filter(overlap == 2)
mt.strict.plot = many.tech.strict %>% gather(key = "time", value = "signif", mid.conf.val, end.conf.val)
ggplot(mt.strict.plot) +
  geom_bar(aes(x = time, fill = signif), position = "dodge2") +
  facet_grid(flake_preference~size_preference, labeller = label_both) +
  scale_x_discrete(labels = c("end of model run", "middle of model run"))


##when are these results occuring -- under what parameter sets?
false.two = two.tech.strict %>% filter(end.conf.val == FALSE)
table(false.two$max_artifact_carry)
table(false.two$max_use_intensity)
table(false.two$max_flake_size)
#table(false.two$max_nodules_size)
table(false.two$blank_prob)
table(false.two$scavenge_prob)
table(false.two$mu)
table(false.two$size_preference)
table(false.two$min_suitable_flake_size)

test1 = glm(end.conf.val ~ mu + max_use_intensity + max_artifact_carry + 
      max_flake_size + blank_prob + scavenge_prob + min_suitable_flake_size , data = two.tech.strict, family = binomial)
summary(test1)

two.tech.strict$flip.val = !two.tech.strict$end.conf.val
many.tech.strict$flip.val = !many.tech.strict$end.conf.val

test2 = glm(flip.val ~ as.factor(max_use_intensity) + as.factor(max_artifact_carry) + 
              as.factor(max_flake_size) +
              as.factor(blank_prob) + as.factor(scavenge_prob) + as.factor(mu) +
              as.factor(min_suitable_flake_size), 
            data = two.tech.strict, family = binomial)
summary(test2)

test3 = glm(flip.val ~ as.factor(max_use_intensity) + as.factor(max_artifact_carry) + 
              as.factor(max_flake_size) + 
              as.factor(blank_prob) + as.factor(scavenge_prob) + as.factor(mu) +
              as.factor(min_suitable_flake_size) , 
            data = many.tech.strict, family = binomial)
summary(test3)

#results will be likelihood of TRUE values
mid.two.tech.fit = glm(mid.conf.val ~ max_use_intensity + max_artifact_carry + 
                         max_flake_size +
                         blank_prob + scavenge_prob + as.factor(flake_preference) + 
                         mu + as.factor(size_preference) + min_suitable_flake_size, 
                       data = two.tech.strict, family = binomial)
summary(mid.two.tech.fit)


end.two.tech.fit = glm(end.conf.val ~ max_use_intensity + max_artifact_carry + 
                         max_flake_size +
                         blank_prob + scavenge_prob + as.factor(flake_preference) + 
                         mu + as.factor(size_preference) + min_suitable_flake_size, 
                       data = two.tech.strict, family = binomial)
summary(end.two.tech.fit)

plot_summs(mid.two.tech.fit, end.two.tech.fit, model.names = c("Middle of model run", "End of model run"), 
           scale = T)
export_summs(mid.two.tech.fit, end.two.tech.fit, model.names = c("Middle of model run", "End of model run"), 
             scale = T, error_format = "[{conf.low}, {conf.high}]")

many.tech.fit = glm(mid.conf.val ~ max_use_intensity + max_artifact_carry + 
                      max_flake_size + 
                      blank_prob + scavenge_prob + as.factor(flake_preference) + 
                      mu + as.factor(size_preference)*min_suitable_flake_size, 
                    data = many.tech.strict, family = binomial)
summary(many.tech.fit)


plot_summs(two.tech.fit, many.tech.fit, model.names = c("Two technologies", "Many technologies"))
export_summs(two.tech.fit, many.tech.fit, scale = T, error_format = "[{conf.low}, {conf.high}]", 
             model.names = c("Two technologies", "Many technologies"))








##NOT SPLIT BY OVERLAP
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

