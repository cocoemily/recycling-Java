##ANALYSIS OF ARTIFACT RETOUCH INTENSITY
library(tidyverse)
library(ggthemes)
library(jtools)

theme_set(theme_bw())

intensity.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-artifact-retouch-intensity.csv")

two.tech = intensity.data %>% filter(overlap == 1)
many.tech = intensity.data %>% filter(overlap == 2)

plot_intensity_counts = function (intensity.data) {
  intensity.plot = intensity.data %>% 
    gather(key = "time", value = "signif", mid.signf, end.signf)
  
  ggplot(intensity.plot) +
    geom_bar(aes(x = time, fill = signif), position = "dodge2") +
    scale_x_discrete(labels = c("end of model run", "middle of model run"))
}

plot_intensity_counts(two.tech)
plot_intensity_counts(many.tech)

mid.two.tech.fit = glm(mid.signf ~ max_use_intensity + max_artifact_carry + 
                     max_flake_size + max_nodules_size + mu +
                     blank_prob + scavenge_prob + as.factor(flake_preference) + 
                     mu + as.factor(size_preference) + as.factor(strict_selection) +
                     min_suitable_flake_size + min_suitable_nodule_size, 
                   data = two.tech, family = binomial)
summary(mid.two.tech.fit)

end.two.tech.fit = glm(end.signf ~ max_use_intensity + max_artifact_carry + 
                         max_flake_size + max_nodules_size + mu +
                         blank_prob + scavenge_prob + as.factor(flake_preference) + 
                         mu + as.factor(size_preference) + as.factor(strict_selection) +
                         min_suitable_flake_size + min_suitable_nodule_size, 
                       data = two.tech, family = binomial)
summary(end.two.tech.fit)

plot_summs(mid.two.tech.fit, end.two.tech.fit, model.names = c("Middle of model run", "End of model run"), 
           scale = T)
#significant values do not overlap 0 on estimates
export_summs(mid.two.tech.fit, end.two.tech.fit, model.names = c("Middle of model run", "End of model run"), 
             scale = T, error_format = "[{conf.low}, {conf.high}]")


mid.many.tech.fit = glm(mid.signf ~ max_use_intensity + max_artifact_carry + 
                         max_flake_size + max_nodules_size + mu +
                         blank_prob + scavenge_prob + as.factor(flake_preference) + 
                         mu + as.factor(size_preference) + as.factor(strict_selection) +
                         min_suitable_flake_size + min_suitable_nodule_size, 
                       data = many.tech, family = binomial)
summary(mid.many.tech.fit)

end.many.tech.fit = glm(end.signf ~ max_use_intensity + max_artifact_carry + 
                         max_flake_size + max_nodules_size + mu +
                         blank_prob + scavenge_prob + as.factor(flake_preference) + 
                         mu + as.factor(size_preference) + as.factor(strict_selection) +
                         min_suitable_flake_size + min_suitable_nodule_size, 
                       data = many.tech, family = binomial)
summary(end.many.tech.fit)

plot_summs(mid.many.tech.fit, end.many.tech.fit, model.names = c("Middle of model run", "End of model run"), 
           scale = T)
#significant values do not overlap 0 on estimates
export_summs(mid.many.tech.fit, end.many.tech.fit, model.names = c("Middle of model run", "End of model run"), 
             scale = T, error_format = "[{conf.low}, {conf.high}]")
