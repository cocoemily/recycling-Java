##ANALYSIS OF ARTIFACT RETOUCH INTENSITY
library(tidyverse)
library(ggthemes)
library(jtools)

theme_set(theme_bw())

intensity.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-artifact-retouch-intensity.csv")

two.tech = intensity.data %>% filter(overlap == 1)
many.tech = intensity.data %>% filter(overlap == 2)

plot_intensity_counts = function (i.data) {
  intensity.plot = i.data %>% 
    gather(key = "time", value = "signif", mid.signf, end.signf)
  intensity.plot$time = factor(intensity.plot$time, levels = c("mid.signf", "end.signf"))
  intensity.plot$signif = factor(intensity.plot$signif, levels = c("TRUE", "FALSE"))
  
  flake.labs = c("flake preference", "nodule preference")
  names(flake.labs) = c("TRUE", "FALSE")
  size.labs = c("size preference", "no size preference")
  names(size.labs) = c("TRUE", "FALSE")
  strict.labs = c("strict selection", "no strict selection")
  names(strict.labs) = c("TRUE", "FALSE")
  
  ggplot(intensity.plot) +
    geom_bar(aes(x = time, fill = signif), position = "dodge2") +
    facet_grid(flake_preference + size_preference ~ strict_selection , 
               labeller = labeller(flake_preference = flake.labs, 
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs)) +
    scale_x_discrete(labels = c("middle", "end")) +
    scale_fill_brewer(palette = "Set2", na.value = "grey") +
    labs(fill = "significantly greater?") +
    theme(legend.key.size = unit(0.25, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8), 
          strip.text = element_text(size = 8))
}

plot_intensity_counts(two.tech)
plot_intensity_counts(many.tech)

grid = ggarrange(plot_intensity_counts(two.tech), plot_intensity_counts(many.tech), 
                 common.legend = T, legend = "bottom", labels = "AUTO")

# ggsave(filename = "../figures/artifact-retouch-intensity-results.tiff", 
#        plot = grid, 
#        dpi = 300, width = 8)



mid.two.tech.fit = glm(mid.signf ~ max_use_intensity + max_artifact_carry + 
                     max_flake_size + max_nodules_size + mu +
                     blank_prob + scavenge_prob + as.factor(flake_preference) + 
                     mu + as.factor(size_preference) + as.factor(strict_selection) +
                     min_suitable_flake_size, 
                   data = two.tech, family = binomial)
summary(mid.two.tech.fit)

end.two.tech.fit = glm(end.signf ~ max_use_intensity + max_artifact_carry + 
                         max_flake_size + max_nodules_size + mu +
                         blank_prob + scavenge_prob + as.factor(flake_preference) + 
                         mu + as.factor(size_preference) + as.factor(strict_selection) +
                         min_suitable_flake_size, 
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
                         min_suitable_flake_size, 
                       data = many.tech, family = binomial)
summary(mid.many.tech.fit)

end.many.tech.fit = glm(end.signf ~ max_use_intensity + max_artifact_carry + 
                         max_flake_size + max_nodules_size + mu +
                         blank_prob + scavenge_prob + as.factor(flake_preference) + 
                         mu + as.factor(size_preference) + as.factor(strict_selection) +
                         min_suitable_flake_size, 
                       data = many.tech, family = binomial)
summary(end.many.tech.fit)

plot_summs(mid.many.tech.fit, end.many.tech.fit, model.names = c("Middle of model run", "End of model run"), 
           scale = T)
#significant values do not overlap 0 on estimates
export_summs(mid.many.tech.fit, end.many.tech.fit, model.names = c("Middle of model run", "End of model run"), 
             scale = T, error_format = "[{conf.low}, {conf.high}]")


