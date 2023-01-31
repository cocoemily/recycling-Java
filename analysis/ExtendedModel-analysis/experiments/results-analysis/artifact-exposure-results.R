##ANALYSIS OF ARTIFACT EXPOSURE

library(tidyverse)
library(ggthemes)
library(jtools)
library(ggpubr)

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
  exposure.plot$time = factor(exposure.plot$time, levels = c("mid.conf.val", "end.conf.val"))
  exposure.plot$signif = factor(exposure.plot$signif, levels = c("TRUE", "FALSE"))
  
  
  flake.labs = c("flake preference", "nodule preference")
  names(flake.labs) = c("TRUE", "FALSE")
  size.labs = c("size preference", "no size preference")
  names(size.labs) = c("TRUE", "FALSE")
  strict.labs = c("strict selection", "no strict selection")
  names(strict.labs) = c("TRUE", "FALSE")
  
  ggplot(exposure.plot) +
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

plot_exposure_counts(two.tech)
plot_exposure_counts(many.tech)

grid = ggarrange(plot_exposure_counts(two.tech), plot_exposure_counts(many.tech), 
          common.legend = T, legend = "bottom", labels = "AUTO")

ggsave(filename = "../figures/artifact-exposure-results.tiff", 
       plot = grid, 
       dpi = 300, width = 8)




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

plot_exposure_counts_by_parameters = function (exposure.data) {
  exposure.plot = exposure.data %>% 
    gather(key = "time", value = "signif", mid.conf.val, end.conf.val)
  exposure.plot$time = factor(exposure.plot$time, levels = c("mid.conf.val", "end.conf.val"))
  exposure.plot$signif = factor(exposure.plot$signif, levels = c("TRUE", "FALSE"))
  
  exposure.plot = exposure.plot %>%
    filter(time == "end.conf.val") %>%
    filter(strict_selection == TRUE) %>%
    filter(flake_preference == TRUE)
  
  bp.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
  names(bp.labs) = c("0.25", "0.5", "0.75")
  sp.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
  names(sp.labs) = c("0.25", "0.5", "0.75")
  size.labs = c("size preference", "no size preference")
  names(size.labs) = c("TRUE", "FALSE")
  msfs.labs = c("min. selectable flake size: 1", "min. selectable flake size: 2")
  names(msfs.labs) = c(1, 2)
  
  p1 = ggplot(exposure.plot) +
    geom_bar(aes(x = mu, fill = signif), position = "dodge2") +
    scale_fill_brewer(palette = "Set2", na.value = "grey") +
    facet_grid(scavenge_prob ~ blank_prob, labeller = labeller(blank_prob = bp.labs, 
                                                    scavenge_prob = sp.labs)) + 
    labs(fill = "significantly greater?") +
    theme(legend.key.size = unit(0.25, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          strip.text = element_text(size = 6))
  
  p2 = ggplot(exposure.plot) +
    geom_bar(aes(x = max_flake_size, fill = signif), position = "dodge2") +
    scale_fill_brewer(palette = "Set2", na.value = "grey") +
    facet_grid(size_preference ~ min_suitable_flake_size, 
               labeller = labeller(size_preference =size.labs,
                        min_suitable_flake_size = msfs.labs)) + 
    labs(fill = "significantly greater?", 
         x = "maximum flake size") +
    scale_x_continuous(breaks = c(1, 2)) +
    theme(legend.key.size = unit(0.25, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          strip.text = element_text(size = 6))
  
  ggarrange(p1, p2, labels = "AUTO", common.legend = T)
}

plot_exposure_counts_by_parameters(two.tech)
ggsave(filename = "../figures/TWO-TECH_exposure-by-parameters.tiff",
       plot_exposure_counts_by_parameters(two.tech), 
       dpi = 300, width = 8)
plot_exposure_counts_by_parameters(many.tech)
ggsave(filename = "../figures/MANY-TECH_exposure-by-parameters.tiff",
       plot_exposure_counts_by_parameters(many.tech), 
       dpi = 300, width = 8)




# plot_summs(test2, test3, model.names = c("Two technologies", "Many technologies"))
# export_summs(test2, test3, scale = T, error_format = "[{conf.low}, {conf.high}]", 
#              model.names = c("Two technologies", "Many technologies"))
