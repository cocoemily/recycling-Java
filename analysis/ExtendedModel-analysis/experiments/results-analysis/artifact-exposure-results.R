##ANALYSIS OF ARTIFACT EXPOSURE

library(tidyverse)
library(ggthemes)
library(jtools)
library(ggpubr)
library(Dict)

theme_set(theme_bw())

exposure.data = read_csv("~/eclipse-workspace/recycling-Java/results/artifact-exposure-Wilcoxon-results.csv")

##recycled.signif.greater = whether (TRUE) or not (FALSE) the recycled artifacts 
## have ~significantly~ greater initial discard dates than the non-recycled artifacts

two.tech = exposure.data %>% filter(overlap == 1)
many.tech = exposure.data %>% filter(overlap == 2)


plot_exposure_counts = function (exposure.data) {
  
  flake.labs = c("flake preference", "nodule preference")
  names(flake.labs) = c("TRUE", "FALSE")
  size.labs = c("size preference", "no size preference")
  names(size.labs) = c("TRUE", "FALSE")
  strict.labs = c("strict selection", "no strict selection")
  names(strict.labs) = c("TRUE", "FALSE")
  
  ggplot(exposure.data) +
    geom_bar(aes(x = recycled.signif.greater, fill = recycled.signif.greater), position = position_dodge2(preserve = "single")) +
    facet_grid(flake_preference + size_preference ~ strict_selection , 
               labeller = labeller(flake_preference = flake.labs, 
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs)) +
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


####exposure by parameters ####
terms_dict = dict(
  "mu" = "mu",
  "strict_selectionTRUE" = "strict selection: TRUE",
  "size_preferenceTRUE" = "size preference: TRUE",
  "flake_preferenceTRUE" = "flake preference: TRUE",
  "scavenge_prob" = "scavenging probability",
  "min_suitable_flake_size" = "min. selectable flake size",
  "max_use_intensity" = "max. use intensity",
  "max_flake_size" = "max. flake size",
  "max_artifact_carry" = "max. artifact carry",
  "blank_prob" = "blank probability",
  "(Intercept)" = "(intercept)",
  "scavenge_prob:blank_prob" = "blank probability:scavenging probability",
  "max_flake_size:min_suitable_flake_size" = "max. flake size:min. selectable flake size",
  .class = "character", 
  .overwrite = FALSE
)
term_levels = c(
  "(intercept)",
  "mu", 
  "blank probability", 
  "scavenging probability", 
  "max. artifact carry", 
  "max. use intensity", 
  "max. flake size", 
  "min. selectable flake size", 
  "flake preference: TRUE", 
  "size preference: TRUE", 
  "strict selection: TRUE", 
  "blank probability:scavenging probability",
  "max. flake size:min. selectable flake size"
)

tfit1 = glm(end.conf.val ~ ., two.tech[,c(2:4,6:7,9:13,15)], family = "binomial")
summary(tfit1)
tfit1.df = tidy(tfit1)
tfit1.df$tech = "two technologies"

mfit1 = glm(end.conf.val ~ ., many.tech[,c(2:4,6:7,9:13,15)], family = "binomial")
summary(mfit1)
mfit1.df = tidy(mfit1)
mfit1.df$tech = "many technologies"

df = rbind(tfit1.df, mfit1.df)
df$term_clean = ""
for(i in 1:nrow(df)) {
  df$term_clean[i] = terms_dict[df$term[i]]
}
df$term_clean = factor(df$term_clean, levels = term_levels)
df$signif = ifelse(df$p.value < 0.05, TRUE, FALSE)
df$lower = df$estimate - df$std.error
df$upper = df$estimate + df$std.error

ggplot(df, aes(y = estimate, x = term_clean, group = tech, color = tech, shape = signif)) +
  geom_hline(aes(yintercept = 0), color = I("red"), linetype = "dashed") +
  #geom_linerange(df %>% filter(signif == TRUE), mapping = aes(ymin = lower, ymax = upper), position = position_dodge2(width = 0.5)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  scale_color_colorblind() +
  scale_shape_manual(values = c(4, 19)) + 
  coord_flip() +
  labs(x = "terms", shape = "significant?", color = "number of technologies")


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
    geom_bar(aes(x = mu, fill = signif), position = position_dodge2(preserve = "single")) +
    scale_fill_brewer(palette = "Set2", na.value = "grey") +
    facet_grid(scavenge_prob ~ blank_prob, labeller = labeller(blank_prob = bp.labs, 
                                                    scavenge_prob = sp.labs)) + 
    labs(fill = "significantly greater?") +
    theme(legend.key.size = unit(0.25, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          strip.text = element_text(size = 6))
  
  p2 = ggplot(exposure.plot) +
    geom_bar(aes(x = max_flake_size, fill = signif), position = position_dodge2(preserve = "single")) +
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
ggsave(filename = "../figures/supplementary-figures/TWO-TECH_exposure-by-parameters.tiff",
       plot_exposure_counts_by_parameters(two.tech), 
       dpi = 300, width = 8)
plot_exposure_counts_by_parameters(many.tech)
ggsave(filename = "../figures/supplementary-figures/MANY-TECH_exposure-by-parameters.tiff",
       plot_exposure_counts_by_parameters(many.tech), 
       dpi = 300, width = 8)


plot_exposure_counts_by_probs = function (exposure.data) {
  exposure.plot = exposure.data %>% 
    gather(key = "time", value = "signif", mid.conf.val, end.conf.val)
  exposure.plot$time = factor(exposure.plot$time, levels = c("mid.conf.val", "end.conf.val"))
  exposure.plot$signif = factor(exposure.plot$signif, levels = c("TRUE", "FALSE"))
  
  
  blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
  names(blank.labs) = c("0.25", "0.5", "0.75")
  scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
  names(scvg.labs) = c("0.25", "0.5", "0.75")
  
  ggplot(exposure.plot) +
    geom_bar(aes(x = time, fill = signif), position = "dodge2") +
    facet_grid(blank_prob ~ scavenge_prob, 
               labeller = labeller(blank_prob = blank.labs, 
                                   scavenge_prob = scvg.labs)) +
    scale_x_discrete(labels = c("middle", "end")) +
    scale_fill_brewer(palette = "Set2", na.value = "grey") +
    labs(fill = "significantly greater?") +
    theme(legend.key.size = unit(0.25, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8), 
          strip.text = element_text(size = 8))
}

plot_exposure_counts_by_probs(two.tech)
plot_exposure_counts_by_probs(many.tech)
