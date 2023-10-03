library(tidyverse)
library(ggthemes)
library(ggpubr)
library(Dict)

theme_set(theme_bw())

layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-other-cor-output.csv")
parameters = colnames(layer.cor[,3:15])
cor.names = colnames(layer.cor[,16:25])

layer.cor = layer.cor %>% group_by(row, col) %>% 
  mutate(square = cur_group_id())


plot_other_correlations = function(data, correlation) {
  size.labs = c("size preference", "no size preference")
  names(size.labs) = c("TRUE", "FALSE")
  flake.labs = c("flake preference", "nodule preference")
  names(flake.labs) = c("TRUE", "FALSE")
  strict.labs = c("strict selection", "non-strict selection")
  names(strict.labs) = c("TRUE", "FALSE")
  tech.labs = c("two technology types", "many technology types")
  names(tech.labs) = c("1", "2")
  occup.labs = c("100 agents", "200 agents")
  names(occup.labs) = c(100, 200)
  
  cor_dict = dict(
    "obj.cnt.num.disc.cor" = "correlation between object count and discard events",
    "obj.cnt.num.scvg.cor" = "correlation between object count and scavenging events",
    "obj.cnt.num.enct.cor" = "correlation between object count and encounters",
    "obj.cnt.num.ret.cor" = "correlation between object count and retouches",
    "num.disc.num.scvg.cor" = "correlation between discard events and scavenging events",
    "num.disc.num.enct.cor" = "correlation between discard events and encounters",
    "num.disc.num.ret.cor" = "correlation between discard events and retouches",
    "num.scvg.num.ret.cor" = "correlation between scavenging events and encounters", #swapped in original script run
    "num.scvg.num.enct.cor" = "correlation between scavenging events and retouches", #swapped in original script run
    "num.enct.num.ret.cor" = "correlation between encounters and retouches",
    .class = "character", 
    .overwrite = FALSE
  )
  
  data$mu = factor(data$mu, levels = c(1, 2, 3))
  
  ggplot(data) +
    geom_boxplot(aes_string(x = "mu", y = correlation, group = "mu", color = "mu")) +
    geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
    facet_grid(num_agents ~ flake_preference + size_preference + strict_selection, 
               labeller = labeller(flake_preference = flake.labs, 
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs, 
                                   num_agents = occup.labs)) +
    labs(y = cor_dict[correlation], x = "\u00b5", color = "\u00b5") +
    scale_color_colorblind() +
    theme(axis.title = element_text(size = 6), strip.text = element_text(size = 6), 
          legend.title = element_text(size = 8))
  
}

sub.layer.cor = layer.cor %>% filter(overlap == 1)


endgrid = ggarrange(plot_other_correlations(sub.layer.cor, cor.names[1]), 
          plot_other_correlations(sub.layer.cor, cor.names[2]),
          plot_other_correlations(sub.layer.cor, cor.names[3]),
          plot_other_correlations(sub.layer.cor, cor.names[4]),
          plot_other_correlations(sub.layer.cor, cor.names[5]),
          plot_other_correlations(sub.layer.cor, cor.names[6]),
          plot_other_correlations(sub.layer.cor, cor.names[7]),
          plot_other_correlations(sub.layer.cor, cor.names[8]),
          plot_other_correlations(sub.layer.cor, cor.names[9]), 
          plot_other_correlations(sub.layer.cor, cor.names[10]), 
          ncol = 2, nrow = 5, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave(filename = "../figures/supplementary-figures/other-correlations_by-parameters.tiff", 
       endgrid, dpi = 300, width = 12, height = 15)

blank.labs = c("blank probability: 0.25", "blank probability: 0.5", "blank probability: 0.75")
names(blank.labs) = c(0.25, 0.5, 0.75)
scavenge.labs = c("scavenging probability: 0.25", "scavenging probability: 0.5", "scavenging probability: 0.75")
names(scavenge.labs) = c(0.25, 0.5, 0.75)

ret.enct.cor = read_csv("~/eclipse-workspace/recycling-Java/results/retouch-encounter-correlation-results.csv")
re.plot = ggplot(ret.enct.cor %>% filter(overlap == 1) %>% filter(num_agents == 100)) +
  geom_boxplot(aes(x = as.factor(mu), y = ret.prop.num.enct.cor, group = as.factor(mu), color = as.factor(mu))) +
  geom_hline(aes(yintercept = 0), color = "red") +
  facet_grid(blank_prob ~ scavenge_prob, 
             labeller = labeller(blank_prob = blank.labs,
                                 scavenge_prob = scavenge.labs)) +
  scale_color_colorblind() +
  labs(color = "\u00b5", x = "\u00b5", y = "correlation between encounters and proportion retouched artifacts") +
  theme(axis.title = element_text(size = 7), strip.text = element_text(size = 7), 
        legend.title = element_text(size = 8))
plot(re.plot)

ggsave(filename = "../figures/supplementary-figures/correlation_retouched-prop_encounters.tiff", 
       re.plot, 
       dpi = 300, width = 8, height = 7)


