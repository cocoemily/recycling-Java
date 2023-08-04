library(tidyverse)
library(ggthemes)
library(ggpubr)
library(Dict)

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
    facet_grid(num_agents ~ flake_preference + size_preference + strict_selection, 
               labeller = labeller(flake_preference = flake.labs, 
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs, 
                                   num_agents = occup.labs)) +
    labs(y = cor_dict[correlation]) +
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

