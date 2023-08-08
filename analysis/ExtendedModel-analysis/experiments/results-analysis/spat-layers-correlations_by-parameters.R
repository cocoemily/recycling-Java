library(tidyverse)
library(ggpubr)
library(ggthemes)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(rstatix)

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")
parameters = colnames(layer.cor[,3:15])
cor.names = colnames(layer.cor[,16:22])

cor.long = layer.cor %>% 
  pivot_longer(cols = c(cor.names), names_to = "correlation") %>%
  filter(correlation != "cr.obj.cnt.cor") %>%
  mutate(correlation = factor(correlation, levels = c(
    "ri.obj.cnt.cor", "ri.cr.cor", "ri.num.disc.cor", "ri.num.scvg.cor", "ri.num.enct.cor", "ri.num.ret.cor"
  )))

avg.cor = ggplot(cor.long, aes(x = correlation, y = value, group = correlation, fill = correlation)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(y = "correlation coefficient") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")
plot(avg.cor)

ggsave(filename = "../figures/average-correlations.tiff", plot = avg.cor, 
       dpi = 300, width = 5, height = 4)


####supplementary figure by parameters ####
sub.layer.cor = layer.cor %>% filter(overlap == 1)

plot_recycling_correlations = function(data, correlation) {
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
    "ri.obj.cnt.cor" = "correlation between RI and object count",
    "ri.cr.cor" = "correlation between RI and cortex ratio",
    "ri.num.disc.cor" = "correlation between RI and discards",
    "ri.num.scvg.cor" = "correlation between RI and scavenging events",
    "ri.num.enct.cor" = "correlation between RI and encounters",
    "ri.num.ret.cor" = "correlation between RI and retouches",
    .class = "character", 
    .overwrite = FALSE
  )
  
  data$mu = factor(data$mu, levels = c(1, 2, 3))
  
  ggplot(data) +
    geom_boxplot(aes_string(x = "strict_selection", y = correlation, group = "strict_selection", color = "strict_selection")) +
    geom_hline(aes(yintercept = 0), color = "red") +
    facet_grid(overlap ~ size_preference + flake_preference, 
               labeller = labeller(flake_preference = flake.labs,
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs, 
                                   overlap = tech.labs)) +
    labs(y = cor_dict[correlation], x = "strict selection") +
    scale_color_colorblind() +
    theme(axis.title = element_text(size = 10), strip.text = element_text(size = 8), 
          legend.title = element_text(size = 10))
  
}

endgrid = ggarrange(plot_recycling_correlations(layer.cor, cor.names[2]),
                     plot_recycling_correlations(layer.cor, cor.names[3]), 
                     ncol = 2, nrow = 1, common.legend = T, legend = "bottom", labels = "AUTO")
plot(endgrid)

ggsave(filename = "../figures/RI-correlations_by-selection.tiff", 
       endgrid, dpi = 300,  width = 10, height = 5)


plot_recycling_correlations_MU = function(data, correlation) {
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
    "ri.obj.cnt.cor" = "correlation between RI and object count",
    "ri.cr.cor" = "correlation between RI and cortex ratio",
    "ri.num.disc.cor" = "correlation between RI and discards",
    "ri.num.scvg.cor" = "correlation between RI and scavenging events",
    "ri.num.enct.cor" = "correlation between RI and encounters",
    "ri.num.ret.cor" = "correlation between RI and retouches",
    .class = "character", 
    .overwrite = FALSE
  )
  
  data$mu = factor(data$mu, levels = c(1, 2, 3))
  
  ggplot(data) +
    geom_boxplot(aes_string(x = "mu", y = correlation, group = "mu", color = "mu")) +
    geom_hline(aes(yintercept = 0), color = "red") +
    facet_grid(overlap ~ num_agents, 
               labeller = labeller(overlap = tech.labs, 
                                   num_agents = occup.labs)) +
    labs(y = cor_dict[correlation]) +
    scale_color_colorblind() +
    theme(axis.title = element_text(size = 10), strip.text = element_text(size = 8), 
          legend.title = element_text(size = 10))
  
}

endgrid = ggarrange(plot_recycling_correlations_MU(layer.cor, cor.names[2]),
                    plot_recycling_correlations_MU(layer.cor, cor.names[3]),
                    plot_recycling_correlations_MU(layer.cor, cor.names[4]),
                    plot_recycling_correlations_MU(layer.cor, cor.names[5]),
                    plot_recycling_correlations_MU(layer.cor, cor.names[6]),
                    plot_recycling_correlations_MU(layer.cor, cor.names[7]), 
                    ncol = 2, nrow = 3, common.legend = T, legend = "bottom", labels = "AUTO")
plot(endgrid)
ggsave(filename = "../figures/RI-correlations_by-movement.tiff", 
       endgrid, dpi = 300, width = 10, height = 12)
