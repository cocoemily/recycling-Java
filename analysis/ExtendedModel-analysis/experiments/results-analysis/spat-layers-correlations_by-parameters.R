library(tidyverse)
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
    "ri.obj.cnt.cor" = "correlation between recycling intensity and object count",
    "ri.cr.cor" = "correlation between recycling intensity and cortex ratio",
    "ri.num.disc.cor" = "correlation between recycling intensity and discards",
    "ri.num.scvg.cor" = "correlation between recycling intensity and scavenging events",
    "ri.num.enct.cor" = "correlation between recycling intensity and encounters",
    "ri.num.ret.cor" = "correlation between recycling intensity and retouches",
    .class = "character", 
    .overwrite = FALSE
  )
  
  data$mu = factor(data$mu, levels = c(1, 2, 3))
  
  data2 = data %>% filter(overlap == 1)
  
  ggplot(data2) +
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

endgrid = ggarrange(plot_recycling_correlations(sub.layer.cor, cor.names[2]),
                     plot_recycling_correlations(sub.layer.cor, cor.names[3]),
                     plot_recycling_correlations(sub.layer.cor, cor.names[4]),
                     plot_recycling_correlations(sub.layer.cor, cor.names[5]),
                     plot_recycling_correlations(sub.layer.cor, cor.names[6]),
                     plot_recycling_correlations(sub.layer.cor, cor.names[7]), 
                     ncol = 2, nrow = 3, common.legend = T, legend = "bottom", labels = "AUTO")
plot(endgrid)

ggsave(filename = "../figures/supplementary-figures/RI-correlations_by-parameters.tiff", 
       endgrid, dpi = 300, width = 12, height = 10)
