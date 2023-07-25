library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(rstatix)

source("ExtendedModel-analysis/experiments/results-analysis/helper-functions.R")

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")
parameters = colnames(layer.cor[,4:15])
cor.names = colnames(layer.cor[,16:24])
cor.names = cor.names[-length(cor.names)] #removing occupations correlation because encounters = occupations

layer.cor1 = layer.cor[-c(25:27)]
layer.cor = layer.cor1  %>% group_by(row, col) %>% 
  mutate(square = cur_group_id())

row.col = unique(layer.cor %>% dplyr::select(row, col, square))


layer.cor.end = layer.cor[which(layer.cor$time == "end"),]
layer.cor.mid = layer.cor[which(layer.cor$time == "mid"),]

end.cor.long = layer.cor.end %>% 
  pivot_longer(cols = c(cor.names[2], cor.names[3], cor.names[4], cor.names[5], cor.names[6], cor.names[8]), 
               names_to = "correlations") %>%
  mutate(correlations = factor(correlations, levels = c(
    "ri.obj.cnt.cor", "ri.cr.cor", "ri.num.disc.cor", "ri.num.scvg.cor", "ri.num.enct.cor", "ri.num.ret.cor"
  )))
mid.cor.long = layer.cor.mid %>% 
  pivot_longer(cols = c(cor.names[2], cor.names[3], cor.names[4], cor.names[5], cor.names[6], cor.names[8]), 
               names_to = "correlations") %>%
  mutate(correlations = factor(correlations, levels = c(
    "ri.obj.cnt.cor", "ri.cr.cor", "ri.num.disc.cor", "ri.num.scvg.cor", "ri.num.enct.cor", "ri.num.ret.cor"
  )))

cor.long = rbind(mid.cor.long, end.cor.long) %>%
  mutate(time = factor(time, levels = c("mid", "end")))

avg.cor = ggplot(cor.long, aes(x = correlations, y = value, group = interaction(time, correlations), fill = interaction(time, correlations))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0), color = "red") +
  labs(y = "correlation coefficient") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")
plot(avg.cor)

ggsave(filename = "../figures/average-correlations.tiff", plot = avg.cor, 
       dpi = 300, width = 7, height = 4)


data = layer.cor.end
correlation = cor.names[2]

plot_recycling_correlations = function(data, correlation) {
  size.labs = c("size preference", "no size preference")
  names(size.labs) = c("TRUE", "FALSE")
  flake.labs = c("flake preference", "nodule preference")
  names(flake.labs) = c("TRUE", "FALSE")
  strict.labs = c("strict selection", "non-strict selection")
  names(strict.labs) = c("TRUE", "FALSE")
  tech.labs = c("two technology types", "many technology types")
  names(tech.labs) = c("1", "2")
  
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
  
  ggplot(data) +
    geom_boxplot(aes_string(x = "mu", y = correlation, group = "mu", color = "mu")) +
    facet_grid(overlap ~ flake_preference + size_preference + strict_selection, 
               labeller = labeller(flake_preference = flake.labs, 
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs, 
                                   overlap = tech.labs)) +
    labs(y = cor_dict[correlation]) +
    scale_color_colorblind() +
    theme(axis.title = element_text(size = 6), strip.text = element_text(size = 6), 
          legend.title = element_text(size = 8))
  
}

endgrid = ggarrange(plot_recycling_correlations(layer.cor.end, cor.names[2]),
                     plot_recycling_correlations(layer.cor.end, cor.names[3]),
                     plot_recycling_correlations(layer.cor.end, cor.names[4]),
                     plot_recycling_correlations(layer.cor.end, cor.names[5]),
                     plot_recycling_correlations(layer.cor.end, cor.names[6]),
                     plot_recycling_correlations(layer.cor.end, cor.names[8]), 
                     ncol = 2, nrow = 3, common.legend = T, legend = "bottom", labels = "AUTO")
plot(endgrid)

ggsave(filename = "../figures/supplementary-figures/end-grid-RI-cor.tiff", 
       endgrid, dpi = 300, width = 12, height = 10)
