library(tidyverse)
library(ggthemes)
library(ggpubr)
library(Dict)

layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-other-gridded-cor.csv")
parameters = colnames(layer.cor[,4:15])
cor.names = colnames(layer.cor[,16:25])

layer.cor1 = layer.cor[-c(26:28)]
layer.cor = layer.cor1  %>% group_by(row, col) %>% 
  mutate(square = cur_group_id())

row.col = unique(layer.cor %>% dplyr::select(row, col, square))

layer.cor.end = layer.cor[which(layer.cor$time == "end"),]
layer.cor.mid = layer.cor[which(layer.cor$time == "mid"),]


# long.end = layer.cor.end %>%
#   pivot_longer(cor.names, names_to = "cor", values_to = "value")
# 
# ggplot(long.end) +
#   geom_boxplot(aes(x = cor, y = value, group = cor, color = cor)) +
#   facet_grid(row ~ col, labeller = label_both)

plot_other_correlations = function(data, correlation) {
  size.labs = c("size preference", "no size preference")
  names(size.labs) = c("TRUE", "FALSE")
  flake.labs = c("flake preference", "nodule preference")
  names(flake.labs) = c("TRUE", "FALSE")
  strict.labs = c("strict selection", "non-strict selection")
  names(strict.labs) = c("TRUE", "FALSE")
  tech.labs = c("two technology types", "many technology types")
  names(tech.labs) = c("1", "2")
  
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
    facet_grid(overlap ~ flake_preference + size_preference + strict_selection, 
               labeller = labeller(flake_preference = flake.labs, 
                                   size_preference = size.labs, 
                                   strict_selection = strict.labs, 
                                   overlap = tech.labs)) +
    labs(y = cor_dict[correlation]) +
    scale_color_colorblind() +
    theme(axis.title = element_text(size = 8), strip.text = element_text(size = 7), 
          legend.title = element_text(size = 8))
  
}

#plot_other_correlations(layer.cor.end, cor.names[1])
p1 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[1]),
          plot_other_correlations(layer.cor.end, cor.names[1]), 
          common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[1], ".tiff"), 
       p1 , dpi = 300, width = 8, height = 8)

p2 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[2]),
               plot_other_correlations(layer.cor.end, cor.names[2]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[2], ".tiff"), 
       p2 , dpi = 300, width = 8, height = 8)

p3 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[3]),
               plot_other_correlations(layer.cor.end, cor.names[3]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[3], ".tiff"), 
       p3 , dpi = 300, width = 8, height = 8)

p4 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[4]),
               plot_other_correlations(layer.cor.end, cor.names[4]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[4], ".tiff"), 
       p4 , dpi = 300, width = 8, height = 8)

p5 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[5]),
               plot_other_correlations(layer.cor.end, cor.names[5]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[5], ".tiff"), 
       p5 , dpi = 300, width = 8, height = 8)

p6 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[6]),
               plot_other_correlations(layer.cor.end, cor.names[6]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[6], ".tiff"), 
       p6 , dpi = 300, width = 8, height = 8)

p7 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[7]),
               plot_other_correlations(layer.cor.end, cor.names[7]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[7], ".tiff"), 
       p7 , dpi = 300, width = 8, height = 8)

p8 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[8]),
               plot_other_correlations(layer.cor.end, cor.names[8]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[8], ".tiff"), 
       p8 , dpi = 300, width = 8, height = 8)

p9 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[9]),
               plot_other_correlations(layer.cor.end, cor.names[9]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[9], ".tiff"), 
       p9 , dpi = 300, width = 8, height = 8)

p10 = ggarrange(plot_other_correlations(layer.cor.mid, cor.names[10]),
               plot_other_correlations(layer.cor.end, cor.names[10]), 
               common.legend = T, labels = "AUTO", nrow = 2, legend = "right")

ggsave(filename = paste0("../figures/supplementary-figures/", cor.names[10], ".tiff"), 
       p10 , dpi = 300, width = 8, height = 8)

