### Analysis of model outcomes over time by scavenging and blank probabilities

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

# load("../results/graph-objects/ri-overlap.rdata")
# load("../results/graph-objects/re-overlap.rdata")
load("../results/graph-objects/ri-mu.rdata")
load("../results/graph-objects/re-mu.rdata")
load("../results/graph-objects/ri-flake.rdata")
load("../results/graph-objects/re-flake.rdata")
load("../results/graph-objects/ri-size.rdata")
load("../results/graph-objects/re-size.rdata")
load("../results/graph-objects/ri-strict.rdata")
load("../results/graph-objects/re-strict.rdata")

plot(mplot1 + ylim(0, 0.3))
plot(mplot2 + ylim(0, 30))

plot(splot1 + ylim(0, 0.3))
plot(splot2 + ylim(0, 30))

plot(ssplot1 + ylim(0, 0.3))
plot(ssplot2 + ylim(0, 30))

plot(ssp1 + ylim(0, 0.3))
plot(ssp2 + ylim(0, 30))


rgrid = ggarrange(mplot1 + ylim(0, 0.3) + labs(color = ""), 
                  mplot2 + ylim(0, 30) + labs(color = ""),
                  ncol = 2, common.legend = T, labels = "AUTO")
plot(rgrid)
ggsave(filename = "../figures/recycling-trends_by-probs-mu.tiff", rgrid,
       dpi = 300, width = 12, height = 6)


srgrid = ggarrange(splot1 + ylim(0, 0.3)+ labs(color = ""), 
                  splot2 + ylim(0, 30)+ labs(color = ""),
                  ssplot1 + ylim(0, 0.3)+ labs(color = ""), 
                  ssplot2 + ylim(0, 30)+ labs(color = ""),
                  ssp1 + ylim(0, 0.3)+ labs(color = ""), 
                  ssp2 + ylim(0, 30)+ labs(color = ""),
                  ncol = 2, nrow = 3, legend = "bottom", labels = "AUTO")
plot(srgrid)
ggsave(filename = "../figures/supplementary-figures/recycling-trends_by-probs-selection.tiff", srgrid,
       dpi = 300, width = 12, height = 16)

