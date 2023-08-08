#### Behavior trend figures
library(tidyverse)
library(ggpubr)
library(ggthemes)

theme_set(theme_bw())

load("../results/graph-objects/ri-mu.rdata")
ri.mplot = mplot +
  theme(legend.position = "bottom")
load("../results/graph-objects/ri-overlap.rdata")
ri.oplot = oplot +
  theme(legend.position = "bottom")
load("../results/graph-objects/ri-selection.rdata")
ri.splot = splot +
  theme(legend.position = "bottom")

load("../results/graph-objects/recycling-events-mu.rdata")
re.mplot = mplot +
  ylim(0, 10) +
  theme(legend.position = "bottom")
load("../results/graph-objects/recycling-events-overlap.rdata")
re.oplot = oplot +
  ylim(0, 10) +
  theme(legend.position = "bottom")
load("../results/graph-objects/recycling-events-selection.rdata")
re.splot = splot +
  ylim(0, 10) +
  theme(legend.position = "bottom")

# plot(ri.mplot)
# plot(ri.oplot)
# plot(ri.splot)
# 
# plot(re.mplot)
# plot(re.oplot)
# plot(re.splot)

grid = ggarrange(
  ri.oplot, re.oplot, 
  ri.mplot, re.mplot, 
  ri.splot, re.splot,
  ncol = 2, nrow = 3, labels = "AUTO"
)

ggsave(filename = "../figures/recycling-trends.tiff", 
       plot = grid, 
       dpi = 300, width = 10, height = 13)
