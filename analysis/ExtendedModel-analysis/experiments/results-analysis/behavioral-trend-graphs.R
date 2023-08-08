#### Behavior trend figures
library(tidyverse)
library(ggpubr)
library(ggthemes)

theme_set(theme_bw())

load("../results/graph-objects/blanks-mu.rdata")
blank.mplot = mplot +
  ylim(0, 50)
load("../results/graph-objects/blanks-overlap.rdata")
blank.oplot = oplot +
  ylim(0, 50)
load("../results/graph-objects/blanks-selection.rdata")
blank.splot = splot +
  ylim(0, 50)

load("../results/graph-objects/discard-mu.rdata")
discard.mplot = mplot +
  ylim(0, 50)
load("../results/graph-objects/discard-overlap.rdata")
discard.oplot = oplot +
  ylim(0, 50)
load("../results/graph-objects/discard-selection.rdata")
discard.splot = splot +
  ylim(0, 50)

load("../results/graph-objects/retouch-mu.rdata")
retouch.mplot = mplot  +
  ylim(0, 50)
load("../results/graph-objects/retouch-overlap.rdata")
retouch.oplot = oplot +
  ylim(0, 50)
load("../results/graph-objects/retouch-selection.rdata")
retouch.splot = splot +
  ylim(0, 50)

load("../results/graph-objects/scavenge-mu.rdata")
scavenge.mplot = mplot +
  ylim(0, 50)
load("../results/graph-objects/scavenge-overlap.rdata")
scavenge.oplot = oplot +
  ylim(0, 50)
load("../results/graph-objects/scavenge-selection.rdata")
scavenge.splot = splot +
  ylim(0, 50)

mu.plot = ggarrange(
  blank.mplot, discard.mplot, scavenge.mplot, retouch.mplot, 
  ncol = 2, nrow = 2, labels = "AUTO", common.legend = T, legend = "bottom"
)
plot(mu.plot)
ggsave(filename = "../figures/behavior-trends-by-mu.tiff", 
       plot = mu.plot,
       dpi = 300, width = 9.5, height = 7)

sel.plot = ggarrange(
  blank.splot, discard.splot, scavenge.splot, retouch.splot, 
  ncol = 2, nrow = 2, labels = "AUTO", common.legend = T, legend = "bottom"
)
plot(sel.plot)
ggsave(filename = "../figures/behavior-trends-by-selection.tiff", 
       plot = sel.plot, 
       dpi = 300, width = 9.5, height = 7)

ov.plot = ggarrange(
  blank.oplot, discard.oplot, scavenge.oplot, retouch.oplot, 
  ncol = 2, nrow = 2, labels = "AUTO", common.legend = T, legend = "bottom"
)
plot(ov.plot)

ggsave(filename = "../figures/supplementary-figures/behavior-trends-by-overlap.tiff", 
       plot = ov.plot, 
       dpi = 300, width = 9.5, height = 7)
