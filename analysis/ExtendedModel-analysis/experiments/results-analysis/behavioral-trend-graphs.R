#### Behavior trend figures
library(tidyverse)
library(ggpubr)
library(ggthemes)

theme_set(theme_bw())

load("../results/model-trend-figure/blanks-mu.rdata")
blank.mplot = mplot +
  ylim(0, 12)
load("../results/model-trend-figure/blanks-overlap.rdata")
blank.oplot = oplot
load("../results/model-trend-figure/blanks-selection.rdata")
blank.splot = splot +
  ylim(0, 12)

load("../results/model-trend-figure/discard-mu.rdata")
discard.mplot = mplot +
  ylim(0, 12)
load("../results/model-trend-figure/discard-overlap.rdata")
discard.oplot = oplot
load("../results/model-trend-figure/discard-selection.rdata")
discard.splot = splot +
  ylim(0, 12)

load("../results/model-trend-figure/retouch-mu.rdata")
retouch.mplot = mplot +
  ylim(0, 12)
load("../results/model-trend-figure/retouch-overlap.rdata")
retouch.oplot = oplot
load("../results/model-trend-figure/retouch-selection.rdata")
retouch.splot = splot +
  ylim(0, 12)

load("../results/model-trend-figure/scavenge-mu.rdata")
scavenge.mplot = mplot +
  ylim(0, 12)
load("../results/model-trend-figure/scavenge-overlap.rdata")
scavenge.oplot = oplot
load("../results/model-trend-figure/scavenge-selection.rdata")
scavenge.splot = splot +
  ylim(0, 12)

mu.plot = ggarrange(
  blank.mplot, discard.mplot, scavenge.mplot, retouch.mplot, 
  ncol = 4, labels = "AUTO", common.legend = T, legend = "bottom"
)
#plot(mu.plot)
ggsave(filename = "../figures/behavior-trends-by-mu.tiff", 
       plot = mu.plot, 
       dpi = 300, width = 12, height = 5)

sel.plot = ggarrange(
  blank.splot, discard.splot, scavenge.splot, retouch.splot, 
  ncol = 4, labels = "AUTO", common.legend = T, legend = "bottom"
)
plot(sel.plot)
ggsave(filename = "../figures/behavior-trends-by-selection.tiff", 
       plot = sel.plot, 
       dpi = 300, width = 12, height = 5)
