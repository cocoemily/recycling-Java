#### Behavior trend figures
library(tidyverse)
library(ggpubr)
library(ggthemes)

theme_set(theme_bw())

load("../results/graph-objects/ri-mu.rdata")
ri.mplot = mplot
load("../results/graph-objects/ri-overlap.rdata")
ri.oplot = oplot
load("../results/graph-objects/ri-selection.rdata")
ri.splot = splot

# load("../results/graph-objects/discard-mu.rdata")
# discard.mplot = mplot +
#   ylim(0, 12)
# load("../results/graph-objects/discard-overlap.rdata")
# discard.oplot = oplot
# load("../results/graph-objects/discard-selection.rdata")
# discard.splot = splot +
#   ylim(0, 12)


plot(ri.oplot)
plot(ri.splot)

ggsave(filename = "../figures/behavior-trends-by-selection.tiff", 
       plot = sel.plot, 
       dpi = 300, width = 12, height = 5)
