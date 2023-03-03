### Analysis of model outcomes over time by scavenging and blank probabilities

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

load("trends-by-scavenge-blank-probs/ri-overlap.rdata")
load("trends-by-scavenge-blank-probs/re-overlap.rdata")
load("trends-by-scavenge-blank-probs/ri-mu.rdata")
load("trends-by-scavenge-blank-probs/re-mu.rdata")
load("trends-by-scavenge-blank-probs/ri-flake.rdata")
load("trends-by-scavenge-blank-probs/re-flake.rdata")
load("trends-by-scavenge-blank-probs/ri-size.rdata")
load("trends-by-scavenge-blank-probs/re-size.rdata")
# load("trends-by-scavenge-blank-probs/ri-strict.rdata")
# load("trends-by-scavenge-blank-probs/re-strict.rdata")


rgrid = ggarrange(p1, p2, mplot1, mplot2, splot1, splot2, ssplot1, ssplot2,
                  nrow = 4, ncol = 2, legend = "right", labels = "AUTO")
ggsave(filename = "recycling-trends-by-probs.tiff", rgrid,
       dpi = 300, width = 15, height = 20)

# strictgrid = ggarrange(ssp1, ssp2, common.legend = T, labels = "AUTO", 
#                        ncol = 2, nrow = 1)

