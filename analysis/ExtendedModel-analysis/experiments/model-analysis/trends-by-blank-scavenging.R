### Analysis of model outcomes over time by scavenging and blank probabilities

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

# alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
# alldata = alldata[alldata$size != "size",]
# alldata = alldata[!is.na(alldata$max_artifact_carry),]
# 
# two.tech = alldata[which(alldata$overlap == 1),]
# multi.tech = alldata[which(alldata$overlap == 2),]
# 
# rm(alldata)

blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")

load("trends-by-scavenge-blank-probs/ri-overlap.rdata")
p2 = p1
load("trends-by-scavenge-blank-probs/re-overlap.rdata")
load("trends-by-scavenge-blank-probs/ri-mu.rdata")
load("trends-by-scavenge-blank-probs/re-mu.rdata")
load("trends-by-scavenge-blank-probs/ri-flake.rdata")
load("trends-by-scavenge-blank-probs/re-flake.rdata")
load("trends-by-scavenge-blank-probs/ri-size.rdata")
load("trends-by-scavenge-blank-probs/re-size.rdata")
#load("trends-by-scavenge-blank-probs/ri-strict.rdata")
#load("trends-by-scavenge-blank-probs/re-strict.rdata")


rgrid = ggarrange(p2, p1, mplot1, mplot2, splot1, splot2, ssplot1, ssplot2,
                  nrow = 2, ncol = 4, common.legend = T, 
                  legend = "bottom", labels = "AUTO")
ggsave(filename = "recycling-trends-by-probs.tiff", rgrid,
       dpi = 300, width = 12, height = 18)
