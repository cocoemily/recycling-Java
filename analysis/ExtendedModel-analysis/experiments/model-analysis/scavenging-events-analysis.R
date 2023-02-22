#analyzing scavenging events
if(!require(jtools)){install.packages("jtools", repos = "https://cran.wustl.edu/")}


library(tidyverse)
library(ggthemes)
library(pscl)
library(MASS)
library(boot)
library(jtools)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size", "blank_prob", "scavenge_prob", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")
grouping_params = c("mu", "overlap")

outputs = colnames(alldata[c(21:33)])
step.outputs = outputs[1:5]

scvg.data = alldata[c(parameters, grouping_params, "model_year", "num.scav.events" )]
rm(alldata)

fit1 = zeroinfl(num.scav.events ~ ., data = scvg.data)

ggsave(filename = "scavenging-event-regressions-by-mu.png",
  plot_summs(fit1, model.names = c("mu = 1")),
  dpi = 300
)


