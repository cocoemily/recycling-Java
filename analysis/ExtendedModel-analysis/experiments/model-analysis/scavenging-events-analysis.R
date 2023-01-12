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

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "size_preference", "flake_preference","min_suitable_flake_size", "strict_selection")

outputs = colnames(alldata[c(22:34)])
step.outputs = outputs[1:5]

scvg.data = alldata[c(parameters, grouping_params, "model_year", "num.scav.events" )]
rm(alldata)


summary(scvg.data$num.scav.events)

print(scvg.data[which(scvg.data$num.scav.events > 1000)],)


# flake.selection = scvg.data[which(scvg.data$flake_preference == TRUE),]
# nodule.selection = scvg.data[which(scvg.data$flake_preference == FALSE),]
# 
# ##scavenging events are zero-inflated negative binomial distribution
# 
# mu.1 = scvg.data[which(scvg.data$mu == 1),]
# mu.2 = scvg.data[which(scvg.data$mu == 2),]
# mu.3 = scvg.data[which(scvg.data$mu == 3),]
# ##hold overlap constant
# mu.1 = mu.1[which(mu.1$overlap == 1),]
# mu.2 = mu.2[which(mu.2$overlap == 1),]
# mu.3 = mu.3[which(mu.3$overlap == 1),]


# fit1 = zeroinfl(num.scav.events ~ ., data = mu.1[c(1:6,9:15)])
# fit2 = zeroinfl(num.scav.events ~ ., data = mu.2[c(1:6,9:15)])
# fit3 = zeroinfl(num.scav.events ~ ., data = mu.3[c(1:6,9:15)])
# 
# ggsave(filename = "scavenging-event-regressions-by-mu.png",
#   plot_summs(fit1, fit2, fit3, model.names = c("mu = 1", "mu = 2", "mu = 3")), 
#   dpi = 300
# )


