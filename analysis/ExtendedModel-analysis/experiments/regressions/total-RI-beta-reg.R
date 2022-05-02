library(rmarkdown)
library(tidyverse)
library(data.table)
library(readr)
library(ggpubr)
library(betareg)
library(lmtest)
library(MASS)
library(bbmle)
library(pscl)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")


#alldata = read_csv("~/eclipse-workspace/recycling-Java/output/sub_model.csv")

alldata = alldata[alldata$size != "size",]

alldata = alldata[!is.na(alldata$max_artifact_carry),]

## total.RI -- beta regression
distRI = (alldata %>% mutate(s.total.RI = ifelse(total.RI == 0, (total.RI + 0.0001), total.RI)) %>% mutate(s.total.RI = ifelse(s.total.RI == 1, (s.total.RI - 0.0001), s.total.RI)))[!is.na(alldata$total.RI) & !is.nan(alldata$total.RI),]
rm(alldata)

bRI = betareg(s.total.RI ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection , data=distRI)

png(filename = "total-RI-beta-reg.png")
qqnorm(residuals(bRI, type = "sweighted2"))
qqline(residuals(bRI, type = "sweighted2"))
dev.off()

coeftest(bRI)
