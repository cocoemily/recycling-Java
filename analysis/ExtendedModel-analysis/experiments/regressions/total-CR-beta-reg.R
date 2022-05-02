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

## total.CR -- beta regression
distCR = (alldata %>% mutate(s.total.CR = ifelse(total.CR == 0, (total.CR + 0.0001), total.CR)) %>% mutate(s.total.CR = ifelse(s.total.CR == 1, (s.total.CR - 0.0001), s.total.CR)))[!is.na(alldata$total.CR) & !is.nan(alldata$total.CR),]
rm(alldata)

bCR = betareg(s.total.CR ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection , data=distCR)

png(filename = "total-CR-beta-reg.png")
qqnorm(residuals(bCR, type = "sweighted2"))
qqline(residuals(bCR, type = "sweighted2"))
dev.off()

coeftest(bCR)