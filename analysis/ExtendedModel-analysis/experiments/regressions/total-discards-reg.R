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

## total.discards -- zero inflated negative binomial
distTD = alldata[!is.na(alldata$total.discards) & !is.nan(alldata$total.discards),]
rm(alldata)

znbTD = zeroinfl(total.discards ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = distTD, dist = "negbin")

png(filename = "total-discards-znb-reg.png")
qqnorm(residuals(znbTD))
qqline(residuals(znbTD))
dev.off()

coeftest(znbTD)

nbTD = glm.nb(total.discards ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = distTD)

png(filename = "total-discards-nb-reg.png")
qqnorm(residuals(nbTD))
qqline(residuals(nbTD))
dev.off()

coeftest(nbTD)

AICtab(znbTD, nbTD, base = T, weights = T)