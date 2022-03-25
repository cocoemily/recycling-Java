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

## num.scav.events -- zero inflated negative binomial vs negative binomial
distSE = alldata[!is.na(alldata$num.scav.events) & !is.nan(alldata$num.scav.events),]

znbSE = zeroinfl(num.scav.events ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = distSE, dist = "negbin")

png(filename = "scav-events-znb-reg.png")
qqnorm(residuals(znbSE))
qqline(residuals(znbSE))
dev.off()

coeftest(znbTD)

nbSE = glm.nb(num.scav.events ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = distSE)

png(filename = "scav-events-nb-reg.png")
qqnorm(residuals(nbSE))
qqline(residuals(nbSE))
dev.off()

coeftest(nbSE)

AICtab(znbSE, nbSE, base = T, weights = T)