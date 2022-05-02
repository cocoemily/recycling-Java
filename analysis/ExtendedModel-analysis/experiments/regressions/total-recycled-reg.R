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

## total.recycled -- zero inflated negative binomial vs negative binomial
distTR = alldata[!is.na(alldata$total.recycled) & !is.nan(alldata$total.recycled),]
rm(alldata)

znbTR = zeroinfl(total.recycled ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = distTR, dist = "negbin")

png(filename = "total-recycled-znb-reg.png")
qqnorm(residuals(znbTR))
qqline(residuals(znbTR))
dev.off()

coeftest(znbTR)

nbTD = glm.nb(total.recycled ~ model_year + max_use_intensity  + max_artifact_carry + max_flake_size + max_nodules_size + blank_prob + scavenge_prob + overlap + mu + size_preference + flake_preference + min_suitable_flake_size + min_suitable_nodule_size + strict_selection, data = distTR)

png(filename = "total-recycled-nb-reg.png")
qqnorm(residuals(nbTR))
qqline(residuals(nbTR))
dev.off()

coeftest(nbTR)

AICtab(znbTR, nbTR, base = T, weights = T)