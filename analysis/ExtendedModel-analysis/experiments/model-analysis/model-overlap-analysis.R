### Analysis of model outcomes by mobility

if(!require(emmeans)){install.packages("emmeans", repos = "https://cran.wustl.edu/")}
if(!require(jtools)){install.packages("jtools", repos = "https://cran.wustl.edu/")}
if(!require(pscl)){install.packages("pscl", repos = "https://cran.wustl.edu/")}
if(!require(MASS)){install.packages("MASS", repos = "https://cran.wustl.edu/")}
if(!require(rstatix)){install.packages("rstatix", repos = "https://cran.wustl.edu/")}
if(!require(sandwich)){install.packages("sandwich", repos = "https://cran.wustl.edu/")}
if(!require(faux)){install.packages("faux", repos = "https://cran.wustl.edu/")}
if(!require(AICcmodavg)){install.packages("AICcmodavg", repos = "https://cran.wustl.edu/")}

library(tidyverse)
library(ggthemes)
library(betareg)
library(lmtest)
library(rcompanion)
library(emmeans)
library(jtools)
library(pscl)
library(MASS)
library(rstatix)
library(sandwich)
library(faux)
library(AICcmodavg)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")
grouping_params = c("mu", "overlap")

exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")

two.tech = alldata[which(alldata$overlap == 1),]
multi.tech = alldata[which(alldata$overlap == 2),]

rm(alldata)

#### Recycling intensity regressions ####

get_beta_fit = function(overlap.data) {
  overlap.data.br = overlap.data[,c(parameters, "total.RI")] 
  overlap.data.br$total.RI = ifelse(overlap.data.br$total.RI == 1, overlap.data.br$total.RI - 0.00001, overlap.data.br$total.RI)
  overlap.data.br$total.RI = ifelse(overlap.data.br$total.RI == 0, overlap.data.br$total.RI + 0.00001, overlap.data.br$total.RI)
  
  overlap.data.br$norm.RI = beta2norm(overlap.data.br$total.RI)
  
  # beta distribution of recycling intensity
  fit1 = betareg(total.RI ~ ., data =overlap.data.br[,c(1:11, 12)])
  coeftest(fit1, vcov = sandwich)
  
  fit1.coef = exp(coeftest(fit1, vcov = sandwich)[,1])
  fit1.pval = as.numeric(coeftest(fit1, vcov = sandwich)[,4])
  fit1.terms = rownames(coeftest(fit1, vcov = sandwich))
  fit1.coef.high = exp(coefci(fit1, vcov = sandwich)[,2])
  fit1.coef.low = exp(coefci(fit1, vcov = sandwich)[,1])
  
  fit1.df = as.data.frame(cbind(fit1.terms, fit1.coef, fit1.coef.low, fit1.coef.high, fit1.pval))
  colnames(fit1.df) = c("terms", "coef","conf.low", "conf.high", "pval")
  fit1.df$pval = as.numeric(fit1.df$pval)
  fit1.df$coef = as.numeric(fit1.df$coef)
  fit1.df$conf.low = as.numeric(fit1.df$conf.low)
  fit1.df$conf.high = as.numeric(fit1.df$conf.high)
  fit1.df$signf = ifelse(fit1.df$pval <= 0.05, "signif.", "not signif.")
  rownames(fit1.df) = seq(1,13)
  
  return(fit1.df)
  
}


two.tech.br = get_beta_fit(two.tech)
two.tech.br$overlap = 1
multi.tech.br = get_beta_fit(multi.tech)
multi.tech.br$overlap = 2


overlap.fits = rbind(two.tech.br, multi.tech.br)
overlap.fits = overlap.fits[which(overlap.fits$terms != "(phi)"),]


ggsave(filename = "beta-regs-by-overlap.tif",
       ggplot(mu.fits, aes(x = terms, y = coef, shape = signf, group = as.factor(mu), color = as.factor(mu))) +
         geom_hline(yintercept = 1 ,color = "grey", linetype = 2) +
         geom_point(position = position_dodge(width = 0.5)) +
         geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
         coord_flip(), 
       dpi = 300
)

