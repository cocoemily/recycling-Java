### Analysis of model outcomes by mobility

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

mu.1 = alldata[which(alldata$mu == 1),]
mu.2 = alldata[which(alldata$mu == 2),]
mu.3 = alldata[which(alldata$mu == 3),]

rm(alldata)

# alldata = alldata %>% group_by(model_year) %>%
#   mutate(RI.lower = min(total.RI, na.rm = T), 
#          RI.upper = max(total.RI, na.rm = T))
# 
# ggsave(filename = "/scratch/ec3307/recycling-Java/results/recycling-intensity-over-time.tif",
#   ggplot(alldata) +
#   geom_linerange(aes(x = model_year, ymin = RI.lower, ymax = RI.upper), alpha = 0.1, size = 0.01) +
#   geom_smooth(aes(x = model_year, y = total.RI)) +
#   facet_grid(overlap~mu, labeller = label_both) +
#   scale_x_reverse(), 
#   dpi = 300
# )

p1 = ggplot(mu.1, aes(x = model_year, y = total.RI)) +
  geom_smooth(color = "grey") +
  facet_grid(~overlap, labeller = label_both) +
  scale_x_reverse()

p2 = ggplot(mu.2, aes(x = model_year, y = total.RI)) +
  geom_smooth(color = "orange") +
  facet_grid(~overlap, labeller = label_both) +
  scale_x_reverse()

p3 = ggplot(mu.3, aes(x = model_year, y = total.RI)) +
  geom_smooth(color = "blue") +
  facet_grid(~overlap, labeller = label_both) +
  scale_x_reverse()

cowplot::plot_grid(p1, p2, p3, labels = "auto")


###need to know distributions to do regressions
#### Recycling intensity regressions ####

get_beta_fit = function(mu.data) {
  mu.br.data = mu.data[,c(parameters, "total.RI")] 
  mu.br.data$total.RI = ifelse(mu.br.data$total.RI == 1, mu.br.data$total.RI - 0.00001, mu.br.data$total.RI)
  mu.br.data$total.RI = ifelse(mu.br.data$total.RI == 0, mu.br.data$total.RI + 0.00001, mu.br.data$total.RI)
  
  # beta distribution of recycling intensity
  fit1 = betareg(total.RI ~ ., data =mu.br.data)
  #joint_tests(fit1)
  # lrtest(fit1)
  # 
  # plot_coefs(fit1)
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


mu1.br = get_beta_fit(mu.1)
mu1.br$mu = 1
mu2.br = get_beta_fit(mu.2)
mu2.br$mu = 2
mu3.br = get_beta_fit(mu.3)
mu3.br$mu = 3

mu.fits = rbind(mu1.br, mu2.br, mu3.br)
mu.fits = mu.fits[which(mu.fits$terms != "(phi)"),]


ggsave(filename = "beta-regs-by-mu.tif",
       ggplot(mu.fits, aes(x = terms, y = coef, shape = signf, group = as.factor(mu), color = as.factor(mu))) +
         geom_hline(yintercept = 1 ,color = "grey", linetype = 2) +
         geom_point(position = position_dodge(width = 0.5)) +
         geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
         coord_flip(), 
       dpi = 300
)

