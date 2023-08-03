library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(rstatix)
library(lme4)
library(merTools)
library(data.table)
library(ggthemes)
library(cowplot)
library(ggpubr)
library(Dict)

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")

parameters = colnames(layer.cor[,3:15])
cor.names = colnames(layer.cor[,16:22])

layer.cor = layer.cor %>%
  group_by(row, col) %>%
  mutate(square = cur_group_id())

summary(layer.cor$ri.obj.cnt.cor)
hist(layer.cor$ri.obj.cnt.cor)


terms_dict = dict(
  "num_agents" = "number of agents",
  "mu" = "mu",
  "strict_selectionTRUE" = "strict selection: TRUE",
  "size_preferenceTRUE" = "size preference: TRUE",
  "flake_preferenceTRUE" = "flake preference: TRUE",
  "scavenge_prob" = "scavenging probability",
  "min_suitable_flake_size" = "min. selectable flake size",
  "max_use_intensity" = "max. use intensity",
  "max_flake_size" = "max. flake size",
  "max_artifact_carry" = "max. artifact carry",
  "blank_prob" = "blank probability",
  "(Intercept)" = "(intercept)",
  "scavenge_prob:blank_prob" = "blank probability:scavenging probability",
  "max_flake_size:min_suitable_flake_size" = "max. flake size:min. selectable flake size",
  .class = "character", 
  .overwrite = FALSE
)
term_levels = c(
  "(intercept)",
  "number of agents",
  "mu", 
  "blank probability", 
  "scavenging probability", 
  "max. artifact carry", 
  "max. use intensity", 
  "max. flake size", 
  "min. selectable flake size", 
  "flake preference: TRUE", 
  "size preference: TRUE", 
  "strict selection: TRUE", 
  "blank probability:scavenging probability",
  "max. flake size:min. selectable flake size"
)

plotFEmult = function(..., facet = FALSE, title = NULL) {
  dots = list(...)
  
  if (!is.null(names(dots))) {
    mods <- dots[names(dots) == ""]
    ex_args <- dots[names(dots) != ""]
  } else {
    mods <- dots
    ex_args <- NULL
  }
  
  for(i in 1:length(ex_args)) {
    data = ex_args[[i]]
    data[, "sd"] <- data[, "sd"] * qnorm(1-((1-0.95)/2))
    data[, "ymax"] <- data[, "median"] + data[, "sd"]
    data[, "ymin"] <- data[, "median"] - data[, "sd"]
    data[, "sig"] <- data[, "ymin"] > 0 | data[, "ymax"] < 0
    hlineInt <- 0
    xvar = "term"
    data$term = as.character(data$term)
    ex_args[[i]] <- data
  }
  
  plot.data = do.call("rbind", ex_args)
  plot.data$group = str_remove(rownames(plot.data), "\\.[0-9]*")
  plot.data$xvar_clean = ""
  for(i in 1:nrow(plot.data)) {
    if(!is.na(plot.data$term[i])) {
      plot.data$xvar_clean[i] = terms_dict[plot.data$term[i]]
    }
  }
  plot.data$xvar_clean = factor(plot.data$xvar_clean, levels = term_levels)
  
  p = ggplot(aes_string(x = "xvar_clean", y = "median", ymax = "ymax", ymin = "ymin", color = "group", group = "group"), data = plot.data) +
    geom_hline(yintercept = hlineInt, color = I("red")) +
    #geom_point(color="gray75", alpha=1/(nrow(data)^.33), size=I(0.5), position = position_dodge(width = 0.5)) +
    geom_point(data = subset(plot.data, sig == TRUE),size=I(1), position = position_dodge(width = 0.65)) +
    #geom_errorbar(color="gray75", alpha=1/(nrow(data)^.33),width = 0.1, position = position_dodge(width = 0.5)) +
    geom_errorbar(data = subset(plot.data, sig == TRUE),width = 0.2, position = position_dodge(width = 0.65)) +
    coord_flip() +
    theme_bw() +
    scale_color_colorblind() +
    labs(y = "estimate", x = "term") +
    theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6), 
          title = element_text(size = 9), axis.title = element_text(size = 8))
  
  if(facet) {
    p = p + facet_grid(~facet)
  } 
  
  if(!is.null(title)) {
    p = p + labs(title = title)
  }
  return(p)
}


##### TWO TECHNOLOGIES #####
two.end = layer.cor %>% filter(overlap == 1)

#ri.obj.cnt.cor
hist(two.end$ri.obj.cnt.cor)
lmm.end1 = lmer(ri.obj.cnt.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end1)
# car::Anova(lmm.end1)
# REsim(lmm.end1)

#ri.cr.cor
lmm.end2 = lmer(ri.cr.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end2)
# car::Anova(lmm.end2)
# REsim(lmm.end2)

#ri.num.disc.cor
lmm.end3 = lmer(ri.num.disc.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end3)
# car::Anova(lmm.end3)
# REsim(lmm.end3)

#ri.num.scvg.cor
lmm.end4 = lmer(ri.num.scvg.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end4)
# car::Anova(lmm.end4)
# REsim(lmm.end4)

#ri.num.enct.cor
lmm.end5 = lmer(ri.num.enct.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end5)
# car::Anova(lmm.end5)
# REsim(lmm.end5)

#ri.num.ret.cor
lmm.end6 = lmer(ri.num.ret.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end6)
# car::Anova(lmm.end6)
# plotREheatmap(REsim(lmm.end6))


##### MANY TECHNOLOGIES #####
many.end = layer.cor %>% filter(overlap == 2)

#ri.obj.cnt.cor
lmm.end1 = lmer(ri.obj.cnt.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end1)
# car::Anova(lmm.end1)
# REsim(lmm.end1)

#ri.cr.cor
lmm.end2 = lmer(ri.cr.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end2)
# car::Anova(lmm.end2)
# REsim(lmm.end2)

#ri.num.disc.cor
lmm.end3 = lmer(ri.num.disc.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end3)
# car::Anova(lmm.end3)
# REsim(lmm.end3)

#ri.num.scvg.cor
lmm.end4 = lmer(ri.num.scvg.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)

# summary(lmm.end4)
# car::Anova(lmm.end4)
# REsim(lmm.end4)

#ri.num.enct.cor
lmm.end5 = lmer(ri.num.enct.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end5)
# car::Anova(lmm.end5)
# REsim(lmm.end5)


#ri.num.ret.cor
lmm.end6 = lmer(ri.num.ret.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end6)
# car::Anova(lmm.end6)
# plotREheatmap(REsim(lmm.end6))

####REGRESSIONS LOOKING AT OCCUPATION INTENSITY#####
compare_occup_scenarios = function(correlation) {
  
  lmf = paste(correlation, "~ mu + scavenge_prob + blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                flake_preference + size_preference + strict_selection +
                min_suitable_flake_size + (1 | square) ")
  
  #####two technologies#####
  occup1 = two.end %>%
    filter(num_agents == 100)
  
  lmm1 = lmer(lmf,  data = occup1)
  two.fe1 = FEsim(lmm1)
  two.fe1$facet = "two technologies"
  
  occup2 = two.end %>%
    filter(num_agents == 200)
  
  lmm2 = lmer(lmf,  data = occup2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  two.fe2 = FEsim(lmm2)
  two.fe2$facet = "two technologies"
  
  #####many technologies#####
  occup1 = many.end %>%
    filter(num_agents == 100)
  
  lmm1 = lmer(lmf,  data = occup1)
  # summary(lmm1)
  # car::Anova(lmm1)
  #plotREsim(REsim(lmm1))
  many.fe1 = FEsim(lmm1)
  many.fe1$facet = "many technologies"
  
  occup2 = many.end %>%
    filter(num_agents == 200)
  
  lmm2 = lmer(lmf,  data = occup2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  many.fe2 = FEsim(lmm2)
  many.fe2$facet = "many technologies"
  
  
  return(plotFEmult("100 agents" = many.fe1, "200 agents" = many.fe2, 
                    "100 agents" = two.fe1, "200 agents" = two.fe2,  
                    facet =TRUE, title = correlation)) 
}

ggaplot.o = ggarrange(compare_occup_scenarios(cor.names[2]), compare_occup_scenarios(cor.names[3]), 
                    compare_occup_scenarios(cor.names[4]), compare_occup_scenarios(cor.names[5]), 
                    compare_occup_scenarios(cor.names[6]), compare_occup_scenarios(cor.names[7]), 
                    ncol = 2, nrow = 3,
                    common.legend = T, labels = "AUTO")
plot(ggaplot.o)
ggsave(filename = "../figures/supplementary-figures/correlation-fixed-effects_occup-intensity.tiff", ggaplot.o, width = 8.25, height =9.5, dpi = 300)


compare_only_occup = function(correlation) {
  
  lmf = paste(correlation, "~ mu + scavenge_prob + blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                flake_preference + size_preference + strict_selection +
                min_suitable_flake_size + (1 | square) ")
  
  #####two technologies#####
  occup1 = two.end %>%
    filter(num_agents == 100)
  
  lmm1 = lmer(lmf,  data = occup1)
  two.fe1 = FEsim(lmm1)
  two.fe1$facet = "two technologies"
  two.fe1$num_agents = "100 agents"
  
  occup2 = two.end %>%
    filter(num_agents == 200)
  
  lmm2 = lmer(lmf,  data = occup2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  two.fe2 = FEsim(lmm2)
  two.fe2$facet = "two technologies"
  two.fe2$num_agents = "200 agents"
  
  #####many technologies#####
  occup1 = many.end %>%
    filter(num_agents == 100)
  
  lmm1 = lmer(lmf,  data = occup1)
  # summary(lmm1)
  # car::Anova(lmm1)
  #plotREsim(REsim(lmm1))
  many.fe1 = FEsim(lmm1)
  many.fe1$facet = "many technologies"
  many.fe1$num_agents = "100 agents"
  
  occup2 = many.end %>%
    filter(num_agents == 200)
  
  lmm2 = lmer(lmf,  data = occup2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  many.fe2 = FEsim(lmm2)
  many.fe2$facet = "many technologies"
  many.fe2$num_agents = "200 agents"
  
  allresults = rbind(many.fe1, many.fe2, two.fe1, two.fe2)
  
  intercept = allresults %>% filter(term == "(Intercept)")
  intercept$upper = intercept$mean + intercept$sd
  intercept$lower = intercept$mean - intercept$sd
  
  ggplot(intercept) +
    geom_col(aes(y = mean, x = facet, group = num_agents, fill = num_agents), position = position_dodge2()) +
    geom_errorbar(aes(x = facet, ymax = upper, ymin = lower, group = num_agents),color = "gray40", width = 0.25, size = 0.3, position = position_dodge(preserve = "single", width  = 0.9)) +
    geom_hline(aes(yintercept = 0), color = "red") +
    scale_fill_colorblind() +
    labs(y = "estimate", title = correlation) +
    theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 6), 
          title = element_text(size = 9), axis.title.y = element_text(size = 8)) +
    scale_y_continuous(limits = c(-0.85, 0.85))
}

ggaplot.o2 = ggarrange(compare_only_occup(cor.names[2]), compare_only_occup(cor.names[3]), 
                     compare_only_occup(cor.names[4]), compare_only_occup(cor.names[5]), 
                     compare_only_occup(cor.names[6]), compare_only_occup(cor.names[7]), 
                     ncol = 2, nrow = 3,
                     common.legend = T, labels = "AUTO")
#plot(ggaplot.o2)
ggsave(filename = "../figures/correlation-fixed-effects_occup-int-intercept.tiff", ggaplot.o2, width = 8.25, height =9.5, dpi = 300)



####REGRESSIONS LOOKING AT MU#####
compare_mu_scenarios = function(correlation) {
  
  lmf = paste(correlation, "~ num_agents + scavenge_prob + blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                flake_preference + size_preference + strict_selection +
                min_suitable_flake_size + (1 | square) ")
  
  #####two technologies#####
  mu1 = two.end %>%
    filter(mu == 1)
  
  lmm1 = lmer(lmf,  data = mu1)
  two.fe1 = FEsim(lmm1)
  two.fe1$facet = "two technologies"
  
  mu2 = two.end %>%
    filter(mu == 2)
  
  lmm2 = lmer(lmf,  data = mu2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  two.fe2 = FEsim(lmm2)
  two.fe2$facet = "two technologies"
  
  mu3 = two.end %>% 
    filter(mu == 3)
  
  lmm3 = lmer(lmf,  data = mu3)
  # summary(lmm3)
  # car::Anova(lmm3)
  #plotREsim(REsim(lmm3))
  two.fe3 = FEsim(lmm3)
  two.fe3$facet = "two technologies"
  
  #two.mu = plotFEmult("mu1" = two.fe1, "mu2" = two.fe2, "mu3" = two.fe3)
  
  #####many technologies#####
  mu1 = many.end %>%
    filter(mu == 1)
  
  lmm1 = lmer(lmf,  data = mu1)
  # summary(lmm1)
  # car::Anova(lmm1)
  #plotREsim(REsim(lmm1))
  many.fe1 = FEsim(lmm1)
  many.fe1$facet = "many technologies"
  
  mu2 = many.end %>%
    filter(mu == 2)
  
  lmm2 = lmer(lmf,  data = mu2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  many.fe2 = FEsim(lmm2)
  many.fe2$facet = "many technologies"
  
  mu3 = many.end %>%
    filter(mu == 3)
  
  lmm3 = lmer(lmf,  data = mu3)
  # summary(lmm3)
  # car::Anova(lmm3)
  #plotREsim(REsim(lmm3))
  many.fe3 = FEsim(lmm3)
  many.fe3$facet = "many technologies"
  
  return(plotFEmult("mu = 1" = many.fe1, "mu = 2" = many.fe2, "mu = 3" = many.fe3, 
                    "mu = 1" = two.fe1, "mu = 2" = two.fe2, "mu = 3" = two.fe3, 
                    facet =TRUE, title = correlation)) 
}

# ggsave(filename = "mu_ri.obj.cnt.cor.png", compare_mu_scenarios(cor.names[2]), 
#        width = 7.5, height = 5, dpi = 300) #ri.obj.cnt.cor
# ggsave(filename = "mu_ri.cr.cor.png",compare_mu_scenarios(cor.names[3]),
#        width = 7.5, height = 5, dpi = 300) #ri.cr.cor
# ggsave(filename = "mu_ri.num.disc.cor.png",compare_mu_scenarios(cor.names[4]),
#        width = 7.5, height = 5, dpi = 300)  #ri.num.disc.cor
# ggsave(filename = "mu_ri.num.scvg.cor.png",compare_mu_scenarios(cor.names[5]),
#        width = 7.5, height = 5, dpi = 300)  #ri.num.scvg.cor
# ggsave(filename = "mu_ri.num.enct.cor.png",compare_mu_scenarios(cor.names[6]),
#        width = 7.5, height = 5, dpi = 300)  #ri.num.enct.cor
# ggsave(filename = "mu_ri.num.ret.cor.png",compare_mu_scenarios(cor.names[8]),
#        width = 7.5, height = 5, dpi = 300)  #ri.num.ret.cor

ggaplot = ggarrange(compare_mu_scenarios(cor.names[2]), compare_mu_scenarios(cor.names[3]), 
                    compare_mu_scenarios(cor.names[4]), compare_mu_scenarios(cor.names[5]), 
                    compare_mu_scenarios(cor.names[6]), compare_mu_scenarios(cor.names[7]), 
                    ncol = 2, nrow = 3,
                    common.legend = T, labels = "AUTO")
plot(ggaplot)
ggsave(filename = "../figures/supplementary-figures/correlation-fixed-effects_mu.tiff", ggaplot, width = 8.25, height =9.5, dpi = 300)


compare_only_mu = function(correlation) {
  
  lmf = paste(correlation, "~ num_agents + scavenge_prob + blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                flake_preference + size_preference + strict_selection +
                min_suitable_flake_size + (1 | square) ")
  
  #####two technologies#####
  mu1 = two.end %>%
    filter(mu == 1)
  
  lmm1 = lmer(lmf,  data = mu1)
  two.fe1 = FEsim(lmm1)
  two.fe1$facet = "two technologies"
  two.fe1$mu = "mu = 1"
  
  mu2 = two.end %>%
    filter(mu == 2)
  
  lmm2 = lmer(lmf,  data = mu2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  two.fe2 = FEsim(lmm2)
  two.fe2$facet = "two technologies"
  two.fe2$mu = "mu = 2"
  
  mu3 = two.end %>% 
    filter(mu == 3)
  
  lmm3 = lmer(lmf,  data = mu3)
  # summary(lmm3)
  # car::Anova(lmm3)
  #plotREsim(REsim(lmm3))
  two.fe3 = FEsim(lmm3)
  two.fe3$facet = "two technologies"
  two.fe3$mu = "mu = 3"
  
  #two.mu = plotFEmult("mu1" = two.fe1, "mu2" = two.fe2, "mu3" = two.fe3)
  
  #####many technologies#####
  mu1 = many.end %>%
    filter(mu == 1)
  
  lmm1 = lmer(lmf,  data = mu1)
  # summary(lmm1)
  # car::Anova(lmm1)
  #plotREsim(REsim(lmm1))
  many.fe1 = FEsim(lmm1)
  many.fe1$facet = "many technologies"
  many.fe1$mu = "mu = 1"
  
  mu2 = many.end %>%
    filter(mu == 2)
  
  lmm2 = lmer(lmf,  data = mu2)
  # summary(lmm2)
  # car::Anova(lmm2)
  #plotREsim(REsim(lmm2))
  many.fe2 = FEsim(lmm2)
  many.fe2$facet = "many technologies"
  many.fe2$mu = "mu = 2"
  
  mu3 = many.end %>%
    filter(mu == 3)
  
  lmm3 = lmer(lmf,  data = mu3)
  # summary(lmm3)
  # car::Anova(lmm3)
  #plotREsim(REsim(lmm3))
  many.fe3 = FEsim(lmm3)
  many.fe3$facet = "many technologies"
  many.fe3$mu = "mu = 3"
  
  allresults = rbind(many.fe1, many.fe2, many.fe3, two.fe1, two.fe2, two.fe3)
  
  intercept = allresults %>% filter(term == "(Intercept)")
  intercept$upper = intercept$mean + intercept$sd
  intercept$lower = intercept$mean - intercept$sd
  
  ggplot(intercept) +
    geom_col(aes(y = mean, x = facet, group = mu, fill = mu), position = position_dodge2()) +
    geom_errorbar(aes(x = facet, ymax = upper, ymin = lower, group = mu),color = "gray40", width = 0.25, size = 0.3, position = position_dodge(preserve = "single", width  = 0.9)) +
    geom_hline(aes(yintercept = 0), color = "red") +
    scale_fill_colorblind() +
    labs(y = "estimate", title = correlation) +
    theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 6), 
          title = element_text(size = 9), axis.title.y = element_text(size = 8)) +
    scale_y_continuous(limits = c(-0.75, 0.75))
}

ggaplot2 = ggarrange(compare_only_mu(cor.names[2]), compare_only_mu(cor.names[3]), 
                     compare_only_mu(cor.names[4]), compare_only_mu(cor.names[5]), 
                     compare_only_mu(cor.names[6]), compare_only_mu(cor.names[7]), 
                    ncol = 2, nrow = 3,
                    common.legend = T, labels = "AUTO")
#plot(ggaplot2)
ggsave(filename = "../figures/correlation-fixed-effects_mu-intercept.tiff", ggaplot2, width = 8.25, height =9.5, dpi = 300)

####REGRESSIONS LOOKING AT SELECTION SCENARIOS#####

# compare_type_pref_scenarios = function(correlation) {
#   lmf = paste(correlation, "~ mu + scavenge_prob +
#                 blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + (1 | square)")
#   
#   #####two technologies #####
#   flake.pref = two.end %>%
#     filter(flake_preference == T, 
#            size_preference == F, 
#            strict_selection == T)
#   nodule.pref = two.end %>%
#     filter(flake_preference == F, 
#            size_preference == F, 
#            strict_selection == T)
#   
#   flk1 = lmer(lmf, data = flake.pref)
#   two.fe1 = FEsim(flk1)
#   two.fe1$facet = "two technologies"
#   
#   nod1 = lmer(lmf, data = nodule.pref)
#   two.fe2 = FEsim(nod1)
#   two.fe2$facet = "two technologies"
#   
#   #####many technologies #####
#   flake.pref = many.end %>%
#     filter(flake_preference == T, 
#            size_preference == F, 
#            strict_selection == F)
#   nodule.pref = many.end %>%
#     filter(flake_preference == F, 
#            size_preference == F, 
#            strict_selection == F)
#   
#   flk1 = lmer(lmf, data = flake.pref)
#   many.fe1 = FEsim(flk1)
#   many.fe1$facet = "many technologies"
#   
#   nod1 = lmer(lmf, data = nodule.pref)
#   many.fe2 = FEsim(nod1)
#   many.fe2$facet = "many technologies"
#   
#   plotFEmult("flakepref" = many.fe1, "nodulepref" = many.fe2, "flakepref" = two.fe1, "nodulepref" = two.fe2, facet = T)
# }
# 
# ggsave(filename = "type_ri.obj.cnt.cor.png", compare_type_pref_scenarios(cor.names[2]), 
#        width = 7.5, height = 5) #ri.obj.cnt.cor
# ggsave(filename = "type_ri.cr.cor.png", compare_type_pref_scenarios(cor.names[3]), 
#        width = 7.5, height = 5) #ri.cr.cor
# ggsave(filename = "type_ri.num.disc.cor.png", compare_type_pref_scenarios(cor.names[4]), 
#        width = 7.5, height = 5)  #ri.num.disc.cor
# ggsave(filename = "type_ri.num.scvg.cor.png", compare_type_pref_scenarios(cor.names[5]), 
#        width = 7.5, height = 5)  #ri.num.scvg.cor
# ggsave(filename = "type_ri.num.enct.cor.png", compare_type_pref_scenarios(cor.names[6]), 
#        width = 7.5, height = 5)  #ri.num.enct.cor
# ggsave(filename = "type_ri.num.ret.cor.png", compare_type_pref_scenarios(cor.names[8]), 
#        width = 7.5, height = 5)  #ri.num.ret.cor


compare_size_pref_scenarios = function(correlation) {
  
  lmf = paste(correlation, "~ mu + num_agents + max_use_intensity + max_artifact_carry  + 
  scavenge_prob + blank_prob + max_flake_size*min_suitable_flake_size  + (1 | square)")
  
  
  sizepref = two.end %>%
    filter(size_preference == T, 
           strict_selection == T, 
           flake_preference == T)
  
  nosizepref = two.end %>%
    filter(size_preference == F, 
           strict_selection == T, 
           flake_preference == T)
  
  size1 = lmer(lmf, data = sizepref)
  two.fe1 = FEsim(size1)
  two.fe1$facet = "two technologies"
  nsize1 = lmer(lmf, data = nosizepref)
  two.fe2 = FEsim(nsize1)
  two.fe2$facet = "two technologies"
  
  sizepref = many.end %>%
    filter(size_preference == T, 
           strict_selection == T, 
           flake_preference == T)
  
  nosizepref = many.end %>%
    filter(size_preference == F, 
           strict_selection == T, 
           flake_preference == T)
  
  size2 = lmer(lmf, data = sizepref)
  many.fe1 = FEsim(size2)
  many.fe1$facet = "many technologies"
  nsize2 = lmer(lmf, data = nosizepref)
  many.fe2 = FEsim(nsize2)
  many.fe2$facet = "many technologies"
  
  plotFEmult("size preference" = many.fe1, "no size preference" = many.fe2, "size preference" = two.fe1, "no size preference" = two.fe2, facet = T, title = correlation)
}

ggaplot2 = ggarrange(compare_size_pref_scenarios(cor.names[2]), compare_size_pref_scenarios(cor.names[3]), 
                     compare_size_pref_scenarios(cor.names[4]), compare_size_pref_scenarios(cor.names[5]), 
                     compare_size_pref_scenarios(cor.names[6]), compare_size_pref_scenarios(cor.names[7]), 
                     ncol = 2, nrow = 3,
                     common.legend = T, labels = "AUTO")
ggsave(filename = "../figures/supplementary-figures/correlation-fixed-effects_size-pref.tiff", ggaplot2, width = 8.25, height = 9.5, dpi = 300)


compare_only_size = function(correlation) {
  
  lmf = paste(correlation, "~ mu + num_agents + max_use_intensity + max_artifact_carry  + 
  scavenge_prob + blank_prob + max_flake_size*min_suitable_flake_size  + (1 | square)")
  
  sizepref = two.end %>%
    filter(size_preference == T, 
           strict_selection == T, 
           flake_preference == T)
  
  nosizepref = two.end %>%
    filter(size_preference == F, 
           strict_selection == T, 
           flake_preference == T)
  
  size1 = lmer(lmf, data = sizepref)
  two.fe1 = FEsim(size1)
  two.fe1$facet = "two technologies"
  two.fe1$size = "size preference"
  nsize1 = lmer(lmf, data = nosizepref)
  two.fe2 = FEsim(nsize1)
  two.fe2$facet = "two technologies"
  two.fe2$size = "no size preference"
  
  sizepref = many.end %>%
    filter(size_preference == T, 
           strict_selection == T, 
           flake_preference == T)
  
  nosizepref = many.end %>%
    filter(size_preference == F, 
           strict_selection == T, 
           flake_preference == T)
  
  size2 = lmer(lmf, data = sizepref)
  many.fe1 = FEsim(size2)
  many.fe1$facet = "many technologies"
  many.fe1$size = "size preference"
  nsize2 = lmer(lmf, data = nosizepref)
  many.fe2 = FEsim(nsize2)
  many.fe2$facet = "many technologies"
  many.fe2$size = "no size preference"
  
  allresults = rbind(many.fe1, many.fe2, two.fe1, two.fe2)
  
  intercept = allresults %>% filter(term == "(Intercept)")
  intercept$upper = intercept$mean + intercept$sd
  intercept$lower = intercept$mean - intercept$sd
  
  ggplot(intercept) +
    geom_col(aes(y = mean, x = facet, group = size, fill = size), position = position_dodge2()) +
    geom_errorbar(aes(x = facet, ymax = upper, ymin = lower, group = size),color = "gray40", width = 0.25, size = 0.3, position = position_dodge(preserve = "single", width  = 0.9)) +
    geom_hline(aes(yintercept = 0), color = "red") +
    scale_fill_colorblind() +
    labs(y = "estimate", title = correlation) +
    theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 6), 
          title = element_text(size = 9), axis.title.y = element_text(size = 8))  +
    scale_y_continuous(limits = c(-1, 1))
}

ggaplot2 = ggarrange(compare_only_size(cor.names[2]), compare_only_size(cor.names[3]), 
                     compare_only_size(cor.names[4]), compare_only_size(cor.names[5]), 
                     compare_only_size(cor.names[6]), compare_only_size(cor.names[7]), 
                     ncol = 2, nrow = 3,
                     common.legend = T, labels = "AUTO")
#plot(ggaplot2)
ggsave(filename = "../figures/correlation-fixed-effects_size-pref-intercept.tiff", ggaplot2, width = 8.25, height =9.5, dpi = 300)


##strict selection vs not strict selection
compare_strict_select_scenarios = function(correlation) {
  lmf = paste(correlation, "~ mu + num_agents + scavenge_prob + blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  min_suitable_flake_size + (1 | square)")
  
  strict = two.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == T)
  
  notstrict = two.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == F)
  
  size1 = lmer(lmf, data = strict)
  two.fe1 = FEsim(size1)
  two.fe1$facet = "two technologies"
  nsize1 = lmer(lmf, data = notstrict)
  two.fe2 = FEsim(nsize1)
  two.fe2$facet = "two technologies"
  
  
  strict = many.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == T)
  
  notstrict = many.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == F)
  
  size2 = lmer(lmf, data = strict)
  many.fe1 = FEsim(size2)
  many.fe1$facet = "many technologies"
  nsize2 = lmer(lmf, data = notstrict)
  many.fe2 = FEsim(nsize2)
  many.fe2$facet = "many technologies"
  
  plotFEmult("strict selection" = many.fe1, "non-strict selection" = many.fe2, "strict selection" = two.fe1, "non-strict selection" = two.fe2, facet = T, title = correlation)
  
}

ggaplot3 = ggarrange(compare_strict_select_scenarios(cor.names[2]), compare_strict_select_scenarios(cor.names[3]), 
                     compare_strict_select_scenarios(cor.names[4]), compare_strict_select_scenarios(cor.names[5]), 
                     compare_strict_select_scenarios(cor.names[6]), compare_strict_select_scenarios(cor.names[7]), 
                     ncol = 2, nrow = 3,
                     common.legend = T, labels = "AUTO")
ggsave(filename = "../figures/supplementary-figures/correlation-fixed-effects_strict.tiff", ggaplot3, width = 8.25, height = 9.5, dpi = 300)

compare_only_strict = function(correlation) {
  lmf = paste(correlation, "~ mu + num_agents + scavenge_prob + blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  min_suitable_flake_size + (1 | square)")
  
  strict = two.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == T)
  
  notstrict = two.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == F)
  
  size1 = lmer(lmf, data = strict)
  two.fe1 = FEsim(size1)
  two.fe1$facet = "two technologies"
  two.fe1$strict = "strict selection"
  nsize1 = lmer(lmf, data = notstrict)
  two.fe2 = FEsim(nsize1)
  two.fe2$facet = "two technologies"
  two.fe2$strict = "non-strict selection"
  
  
  strict = many.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == T)
  
  notstrict = many.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == F)
  
  size2 = lmer(lmf, data = strict)
  many.fe1 = FEsim(size2)
  many.fe1$facet = "many technologies"
  many.fe1$strict = "strict selection"
  nsize2 = lmer(lmf, data = notstrict)
  many.fe2 = FEsim(nsize2)
  many.fe2$facet = "many technologies"
  many.fe2$strict = "non-strict selection"
  
  allresults = rbind(many.fe1, many.fe2, two.fe1, two.fe2)
  
  intercept = allresults %>% filter(term == "(Intercept)")
  intercept$upper = intercept$mean + intercept$sd
  intercept$lower = intercept$mean - intercept$sd
  
  ggplot(intercept) +
    geom_col(aes(y = mean, x = facet, group = strict, fill = strict), position = position_dodge2()) +
    geom_errorbar(aes(x = facet, ymax = upper, ymin = lower, group = strict),color = "gray40", width = 0.25, size = 0.3, position = position_dodge(preserve = "single", width  = 0.9)) +
    geom_hline(aes(yintercept = 0), color = "red") +
    scale_fill_colorblind() +
    labs(y = "estimate", title = correlation) +
    theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text = element_text(size = 6), 
          title = element_text(size = 9), axis.title.y = element_text(size = 8))  +
    scale_y_continuous(limits = c(-0.75, 0.75))
  
}

ggaplot2 = ggarrange(compare_only_strict(cor.names[2]), compare_only_strict(cor.names[3]), 
                     compare_only_strict(cor.names[4]), compare_only_strict(cor.names[5]), 
                     compare_only_strict(cor.names[6]), compare_only_strict(cor.names[7]), 
                     ncol = 2, nrow = 3,
                     common.legend = T, labels = "AUTO")
plot(ggaplot2)
ggsave(filename = "../figures/correlation-fixed-effects_strict-intercept.tiff", ggaplot2, width = 8.25, height =9.5, dpi = 300)

