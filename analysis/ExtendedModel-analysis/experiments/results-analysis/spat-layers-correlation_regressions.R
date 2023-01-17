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

source("ExtendedModel-analysis/experiments/results-analysis/helper-functions.R")

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")
parameters = colnames(layer.cor[,4:15])
cor.names = colnames(layer.cor[,16:24])
cor.names = cor.names[-length(cor.names)] #removing occupations correlation because encounters = occupations

layer.cor1 = layer.cor[-c(25:27)]
layer.cor = layer.cor1  %>% group_by(row, col) %>% 
  mutate(square = cur_group_id())

row.col = unique(layer.cor %>% select(row, col, square))


layer.cor.end = layer.cor[which(layer.cor$time == "end"),]
layer.cor.mid = layer.cor[which(layer.cor$time == "mid"),]


plotREheatmap = function(data) {
  data$groupID = as.numeric(data$groupID)
  data = data %>% left_join(row.col, by = c("groupID" = "square"))
  
  data[, "sd"] <- data[, "sd"] * qnorm(1-((1-0.95)/2))
  data[, "ymax"] <- data[, "median"] + data[, "sd"]
  data[, "ymin"] <- data[, "median"] - data[, "sd"]
  data[, "sig"] <- data[, "ymin"] > 0 | data[, "ymax"] < 0
  hlineInt <- 0
  
  p2 = ggplot(data) +
    geom_tile(aes(x = as.factor(col), y = as.factor(row), fill = median, alpha = sig), color = "black") +
    coord_fixed() +
    scale_fill_gradient2(low = "#075AFF",
                         mid = "#FFFFCC",
                         high = "#FF0000", 
                         limits = c(-0.15, 0.15)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          legend.position = "none") +
    labs(x = NULL, y = NULL, fill = "random effect intercept", alpha = "signf.") +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top")
  #plot(p2)
  return(p2)
  
}

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
  
  p = ggplot(aes_string(x = xvar, y = "median", ymax = "ymax", ymin = "ymin", color = "group", group = "group"), data = plot.data) +
    geom_hline(yintercept = hlineInt, color = I("red")) +
    #geom_point(color="gray75", alpha=1/(nrow(data)^.33), size=I(0.5), position = position_dodge(width = 0.5)) +
    geom_point(data = subset(plot.data, sig == TRUE),size=I(1), position = position_dodge(width = 0.5)) +
    #geom_errorbar(color="gray75", alpha=1/(nrow(data)^.33),width = 0.1, position = position_dodge(width = 0.5)) +
    geom_errorbar(data = subset(plot.data, sig == TRUE),width = 0.2, position = position_dodge(width = 0.5)) +
    coord_flip() +
    theme_bw() +
    scale_color_colorblind()
  
  if(facet) {
    p = p + facet_grid(~facet)
  } 
  
  if(!is.null(title)) {
    p = p + labs(title = title)
  }
  return(p)
}


##need to separate out overlap = 1 and overlap = 2

##### TWO TECHNOLOGIES #####
two.end = layer.cor.end %>% filter(overlap == 1)
two.mid = layer.cor.mid %>% filter(overlap == 1)

#ri.obj.cnt.cor
lmm.end1 = lmer(ri.obj.cnt.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end1)
# car::Anova(lmm.end1)
# REsim(lmm.end1)
plotFEsim(FEsim(lmm.end1))

lmm.mid1 = lmer(ri.obj.cnt.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.mid)
# summary(lmm.mid1)
# car::Anova(lmm.mid1)
# REsim(lmm.mid1)

plot_row1 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid1)), plotREheatmap(REsim(lmm.end1)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title1 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and object count",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p1 = plot_grid(title1, plot_row1, ncol = 1, rel_heights = c(0.1, 1))
plot(p1)

#ri.cr.cor
lmm.end2 = lmer(ri.cr.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end2)
# car::Anova(lmm.end2)
# REsim(lmm.end2)
plotFEsim(FEsim(lmm.end2))

lmm.mid2 = lmer(ri.cr.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size +  (1 | square), data = two.mid)
# summary(lmm.mid2)
# car::Anova(lmm.mid2)
# REsim(lmm.mid2)

plot_row2 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid2)), plotREheatmap(REsim(lmm.end2)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title2 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and cortex ratio",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p2 = plot_grid(title2, plot_row2, ncol = 1, rel_heights = c(0.1, 1))
plot(p2)

#ri.num.disc.cor
lmm.end3 = lmer(ri.num.disc.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end3)
# car::Anova(lmm.end3)
# REsim(lmm.end3)

lmm.mid3 = lmer(ri.num.disc.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.mid)
# summary(lmm.mid3)
# car::Anova(lmm.mid3)
# REsim(lmm.mid3)

plot_row3 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid3)), plotREheatmap(REsim(lmm.end3)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title3 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and discard events",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p3 = plot_grid(title3, plot_row3, ncol = 1, rel_heights = c(0.1, 1))
plot(p3)

#ri.num.scvg.cor
lmm.end4 = lmer(ri.num.scvg.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end4)
# car::Anova(lmm.end4)
# REsim(lmm.end4)

lmm.mid4 = lmer(ri.num.scvg.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.mid)
# summary(lmm.mid4)
# car::Anova(lmm.mid4)
# REsim(lmm.mid4)

plot_row4 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid4)), plotREheatmap(REsim(lmm.end4)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title4 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and scavenging events",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p4 = plot_grid(title4, plot_row4, ncol = 1, rel_heights = c(0.1, 1))
plot(p4)

#ri.num.enct.cor
lmm.end5 = lmer(ri.num.enct.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end5)
# car::Anova(lmm.end5)
# REsim(lmm.end5)

lmm.mid5 = lmer(ri.num.enct.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.mid)
# summary(lmm.mid5)
# car::Anova(lmm.mid5)
# REsim(lmm.mid5)

plot_row5 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid5)), plotREheatmap(REsim(lmm.end5)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title5 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and encounters",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p5 = plot_grid(title5, plot_row5, ncol = 1, rel_heights = c(0.1, 1))
plot(p5)

#ri.num.ret.cor
lmm.end6 = lmer(ri.num.ret.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.end)
# summary(lmm.end6)
# car::Anova(lmm.end6)
# plotREheatmap(REsim(lmm.end6))

lmm.mid6 = lmer(ri.num.ret.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = two.mid)
# summary(lmm.mid6)
# car::Anova(lmm.mid6)
# plotREheatmap(REsim(lmm.mid6))

plot_row6 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid6)), plotREheatmap(REsim(lmm.end6)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title6 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and retouch events",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p6 = plot_grid(title6, plot_row6, ncol = 1, rel_heights = c(0.1, 1))


plot_grid(p1, p2, p3, p4, p5, p6, ncol = 1)

##### MANY TECHNOLOGIES #####
many.end = layer.cor.end %>% filter(overlap == 2)
many.mid = layer.cor.mid %>% filter(overlap == 2)

#ri.obj.cnt.cor
lmm.end1 = lmer(ri.obj.cnt.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end1)
# car::Anova(lmm.end1)
# REsim(lmm.end1)
plotFEsim(FEsim(lmm.end1))

lmm.mid1 = lmer(ri.obj.cnt.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.mid)
# summary(lmm.mid1)
# car::Anova(lmm.mid1)
# REsim(lmm.mid1)

plot_row1 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid1)), plotREheatmap(REsim(lmm.end1)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title1 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and object count",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p1 = plot_grid(title1, plot_row1, ncol = 1, rel_heights = c(0.1, 1))

#ri.cr.cor
lmm.end2 = lmer(ri.cr.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end2)
# car::Anova(lmm.end2)
# REsim(lmm.end2)

lmm.mid2 = lmer(ri.cr.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.mid)
# summary(lmm.mid2)
# car::Anova(lmm.mid2)
# REsim(lmm.mid2)

plot_row2 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid2)), plotREheatmap(REsim(lmm.end2)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title2 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and cortex ratio",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p2 = plot_grid(title2, plot_row2, ncol = 1, rel_heights = c(0.1, 1))

#ri.num.disc.cor
lmm.end3 = lmer(ri.num.disc.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end3)
# car::Anova(lmm.end3)
# REsim(lmm.end3)

lmm.mid3 = lmer(ri.num.disc.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size +  (1 | square), data = many.mid)
# summary(lmm.mid3)
# car::Anova(lmm.mid3)
# REsim(lmm.mid3)

plot_row3 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid3)), plotREheatmap(REsim(lmm.end3)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title3 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and discard events",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p3 = plot_grid(title3, plot_row3, ncol = 1, rel_heights = c(0.1, 1))

#ri.num.scvg.cor
lmm.end4 = lmer(ri.num.scvg.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)

# summary(lmm.end4)
# car::Anova(lmm.end4)
# REsim(lmm.end4)

lmm.mid4 = lmer(ri.num.scvg.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.mid)
# summary(lmm.mid4)
# car::Anova(lmm.mid4)
# REsim(lmm.mid4)

plot_row4 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid4)), plotREheatmap(REsim(lmm.end4)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title4 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and scavenging events",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p4 = plot_grid(title4, plot_row4, ncol = 1, rel_heights = c(0.1, 1))

#ri.num.enct.cor
lmm.end5 = lmer(ri.num.enct.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end5)
# car::Anova(lmm.end5)
# REsim(lmm.end5)

lmm.mid5 = lmer(ri.num.enct.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.mid)
# summary(lmm.mid5)
# car::Anova(lmm.mid5)
# REsim(lmm.mid5)

plot_row5 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid5)), plotREheatmap(REsim(lmm.end5)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title5 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and encounters",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p5 = plot_grid(title5, plot_row5, ncol = 1, rel_heights = c(0.1, 1))


#ri.num.ret.cor
lmm.end6 = lmer(ri.num.ret.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.end)
# summary(lmm.end6)
# car::Anova(lmm.end6)
# plotREheatmap(REsim(lmm.end6))

lmm.mid6 = lmer(ri.num.ret.cor ~ mu + scavenge_prob +
                  blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                  flake_preference + size_preference + strict_selection +
                  min_suitable_flake_size + (1 | square), data = many.mid)
# summary(lmm.mid6)
# car::Anova(lmm.mid6)
# plotREheatmap(REsim(lmm.mid6))

plot_row6 = cowplot::plot_grid(plotREheatmap(REsim(lmm.mid6)), plotREheatmap(REsim(lmm.end6)),
                               labels = c("middle of model", "end of model"), label_size = 8)
title6 <- ggdraw() + 
  draw_label(
    "Correlation of recycling intensity and retouch events",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p6 = plot_grid(title6, plot_row6, ncol = 1, rel_heights = c(0.1, 1))


plot_grid(p1, p2, p3, p4, p5, p6, ncol = 1)


####REGRESSIONS LOOKING AT MU#####
compare_mu_scenarios = function(correlation) {
  
  lmf = paste(correlation, "~ scavenge_prob +
                blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
                flake_preference + size_preference + strict_selection +
                min_suitable_flake_size +(1 | square) ")
  
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
  
  return(plotFEmult("mu1" = many.fe1, "mu2" = many.fe2, "mu3" = many.fe3, 
                    "mu1" = two.fe1, "mu2" = two.fe2, "mu3" = two.fe3, 
                    facet =TRUE, title = correlation)) 
}

ggsave(filename = "mu_ri.obj.cnt.cor.png", compare_mu_scenarios(cor.names[2]), 
       width = 7.5, height = 5) #ri.obj.cnt.cor
ggsave(filename = "mu_ri.cr.cor.png",compare_mu_scenarios(cor.names[3]),
       width = 7.5, height = 5) #ri.cr.cor
ggsave(filename = "mu_ri.num.disc.cor.png",compare_mu_scenarios(cor.names[4]),
       width = 7.5, height = 5)  #ri.num.disc.cor
ggsave(filename = "mu_ri.num.scvg.cor.png",compare_mu_scenarios(cor.names[5]),
       width = 7.5, height = 5)  #ri.num.scvg.cor
ggsave(filename = "mu_ri.num.enct.cor.png",compare_mu_scenarios(cor.names[6]),
       width = 7.5, height = 5)  #ri.num.enct.cor
ggsave(filename = "mu_ri.num.ret.cor.png",compare_mu_scenarios(cor.names[8]),
       width = 7.5, height = 5)  #ri.num.ret.cor

####REGRESSIONS LOOKING AT SELECTION SCENARIOS#####

compare_type_pref_scenarios = function(correlation) {
  lmf = paste(correlation, "~ mu + scavenge_prob +
                blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + (1 | square)")
  
  #####two technologies #####
  flake.pref = two.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == T)
  nodule.pref = two.end %>%
    filter(flake_preference == F, 
           size_preference == F, 
           strict_selection == T)
  
  flk1 = lmer(lmf, data = flake.pref)
  two.fe1 = FEsim(flk1)
  two.fe1$facet = "two technologies"
  
  nod1 = lmer(lmf, data = nodule.pref)
  two.fe2 = FEsim(nod1)
  two.fe2$facet = "two technologies"
  
  #####many technologies #####
  flake.pref = many.end %>%
    filter(flake_preference == T, 
           size_preference == F, 
           strict_selection == T)
  nodule.pref = many.end %>%
    filter(flake_preference == F, 
           size_preference == F, 
           strict_selection == T)
  
  flk1 = lmer(lmf, data = flake.pref)
  many.fe1 = FEsim(flk1)
  many.fe1$facet = "many technologies"
  
  nod1 = lmer(lmf, data = nodule.pref)
  many.fe2 = FEsim(nod1)
  many.fe2$facet = "many technologies"
  
  plotFEmult("flakepref" = many.fe1, "nodulepref" = many.fe2, "flakepref" = two.fe1, "nodulepref" = two.fe2, facet = T)
}

ggsave(filename = "type_ri.obj.cnt.cor.png", compare_type_pref_scenarios(cor.names[2]), 
       width = 7.5, height = 5) #ri.obj.cnt.cor
ggsave(filename = "type_ri.cr.cor.png", compare_type_pref_scenarios(cor.names[3]), 
       width = 7.5, height = 5) #ri.cr.cor
ggsave(filename = "type_ri.num.disc.cor.png", compare_type_pref_scenarios(cor.names[4]), 
       width = 7.5, height = 5)  #ri.num.disc.cor
ggsave(filename = "type_ri.num.scvg.cor.png", compare_type_pref_scenarios(cor.names[5]), 
       width = 7.5, height = 5)  #ri.num.scvg.cor
ggsave(filename = "type_ri.num.enct.cor.png", compare_type_pref_scenarios(cor.names[6]), 
       width = 7.5, height = 5)  #ri.num.enct.cor
ggsave(filename = "type_ri.num.ret.cor.png", compare_type_pref_scenarios(cor.names[8]), 
       width = 7.5, height = 5)  #ri.num.ret.cor


compare_size_pref_scenarios = function(correlation) {
  
  lmf = paste(correlation, "~ mu + scavenge_prob +
                 blank_prob + max_use_intensity + max_artifact_carry  + 
              max_flake_size*min_suitable_flake_size + (1 | square)")
  
  
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
  
  plotFEmult("sizepref" = many.fe1, "nosizepref" = many.fe2, "sizepref" = two.fe1, "nosizepref" = two.fe2, facet = T, title = correlation)
}
ggsave(filename = "size_ri.obj.cnt.cor.png", compare_size_pref_scenarios(cor.names[2]), 
       width = 7.5, height = 5) #ri.obj.cnt.cor
ggsave(filename = "size_ri.cr.cor.png", compare_size_pref_scenarios(cor.names[3]), 
       width = 7.5, height = 5) #ri.cr.cor
ggsave(filename = "size_ri.num.disc.cor.png", compare_size_pref_scenarios(cor.names[4]), 
       width = 7.5, height = 5)  #ri.num.disc.cor
ggsave(filename = "size_ri.num.scvg.cor.png", compare_size_pref_scenarios(cor.names[5]), 
       width = 7.5, height = 5)  #ri.num.scvg.cor
ggsave(filename = "size_ri.num.enct.cor.png", compare_size_pref_scenarios(cor.names[6]), 
       width = 7.5, height = 5)  #ri.num.enct.cor
ggsave(filename = "size_ri.num.ret.cor.png", compare_size_pref_scenarios(cor.names[8]), 
       width = 7.5, height = 5)  #ri.num.ret.cor


##strict selection vs not strict selection
compare_strict_select_scenarios = function(correlation) {
  lmf = paste(correlation, "~ mu + scavenge_prob +
                 blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
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
  
  plotFEmult("strict" = many.fe1, "notstrict" = many.fe2, "strict" = two.fe1, "notstrict" = two.fe2, facet = T, title = correlation)
    
}
ggsave(filename = "strict_ri.obj.cnt.cor.png", compare_strict_select_scenarios(cor.names[2]), 
       width = 7.5, height = 5) #ri.obj.cnt.cor
ggsave(filename = "strict_ri.cr.cor.png", compare_strict_select_scenarios(cor.names[3]), 
       width = 7.5, height = 5) #ri.cr.cor
ggsave(filename = "strict_ri.num.disc.cor.png", compare_strict_select_scenarios(cor.names[4]), 
       width = 7.5, height = 5)  #ri.num.disc.cor
ggsave(filename = "strict_ri.num.scvg.cor.png", compare_strict_select_scenarios(cor.names[5]), 
       width = 7.5, height = 5)  #ri.num.scvg.cor
ggsave(filename = "strict_ri.num.enct.cor.png", compare_strict_select_scenarios(cor.names[6]), 
       width = 7.5, height = 5)  #ri.num.enct.cor
ggsave(filename = "strict_ri.num.ret.cor.png", compare_strict_select_scenarios(cor.names[8]), 
       width = 7.5, height = 5)  #ri.num.ret.cor
