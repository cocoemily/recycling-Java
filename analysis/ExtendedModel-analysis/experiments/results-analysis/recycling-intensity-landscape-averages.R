library(tidyverse)
library(ggpubr)
library(ggthemes)
library(plyr)
library(fitdistrplus)
library(MASS)
library(rstatix)

theme_set(theme_bw())

size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "non-strict selection")
names(strict.labs) = c("TRUE", "FALSE")
tech.labs = c("two technology types", "many technology types")
names(tech.labs) = c("1", "2")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)
blank.labs = c("blank probability: 0.25", "blank probability: 0.5", "blank probability: 0.75")
names(blank.labs) = c(0.25, 0.5, 0.75)
mu.labs = c("mu = 1", "mu = 2", "mu = 3")
names(mu.labs) = c(1, 2, 3)

ri.ci = read_csv("~/eclipse-workspace/recycling-Java/results/recycling-intensity-CI.csv")

summary(ri.ci$mean)
hist(ri.ci$mean)
ri.ci$range = ri.ci$upper - ri.ci$lower
summary(ri.ci$sd)

wilcox_test(ri.ci, mean ~ overlap, p.adjust.method = "bonferroni")

stat.test <- ri.ci %>%
  group_by(blank_prob, overlap) %>%
  wilcox_test(mean ~ scavenge_prob) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

mu1 = ddply(ri.ci, c("blank_prob", "scavenge_prob", "overlap"), summarize, grp.mean = median(mean))
plot1 = ggplot(ri.ci) +
  geom_density(aes(x = mean, fill = as.factor(scavenge_prob), color = as.factor(scavenge_prob)), alpha = 0.25) +
  geom_density(aes(x = mean, color = as.factor(scavenge_prob))) +
  geom_vline(data = mu1, aes(xintercept = grp.mean, color = as.factor(scavenge_prob)), linetype = "dashed") +
  facet_grid(overlap~blank_prob, scales = "free", labeller = labeller(
    overlap = tech.labs, 
    blank_prob = blank.labs
  ))  +
  labs(color = "scavenging probability") +
  guides(fill = F) +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(limits = c(0, 60))

ggsave(
  filename = "../figures/average-recycling-intensity_by-probs.tiff", 
  plot1, 
  dpi = 300, width = 8, height = 4.5
)


###archaeology-like scenarios with only two technologies and random recycling
two.ri = ri.ci %>% filter(overlap == 1) %>%
  filter(blank_prob == 0.5 & scavenge_prob == 0.5)

stat.test2 <- two.ri %>%
  group_by(flake_preference, size_preference) %>%
  wilcox_test(mean ~ strict_selection) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

stat.test22 <- two.ri %>% filter(size_preference == F) %>%
  group_by(strict_selection) %>%
  wilcox_test(mean ~ flake_preference) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()


mu2 = ddply(two.ri, c("strict_selection", "flake_preference", "size_preference"), summarize, grp.mean = mean(mean))
plot2 = ggplot(two.ri) +
  geom_density(aes(x = mean, fill = as.factor(strict_selection), color = as.factor(strict_selection)), alpha = 0.25) +
  geom_density(aes(x = mean, color = as.factor(strict_selection))) +
  geom_vline(data = mu2, aes(xintercept = grp.mean, color = as.factor(strict_selection)), linetype = "dashed") +
  facet_wrap(~flake_preference+size_preference, ncol = 1,
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs
             )) +
  scale_color_colorblind(labels = strict.labs) +
  scale_fill_colorblind() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = F)

# ggsave(
#   filename = "../figures/supplementary-figures/average-recycling-intensity_by-selection.tiff", 
#   plot2, 
#   dpi = 300, width = 6, height = 6
# )


wilcox_test(data = two.ri, mean ~ num_agents)

stat.test3 <- two.ri %>%
  group_by(num_agents) %>%
  wilcox_test(mean ~ mu) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

mu3 = ddply(two.ri, c("num_agents", "mu"), summarize, grp.mean = mean(mean))
plot3 = ggplot(two.ri) +
  geom_density(aes(x = mean, color = as.factor(mu), fill = as.factor(mu)), alpha = 0.25) +
  geom_density(aes(x = mean, color = as.factor(mu))) +
  geom_vline(data = mu3, aes(xintercept = grp.mean, color = as.factor(mu)), linetype = "dashed") +
  facet_wrap(~num_agents, ncol = 1, 
             labeller = labeller(
               num_agents = occup.labs
             )) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_colorblind(labels = mu.labs) +
  scale_fill_colorblind() +
  guides(fill = F)

# ggsave(
#   filename = "../figures/supplementary-figures/average-recycling-intensity_by-move-occup.tiff", 
#   plot3, 
#   dpi = 300, width = 6, height = 6
# )

supp.plot = ggarrange(plot3, plot2, labels = "AUTO")
ggsave(
  filename = "../figures/supplementary-figures/average-recycling-intensity_SUPP.tiff",
  supp.plot,
  dpi = 300, width = 8, height = 6
)


fit1 = glm.nb(mean ~ ., data = ri.ci[,c(2:4, 6:15)])
summary(fit1)


