##ANALYSIS OF ARTIFACT EXPOSURE

library(tidyverse)
library(ggthemes)
library(jtools)
library(ggpubr)
library(Dict)

theme_set(theme_bw())

exposure.data = read_csv("~/eclipse-workspace/recycling-Java/results/artifact-exposure-Wilcoxon-results.csv")

##recycled.signif.greater = whether (TRUE) or not (FALSE) the recycled artifacts 
## have ~significantly~ greater initial discard dates than the non-recycled artifacts

two.tech = exposure.data %>% filter(overlap == 1)
many.tech = exposure.data %>% filter(overlap == 2)


check.exp = exposure.data %>% 
  group_by(exp) %>%
  summarize(count_TRUE = sum(recycled.signif.greater), 
            percent_TRUE = sum(recycled.signif.greater)/n(), 
            percent_FALSE = sum(!recycled.signif.greater)/n())

summary(check.exp$percent_TRUE)
summary(check.exp$count_TRUE)

check.not.older = check.exp %>% filter(percent_FALSE > 0.5) %>%
  left_join(exposure.data[,c("exp",parameters)], by = "exp")

for(p in parameters) {
  print(unique(check.not.older[,c(p)]))
}


terms_dict = dict(
  "as.factor(mu)2" = "mu = 2", 
  "as.factor(mu)3" = "mu = 3", 
  "as.factor(overlap)2" = "many technologies", 
  "mu" = "mu",
  "num_agents" = "number of agents",
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
  "many technologies",
  "mu",
  "mu = 2", 
  "mu = 3",
  "number of agents",
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

two.ed = exposure.data %>% filter(overlap == 1)

fit1 = glm(recycled.signif.greater ~ ., two.ed[,c(3,4:6,8:9,11:16)], family = "binomial")
summary(fit1)
fit1.df = tidy(fit1)

fit1.df$term_clean = ""
for(i in 1:nrow(fit1.df)) {
  fit1.df$term_clean[i] = terms_dict[fit1.df$term[i]]
}
fit1.df$term_clean = factor(fit1.df$term_clean, levels = term_levels)
fit1.df$signif = ifelse(fit1.df$p.value < 0.05, TRUE, FALSE)
fit1.df$lower = fit1.df$estimate - fit1.df$std.error
fit1.df$upper = fit1.df$estimate + fit1.df$std.error

fit1.df$odds = exp(fit1.df$estimate)

odds.or = ggplot(fit1.df, aes(y = odds, x = term_clean, color = signif)) +
  geom_hline(aes(yintercept = 0), color = I("red"), linetype = "dashed") +
  #geom_linerange(mapping = aes(ymin = lower, ymax = upper), position = position_dodge2(width = 0.5)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  scale_color_colorblind() +
  #scale_shape_manual(values = c(4, 19)) + 
  coord_flip() +
  labs(x = "terms", shape = "significant?") +
  theme(legend.position = "none")

# ggsave(filename = "../figures/odds-ratios-YOFD.tiff", 
#        odds.or, 
#        dpi = 300, width = 5, height = 3)



# fit.test = glm(recycled.signif.greater ~ exp, exposure.data, family = "binomial")
# test.df = tidy(fit.test)

#### dummy variables####
reg.data = exposure.data

reg.data$overlap1 = ifelse(reg.data$overlap == 1, 1, 0)
reg.data$overlap2 = ifelse(reg.data$overlap == 2, 1, 0)
reg.data$maxuse15 = ifelse(reg.data$max_use_intensity == 15, 1, 0)
reg.data$maxuse30 = ifelse(reg.data$max_use_intensity == 30, 1, 0)
reg.data$maxcarry10 = ifelse(reg.data$max_artifact_carry == 10, 1, 0)
reg.data$maxcarry20 = ifelse(reg.data$max_artifact_carry == 20, 1, 0)
reg.data$maxflake1 = ifelse(reg.data$max_flake_size == 1, 1, 0)
reg.data$maxflake2 = ifelse(reg.data$max_flake_size == 2, 1, 0)
reg.data$blankp25 = ifelse(reg.data$blank_prob == 0.25, 1, 0)
reg.data$blankp50 = ifelse(reg.data$blank_prob == 0.5, 1, 0)
reg.data$blankp75 = ifelse(reg.data$blank_prob == 0.75, 1, 0)
reg.data$scvgp25 = ifelse(reg.data$scavenge_prob == 0.25, 1, 0)
reg.data$scvgp50 = ifelse(reg.data$scavenge_prob == 0.5, 1, 0)
reg.data$scvgp75 = ifelse(reg.data$scavenge_prob == 0.75, 1, 0)
reg.data$mu1 = ifelse(reg.data$mu == 1, 1, 0)
reg.data$mu2 = ifelse(reg.data$mu == 2, 1, 0)
reg.data$mu3 = ifelse(reg.data$mu == 3, 1, 0)
reg.data$agents100 = ifelse(reg.data$num_agents == 100, 1, 0)
reg.data$agents200 = ifelse(reg.data$num_agents == 200, 1, 0)
reg.data$sizeTRUE = ifelse(reg.data$size_preference == T, 1, 0)
reg.data$sizeFALSE = ifelse(reg.data$size_preference == F, 1, 0)
reg.data$flakeTRUE = ifelse(reg.data$flake_preference == T, 1, 0)
reg.data$nodTRUE = ifelse(reg.data$flake_preference == F, 1, 0)
reg.data$strictTRUE = ifelse(reg.data$strict_selection == T, 1, 0)
reg.data$strictFALSE = ifelse(reg.data$strict_selection == F, 1, 0)
reg.data$minflake1 = ifelse(reg.data$min_suitable_flake_size == 1, 1, 0)
reg.data$minflake2 = ifelse(reg.data$min_suitable_flake_size == 2, 1, 0)

reg.data2 = reg.data[,c(3,17:43)]

fit2 = glm(recycled.signif.greater ~ ., reg.data2, family = "binomial")
summary(fit2)
fit2.df = tidy(fit2)
fit2.df$odds = exp(fit2.df$estimate)

# odds.or2 = ggplot(fit2.df, aes(y = odds, x = term)) +
#   geom_hline(aes(yintercept = 0), color = I("red"), linetype = "dashed") +
#   #geom_linerange(mapping = aes(ymin = lower, ymax = upper)) +
#   geom_point() +
#   #scale_shape_manual(values = c(4, 19)) + 
#   coord_flip() +
#   labs(x = "terms", shape = "significant?") +
#   theme(legend.position = "none")
# plot(odds.or2)

fit3 = glm(recycled.signif.greater ~ 
             as.factor(overlap) + as.factor(mu) + num_agents +
             max_use_intensity + max_artifact_carry +
             blank_prob + scavenge_prob +
             max_flake_size + min_suitable_flake_size + max_flake_size:min_suitable_flake_size +
             flake_preference + size_preference + strict_selection, 
           data = exposure.data, family = "binomial")  
summary(fit3)
fit3.df = tidy(fit3)

fit3.df$signif = ifelse(fit3.df$p.value < 0.05, TRUE, FALSE)
fit3.df$lower = fit3.df$estimate - fit3.df$std.error
fit3.df$upper = fit3.df$estimate + fit3.df$std.error

fit3.df$odds = exp(fit3.df$estimate)

fit3.df$term_clean = ""
for(i in 1:nrow(fit3.df)) {
  fit3.df$term_clean[i] = terms_dict[fit3.df$term[i]]
}
fit3.df$term_clean = factor(fit3.df$term_clean, levels = term_levels)

odds.or3 = ggplot(fit3.df, aes(y = odds, x = term_clean, shape = signif)) +
  geom_hline(aes(yintercept = 0), color = I("red"), linetype = "dashed") +
  #geom_linerange(mapping = aes(ymin = lower, ymax = upper)) +
  geom_point() +
  #scale_shape_manual(values = c(4, 19)) + 
  coord_flip() +
  labs(x = "terms", shape = "significant?") +
  theme(legend.position = "none")
plot(odds.or3)

ggsave(filename = "../figures/odds-ratios-YOFD.tiff", 
       odds.or2, 
       dpi = 300, width = 7, height = 3)
