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
  "mu",
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

ggsave(filename = "../figures/odds-ratios-YOFD.tiff", 
       odds.or, 
       dpi = 300, width = 5, height = 3)

