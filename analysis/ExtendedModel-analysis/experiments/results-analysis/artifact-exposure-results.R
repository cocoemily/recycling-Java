##ANALYSIS OF ARTIFACT EXPOSURE

library(tidyverse)
library(ggthemes)
library(jtools)
library(ggpubr)
library(Dict)

theme_set(theme_bw())

exposure.data = read_csv("~/eclipse-workspace/recycling-Java/results/artifact-exposure-Wilcoxon-results.csv")
parameters = colnames(exposure.data[,c(4:16)])

##recycled.signif.greater = whether (TRUE) or not (FALSE) the recycled artifacts 
## have ~significantly~ greater initial discard dates than the non-recycled artifacts

two.tech = exposure.data %>% filter(overlap == 1)
many.tech = exposure.data %>% filter(overlap == 2)


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

exposure.data[,c(4:16)] = lapply(exposure.data[,c(4:16)], factor)
exposure.data$blank_prob = factor(exposure.data$blank_prob, levels = c(0.5, 0.25, 0.75))
exposure.data$scavenge_prob = factor(exposure.data$scavenge_prob, levels = c(0.5, 0.25, 0.75))
r.params = parameters[c(1:3,5:13)]

fit1 = glm(recycled.signif.greater ~ ., exposure.data[,c(r.params, "recycled.signif.greater")], family = "binomial")
summary(fit1)
fit1.df = tidy(fit1)

#fit1.df$signif = ifelse(fit1.df$p.value < 0.05, TRUE, FALSE)
fit1.df$odds = exp(fit1.df$estimate)
fit1.df$lower = fit1.df$odds - exp(fit1.df$std.error)
fit1.df$upper = fit1.df$odds + exp(fit1.df$std.error)



x.labs = c(
  "intercept", 
  "manufacture events: 30 vs. manufacture events: 15", 
  "carry capacity: 20 vs. carry capacity: 10", 
  "max. flake size: 2 vs. max. flake size: 1", 
  "blank probability: 0.25 vs. blank probability: 0.5", 
  "blank probability: 0.75 vs. blank probability: 0.5", 
  "scavenging probability: 0.25 vs. scavenging probability: 0.5", 
  "scavenging probability: 0.75 vs. scavenging probability: 0.5", 
  "many technologies vs. two technologies", 
  "mu: 2 vs. mu: 1", 
  "mu: 3 vs. mu: 1", 
  "number of agents: 200 vs number of agents: 100", 
  "size preference vs. no size preference", 
  "flake preference vs. nodule preference", 
  "min. selectable flake size: 2 vs. min. selectable flake size: 1",
  "strict selection vs. non-strict selection"
)
names(x.labs) = unique(fit1.df$term)

fit1.df$term = factor(fit1.df$term, 
                      levels = c(
                        "(Intercept)",
                        "overlap2",
                        "num_agents200",
                        "mu2",
                        "mu3",
                        "max_use_intensity30",     
                        "max_artifact_carry20",
                        "max_flake_size2",         
                        "blank_prob0.25",
                        "blank_prob0.75",          
                        "scavenge_prob0.25",
                        "scavenge_prob0.75",
                        "flake_preferenceTRUE",
                        "size_preferenceTRUE",
                        "min_suitable_flake_size2",
                        "strict_selectionTRUE" 
                      ))


odds.or = ggplot(fit1.df %>% filter(term != "(Intercept)"), aes(y = odds, x = term)) +
  geom_hline(aes(yintercept = 1), color = I("red"), linetype = "dashed") +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), position = position_dodge2(width = 0.5), width = 0.2) +
  geom_point(position = position_dodge2(width = 0.5)) +
  scale_color_colorblind() +
  scale_x_discrete(labels = x.labs) +
  coord_flip() +
  labs(x = "terms", shape = "significant?") +
  theme(legend.position = "none", axis.text.y = element_text(size = 8))
plot(odds.or)

ggsave(filename = "../figures/odds-ratios-YOFD.tiff",
       odds.or,
       dpi = 300, width = 8, height = 4)



