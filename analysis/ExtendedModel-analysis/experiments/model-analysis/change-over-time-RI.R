### Analysis of model outcomes over time by overlap and mobility

library(tidyverse)
library(ggthemes)

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

# two.tech = alldata[which(alldata$overlap == 1),]
# multi.tech = alldata[which(alldata$overlap == 2),]
# mu.1 = alldata[which(alldata$mu == 1),]
# mu.2 = alldata[which(alldata$mu == 2),]
# mu.3 = alldata[which(alldata$mu == 3),]
flake.selection = alldata[which(alldata$flake_preference == TRUE),]
nodule.selection = alldata[which(alldata$flake_preference == FALSE),]

rm(alldata)

###OVERLAP####
# avg.two.tech = two.tech %>%
#   group_by(model_year) %>%
#   summarize(mean.RI = mean(total.RI), 
#             sd.RI = sd(total.RI), 
#             n.RI = n()) %>%
#   mutate(overlap = 1, 
#          se.RI = sd.RI / sqrt(n.RI),
#          lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
#          upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
# avg.multi.tech = multi.tech %>%
#   group_by(model_year) %>%
#   summarize(mean.RI = mean(total.RI), 
#             sd.RI = sd(total.RI), 
#             n.RI = n()) %>%
#   mutate(overlap = 2, 
#          se.RI = sd.RI / sqrt(n.RI),
#          lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
#          upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
# 
# oplot = ggplot() +
#   geom_line(data = avg.two.tech, aes(x = model_year, y = mean.RI, color = as.factor(overlap))) + 
#   geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.RI, color = as.factor(overlap))) +
#   geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   scale_x_reverse() +
#   scale_color_colorblind() +
#   labs(color = "overlap parameter", x = "model year", y = "average recycling intensity")
# 
# ggsave(filename = "recycling-intensity-trend-by-overlap.png", oplot, dpi = 300)

####MU####
# avg.mu.1 = mu.1 %>%
#   group_by(model_year) %>%
#   summarize(mean.RI = mean(total.RI), 
#             sd.RI = sd(total.RI), 
#             n.RI = n()) %>%
#   mutate(mu = 1, 
#          se.RI = sd.RI / sqrt(n.RI),
#          lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
#          upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
# avg.mu.2 = mu.2 %>%
#   group_by(model_year) %>%
#   summarize(mean.RI = mean(total.RI), 
#             sd.RI = sd(total.RI), 
#             n.RI = n()) %>%
#   mutate(mu = 2, 
#          se.RI = sd.RI / sqrt(n.RI),
#          lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
#          upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
# avg.mu.3 = mu.3 %>%
#   group_by(model_year) %>%
#   summarize(mean.RI = mean(total.RI), 
#             sd.RI = sd(total.RI), 
#             n.RI = n()) %>%
#   mutate(mu = 3, 
#          se.RI = sd.RI / sqrt(n.RI),
#          lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
#          upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
# 
# mplot = ggplot() +
#   geom_line(data = avg.mu.1, aes(x = model_year, y = mean.RI, color = as.factor(mu))) + 
#   geom_ribbon(data = avg.mu.1, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   geom_line(data = avg.mu.2, aes(x = model_year, y = mean.RI, color = as.factor(mu))) +
#   geom_ribbon(data = avg.mu.2, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   geom_line(data = avg.mu.3, aes(x = model_year, y = mean.RI, color = as.factor(mu))) +
#   geom_ribbon(data = avg.mu.3, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   scale_x_reverse() +
#   scale_color_colorblind() +
#   labs(color = "mu parameter", x = "model year", y = "average recycling intensity")
# 
# ggsave(filename = "recycling-intensity-trend-by-mu.png", mplot, dpi = 300)


####SELECTION####
avg.flk.select = flake.selection %>%
  group_by(size_preference, strict_selection, model_year) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(flake_preference = TRUE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
  

avg.nod.select = nodule.selection %>%
  group_by(size_preference, strict_selection, model_year) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(flake_preference = FALSE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)

splot = ggplot() +
  geom_line(data = avg.flk.select, aes(x = model_year, y = mean.RI, color = as.factor(flake_preference))) + 
  geom_ribbon(data = avg.flk.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  geom_line(data = avg.nod.select, aes(x = model_year, y = mean.RI, color = as.factor(flake_preference))) +
  geom_ribbon(data = avg.nod.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  facet_grid(size_preference ~ strict_selection, labeller = label_both) +
  scale_x_reverse() +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "average recycling intensity")

ggsave(filename = "recycling-intensity-trend-by-selection.png", splot, dpi = 300)