### Analysis of model outcomes over time by overlap, mobility, and selection

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())
tech.labs = c("two technology types", "many technology types")
names(tech.labs) = c("1", "2")
flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)
mu.labs = c("\u00b5 = 1", "\u00b5 = 2", "\u00b5 = 3")
names(mu.labs) = c(1,2,3)
blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")

alldata = readr::read_csv("/scratch/ec3307/updated-recycling-Java/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

two.tech = alldata[which(alldata$overlap == 1),]
multi.tech = alldata[which(alldata$overlap == 2),]

mu.1 = alldata[which(alldata$mu == 1),]
mu.2 = alldata[which(alldata$mu == 2),]
mu.3 = alldata[which(alldata$mu == 3),]

flake.selection = alldata[which(alldata$flake_preference == TRUE),]
nodule.selection = alldata[which(alldata$flake_preference == FALSE),]

rm(alldata)


###OVERLAP####
avg.two.tech = two.tech %>%
  group_by(model_year) %>%
  summarize(mean.scvg = mean(num.scav.events),
            sd.scvg = sd(num.scav.events),
            n.scvg = n()) %>%
  mutate(overlap = 1,
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg,
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)
avg.multi.tech = multi.tech %>%
  group_by(model_year) %>%
  summarize(mean.scvg = mean(num.scav.events),
            sd.scvg = sd(num.scav.events),
            n.scvg = n()) %>%
  mutate(overlap = 2,
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg,
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)

oplot = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.scvg, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.scvg, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind(labels = tech.labs) +
  labs(x = "model year", y = "average number of scavenging events")

#ggsave(filename = "scavenging-trend-by-overlap.png", oplot, dpi = 300)

####MU####
avg.mu.1 = mu.1 %>%
  group_by(num_agents, model_year) %>%
  summarize(mean.scvg = mean(num.scav.events),
            sd.scvg = sd(num.scav.events),
            n.scvg = n()) %>%
  mutate(mu = 1,
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg,
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)
avg.mu.2 = mu.2 %>%
  group_by(num_agents, model_year) %>%
  summarize(mean.scvg = mean(num.scav.events),
            sd.scvg = sd(num.scav.events),
            n.scvg = n()) %>%
  mutate(mu = 2,
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg,
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)
avg.mu.3 = mu.3 %>%
  group_by(num_agents, model_year) %>%
  summarize(mean.scvg = mean(num.scav.events),
            sd.scvg = sd(num.scav.events),
            n.scvg = n()) %>%
  mutate(mu = 3,
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg,
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)

mplot = ggplot() +
  geom_line(data = avg.mu.1, aes(x = model_year, y = mean.scvg, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.1, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  geom_line(data = avg.mu.2, aes(x = model_year, y = mean.scvg, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.2, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  geom_line(data = avg.mu.3, aes(x = model_year, y = mean.scvg, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.3, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  facet_wrap(~ num_agents, 
             labeller = labeller(num_agents = occup.labs)) +
  scale_color_colorblind(labels = mu.labs) +
  labs(x = "model year", y = "average number of scavenging events")

#ggsave(filename = "scavenging-trend-by-mu.png", mplot, dpi = 300)


####SELECTION####
avg.flk.select = flake.selection %>%
  group_by(size_preference, strict_selection, model_year) %>%
  summarize(mean.scvg = mean(num.scav.events), 
            sd.scvg = sd(num.scav.events), 
            n.scvg = n()) %>%
  mutate(flake_preference = TRUE, 
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg, 
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)
  

avg.nod.select = nodule.selection %>%
  group_by(size_preference, strict_selection, model_year) %>%
  summarize(mean.scvg = mean(num.scav.events), 
            sd.scvg = sd(num.scav.events), 
            n.scvg = n()) %>%
  mutate(flake_preference = FALSE, 
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg, 
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)

splot = ggplot() +
  geom_line(data = avg.flk.select, aes(x = model_year, y = mean.scvg, color = as.factor(flake_preference))) + 
  geom_ribbon(data = avg.flk.select, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  geom_line(data = avg.nod.select, aes(x = model_year, y = mean.scvg, color = as.factor(flake_preference))) +
  geom_ribbon(data = avg.nod.select, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  facet_grid(size_preference ~ strict_selection, 
             labeller = labeller(strict_selection = strict.labs, size_preference = size.labs)) +
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind(labels = flake.labs) +
  labs(x = "model year", y = "average number of scavenging events")

#ggsave(filename = "scavenging-trend-by-selection.png", splot, dpi = 300)

save(oplot, file = "scavenge-overlap.rdata")
save(mplot, file = "scavenge-mu.rdata")
save(splot, file = "scavenge-selection.rdata")

# ggsave(filename = "scavenging-trends.tiff",
#        ggarrange(oplot, mplot, splot, legend = "bottom", ncol = 3, nrow = 1, labels = "AUTO"), 
#        dpi = 300, width = 11, height = 4.5
# )
