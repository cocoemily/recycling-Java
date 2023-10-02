### Analysis of model outcomes over time by overlap, mobility, and selection

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

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

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)

###OVERLAP####
avg.two.tech = two.tech %>%
  group_by(model_year) %>%
  summarize(mean.disc = mean(num.retouch.events),
            sd.disc = sd(num.retouch.events),
            n.disc = n()) %>%
  mutate(overlap = 1,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)
avg.multi.tech = multi.tech %>%
  group_by(model_year) %>%
  summarize(mean.disc = mean(num.retouch.events),
            sd.disc = sd(num.retouch.events),
            n.disc = n()) %>%
  mutate(overlap = 2,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)

oplot = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "overlap parameter", x = "model year", y = "average number of retouch events")

# ggsave(filename = "retouch-trend-by-overlap.png", oplot, dpi = 300)

####MU####
avg.mu.1 = mu.1 %>%
  group_by(num_agents, model_year) %>%
  summarize(mean.disc = mean(num.retouch.events),
            sd.disc = sd(num.retouch.events),
            n.disc = n()) %>%
  mutate(mu = 1,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)
avg.mu.2 = mu.2 %>%
  group_by(num_agents, model_year) %>%
  summarize(mean.disc = mean(num.retouch.events),
            sd.disc = sd(num.retouch.events),
            n.disc = n()) %>%
  mutate(mu = 2,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)
avg.mu.3 = mu.3 %>%
  group_by(num_agents, model_year) %>%
  summarize(mean.disc = mean(num.retouch.events),
            sd.disc = sd(num.retouch.events),
            n.disc = n()) %>%
  mutate(mu = 3,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)

mplot = ggplot() +
  geom_line(data = avg.mu.1, aes(x = model_year, y = mean.disc, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.1, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  geom_line(data = avg.mu.2, aes(x = model_year, y = mean.disc, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.2, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  geom_line(data = avg.mu.3, aes(x = model_year, y = mean.disc, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.3, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  facet_wrap(~ num_agents, 
             labeller = labeller(num_agents = occup.labs)) +
  scale_color_colorblind() +
  labs(color = "\u00b5", x = "model year", y = "average number of retouch events")

# ggsave(filename = "retouch-trend-by-mu.png", mplot, dpi = 300)


####SELECTION####
avg.flk.select = flake.selection %>%
  group_by(size_preference, strict_selection, model_year) %>%
  summarize(mean.disc = mean(num.retouch.events), 
            sd.disc = sd(num.retouch.events), 
            n.disc = n()) %>%
  mutate(flake_preference = TRUE, 
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc, 
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)
  

avg.nod.select = nodule.selection %>%
  group_by(size_preference, strict_selection, model_year) %>%
  summarize(mean.disc = mean(num.retouch.events), 
            sd.disc = sd(num.retouch.events), 
            n.disc = n()) %>%
  mutate(flake_preference = FALSE, 
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc, 
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)

splot = ggplot() +
  geom_line(data = avg.flk.select, aes(x = model_year, y = mean.disc, color = as.factor(flake_preference))) + 
  geom_ribbon(data = avg.flk.select, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  geom_line(data = avg.nod.select, aes(x = model_year, y = mean.disc, color = as.factor(flake_preference))) +
  geom_ribbon(data = avg.nod.select, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  facet_grid(size_preference ~ strict_selection, 
             labeller = labeller(strict_selection = strict.labs, size_preference = size.labs)) +
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "average number of retouch events")

#ggsave(filename = "retouch-trend-by-selection.png", splot, dpi = 300)

save(oplot, file = "retouch-overlap.rdata")
save(mplot, file = "retouch-mu.rdata")
save(splot, file = "retouch-selection.rdata")

# ggsave(filename = "retouch-trends.tiff",
#        ggarrange(oplot, mplot, splot, legend = "bottom", ncol = 3, nrow = 1, labels = "AUTO"), 
#        dpi = 300, width = 11, height = 4.5
# )
