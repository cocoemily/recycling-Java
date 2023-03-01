### Analysis of model outcomes over time by scavenging and blank probabilities

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

two.tech = alldata[which(alldata$overlap == 1),]
multi.tech = alldata[which(alldata$overlap == 2),]

rm(alldata)

blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")

####overlap####
#####recycling intensity####
avg.two.tech = two.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI),
            sd.RI = sd(total.RI),
            n.RI = n()) %>%
  mutate(overlap = 1,
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI,
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
avg.multi.tech = multi.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI),
            sd.RI = sd(total.RI),
            n.RI = n()) %>%
  mutate(overlap = 2,
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI,
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)

p1 = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.RI, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.RI, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_colorblind() +
  labs(color = "overlap parameter", x = "model year", y = "average recycling intensity") +
  theme(strip.text = element_text(size = 5))

# ggsave(filename = "recycling-intensity-trend-by-probs.png", p1, dpi = 300, 
#        height = 7, width = 8)
rm(avg.two.tech, avg.multi.tech)

#####recycling events####
avg.two.tech = two.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made),
            sd.ro = sd(num.rcycl.obj.made),
            n.ro = n()) %>%
  mutate(overlap = 1,
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro,
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)
avg.multi.tech = multi.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made),
            sd.ro = sd(num.rcycl.obj.made),
            n.ro = n()) %>%
  mutate(overlap = 2,
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro,
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)

p2 = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.ro, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.ro, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_colorblind() +
  labs(color = "overlap parameter", x = "model year", y = "average number of recycled objects created") +
  theme(strip.text = element_text(size = 5))

# ggsave(filename = "recycled-objects-trend-by-probs.png", p2, dpi = 300, 
#        height = 7, width = 8)
rm(avg.two.tech, avg.multi.tech)

#####scavenging events####
avg.two.tech = two.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.scvg = mean(num.scav.events),
            sd.scvg = sd(num.scav.events),
            n.scvg = n()) %>%
  mutate(overlap = 1,
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg,
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)
avg.multi.tech = multi.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.scvg = mean(num.scav.events),
            sd.scvg = sd(num.scav.events),
            n.scvg = n()) %>%
  mutate(overlap = 2,
         se.scvg = sd.scvg / sqrt(n.scvg),
         lower.ci.scvg = mean.scvg - qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg,
         upper.ci.scvg = mean.scvg + qt(1 - (0.05 / 2), n.scvg - 1) * se.scvg)

p3 = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.scvg, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.scvg, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.scvg, ymax = upper.ci.scvg), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_colorblind() +
  labs(color = "overlap parameter", x = "model year", y = "average number of scavenging events") +
  theme(strip.text = element_text(size = 5))

# ggsave(filename = "scavenging-trend-by-probs.png", p3, dpi = 300, 
#        height = 7, width = 8)
rm(avg.two.tech, avg.multi.tech)


#####discard events####
avg.two.tech = two.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.disc = mean(num.discard.events),
            sd.disc = sd(num.discard.events),
            n.disc = n()) %>%
  mutate(overlap = 1,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)
avg.multi.tech = multi.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.disc = mean(num.discard.events),
            sd.disc = sd(num.discard.events),
            n.disc = n()) %>%
  mutate(overlap = 2,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)

p4 = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_colorblind() +
  labs(color = "overlap parameter", x = "model year", y = "average number of discard events") +
  theme(strip.text = element_text(size = 5))

# ggsave(filename = "discard-trend-by-probs.png", p4, dpi = 300, 
#        height = 7, width = 8)
rm(avg.two.tech, avg.multi.tech)

#####retouch events####
avg.two.tech = two.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.disc = mean(num.retouch.events),
            sd.disc = sd(num.retouch.events),
            n.disc = n()) %>%
  mutate(overlap = 1,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)
avg.multi.tech = multi.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.disc = mean(num.retouch.events),
            sd.disc = sd(num.retouch.events),
            n.disc = n()) %>%
  mutate(overlap = 2,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)

p5 = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_colorblind() +
  labs(color = "overlap parameter", x = "model year", y = "average number of retouch events") +
  theme(strip.text = element_text(size = 5))

# ggsave(filename = "retouch-trend-by-probs.png", p5, dpi = 300, 
#        height = 7, width = 8)
rm(avg.two.tech, avg.multi.tech)

#####blank creation events####
avg.two.tech = two.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.disc = mean(num.blank.events),
            sd.disc = sd(num.blank.events),
            n.disc = n()) %>%
  mutate(overlap = 1,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)
avg.multi.tech = multi.tech %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.disc = mean(num.blank.events),
            sd.disc = sd(num.blank.events),
            n.disc = n()) %>%
  mutate(overlap = 2,
         se.disc = sd.disc / sqrt(n.disc),
         lower.ci.disc = mean.disc - qt(1 - (0.05 / 2), n.disc - 1) * se.disc,
         upper.ci.disc = mean.disc + qt(1 - (0.05 / 2), n.disc - 1) * se.disc)

p6 = ggplot() +
  geom_line(data = avg.two.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.two.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  geom_line(data = avg.multi.tech, aes(x = model_year, y = mean.disc, color = as.factor(overlap))) +
  geom_ribbon(data = avg.multi.tech, aes(x = model_year, ymin = lower.ci.disc, ymax = upper.ci.disc), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_colorblind() +
  labs(color = "overlap parameter", x = "model year", y = "average number of blanks produced") +
  theme(strip.text = element_text(size = 5))

# ggsave(filename = "blanks-trend-by-probs.png", p6, dpi = 300, 
#        height = 7, width = 8)
rm(avg.two.tech, avg.multi.tech)
rm(two.tech, multi.tech)

####mu####
mu.1 = alldata[which(alldata$mu == 1),]
mu.2 = alldata[which(alldata$mu == 2),]
mu.3 = alldata[which(alldata$mu == 3),]


#####recycling intensity####
avg.mu.1 = mu.1 %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI),
            sd.RI = sd(total.RI),
            n.RI = n()) %>%
  mutate(mu = 1,
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI,
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
avg.mu.2 = mu.2 %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI),
            sd.RI = sd(total.RI),
            n.RI = n()) %>%
  mutate(mu = 2,
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI,
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
avg.mu.3 = mu.3 %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI),
            sd.RI = sd(total.RI),
            n.RI = n()) %>%
  mutate(mu = 3,
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI,
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)

mplot1 = ggplot() +
  geom_line(data = avg.mu.1, aes(x = model_year, y = mean.RI, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.1, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  geom_line(data = avg.mu.2, aes(x = model_year, y = mean.RI, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.2, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  geom_line(data = avg.mu.3, aes(x = model_year, y = mean.RI, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.3, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "mu parameter", x = "model year", y = "average recycling intensity")

# ggsave(filename = "recycling-intensity-trend-by-mu.png", mplot, dpi = 300)

#####recycling events####
avg.mu.1 = mu.1 %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made),
            sd.ro = sd(num.rcycl.obj.made),
            n.ro = n()) %>%
  mutate(mu = 1,
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro,
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)
avg.mu.2 = mu.2 %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made),
            sd.ro = sd(num.rcycl.obj.made),
            n.ro = n()) %>%
  mutate(mu = 2,
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro,
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)
avg.mu.3 = mu.3 %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made),
            sd.ro = sd(num.rcycl.obj.made),
            n.ro = n()) %>%
  mutate(mu = 3,
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro,
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)

mplot2 = ggplot() +
  geom_line(data = avg.mu.1, aes(x = model_year, y = mean.ro, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.1, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  geom_line(data = avg.mu.2, aes(x = model_year, y = mean.ro, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.2, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  geom_line(data = avg.mu.3, aes(x = model_year, y = mean.ro, color = as.factor(mu))) +
  geom_ribbon(data = avg.mu.3, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "mu parameter", x = "model year", y = "average number of recycled objects created")

# ggsave(filename = "recycled-objects-trend-by-mu.png", mplot, dpi = 300)

rm(mu.1, mu.2, mu.3)

####flake preference####
flake.selection = alldata[which(alldata$flake_preference == TRUE),]
nodule.selection = alldata[which(alldata$flake_preference == FALSE),]

#####recycling intensity####
avg.flk.select = flake.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(flake_preference = TRUE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)


avg.nod.select = nodule.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(flake_preference = FALSE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)

splot1 = ggplot() +
  geom_line(data = avg.flk.select, aes(x = model_year, y = mean.RI, color = as.factor(flake_preference))) + 
  geom_ribbon(data = avg.flk.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  geom_line(data = avg.nod.select, aes(x = model_year, y = mean.RI, color = as.factor(flake_preference))) +
  geom_ribbon(data = avg.nod.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "average recycling intensity")

#####recycled objects####
avg.flk.select = flake.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made), 
            sd.ro = sd(num.rcycl.obj.made), 
            n.ro = n()) %>%
  mutate(flake_preference = TRUE, 
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro, 
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)


avg.nod.select = nodule.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made), 
            sd.ro = sd(num.rcycl.obj.made), 
            n.ro = n()) %>%
  mutate(flake_preference = FALSE, 
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro, 
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)

splot2 = ggplot() +
  geom_line(data = avg.flk.select, aes(x = model_year, y = mean.ro, color = as.factor(flake_preference))) + 
  geom_ribbon(data = avg.flk.select, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  geom_line(data = avg.nod.select, aes(x = model_year, y = mean.ro, color = as.factor(flake_preference))) +
  geom_ribbon(data = avg.nod.select, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "flake preference", x = "model year", y = "average number of recycled objects created")

# ggsave(filename = "recycled-objects-trend-by-selection.png", splot, dpi = 300)

rm(flake.selection, nodule.selection)

####size preference####
size.selection = alldata[which(alldata$size_preference == TRUE),]
nosize.selection = alldata[which(alldata$size_preference == FALSE),]

#####recycling intensity####
avg.size.select = size.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(size_preference = TRUE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)


avg.nsize.select = nosize.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(size_preference = FALSE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)

ssplot1 = ggplot() +
  geom_line(data = avg.size.select, aes(x = model_year, y = mean.RI, color = as.factor(size_preference))) + 
  geom_ribbon(data = avg.size.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  geom_line(data = avg.nsize.select, aes(x = model_year, y = mean.RI, color = as.factor(size_preference))) +
  geom_ribbon(data = avg.nsize.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "size preference", x = "model year", y = "average recycling intensity")

#####recycled objects####
avg.size.select = size.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made), 
            sd.ro = sd(num.rcycl.obj.made), 
            n.ro = n()) %>%
  mutate(size_preference = TRUE, 
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro, 
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)


avg.nsize.select = nosize.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made), 
            sd.ro = sd(num.rcycl.obj.made), 
            n.ro = n()) %>%
  mutate(size_preference = FALSE, 
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro, 
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)

ssplot2 = ggplot() +
  geom_line(data = avg.size.select, aes(x = model_year, y = mean.ro, color = as.factor(size_preference))) + 
  geom_ribbon(data = avg.size.select, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  geom_line(data = avg.nsize.select, aes(x = model_year, y = mean.ro, color = as.factor(size_preference))) +
  geom_ribbon(data = avg.nsize.select, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "size preference", x = "model year", y = "average number of recycled objects created")

# ggsave(filename = "recycled-objects-trend-by-selection.png", splot, dpi = 300)

rm(size.selection, nosize.selection)

####strict selection####
strict.selection = alldata[which(alldata$strict_selection == TRUE),]
nostrict.selection = alldata[which(alldata$strict_selection == FALSE),]

#####recycling intensity####
avg.strict.select = strict.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(strict_selection = TRUE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)


avg.nostrict.select = nostrict.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.RI = mean(total.RI), 
            sd.RI = sd(total.RI), 
            n.RI = n()) %>%
  mutate(strict_selection = FALSE, 
         se.RI = sd.RI / sqrt(n.RI),
         lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI, 
         upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)

ssp1 = ggplot() +
  geom_line(data = avg.strict.select, aes(x = model_year, y = mean.RI, color = as.factor(strict_selection))) + 
  geom_ribbon(data = avg.strict.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  geom_line(data = avg.nostrict.select, aes(x = model_year, y = mean.RI, color = as.factor(strict_selection))) +
  geom_ribbon(data = avg.nostrict.select, aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "strict selection", x = "model year", y = "average recycling intensity")

#####recycled objects####
avg.strict.select = strict.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made), 
            sd.ro = sd(num.rcycl.obj.made), 
            n.ro = n()) %>%
  mutate(flake_preference = TRUE, 
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro, 
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)


avg.nostrict.select = nostrict.selection %>%
  group_by(model_year, blank_prob, scavenge_prob) %>%
  summarize(mean.ro = mean(num.rcycl.obj.made), 
            sd.ro = sd(num.rcycl.obj.made), 
            n.ro = n()) %>%
  mutate(flake_preference = FALSE, 
         se.ro = sd.ro / sqrt(n.ro),
         lower.ci.ro = mean.ro - qt(1 - (0.05 / 2), n.ro - 1) * se.ro, 
         upper.ci.ro = mean.ro + qt(1 - (0.05 / 2), n.ro - 1) * se.ro)

ssp2 = ggplot() +
  geom_line(data = avg.strict.select, aes(x = model_year, y = mean.ro, color = as.factor(strict_selection))) + 
  geom_ribbon(data = avg.strict.select, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  geom_line(data = avg.nostrict.select, aes(x = model_year, y = mean.ro, color = as.factor(strict_selection))) +
  geom_ribbon(data = avg.nostrict.select, aes(x = model_year, ymin = lower.ci.ro, ymax = upper.ci.ro), alpha = 0.2) +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs
  )) +
  scale_x_reverse(breaks = c(500000, 350000, 200000), 
                  labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "strict selection", x = "model year", y = "average number of recycled objects created")

# ggsave(filename = "recycled-objects-trend-by-selection.png", splot, dpi = 300)

rm(strict.selection, nostrict.selection)


####recycling trends plots####
# grid = ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = T, legend = "bottom", labels = "AUTO")
# ggsave(filename = "trends-by-probs.tiff", grid,
#        dpi = 300, width = 12, height = 9)

ogrid = ggarrange(p3, p4, p5, p6, nrow = 2, ncol = 2, common.legend = T, legend = "bottom", labels = "AUTO")
ggsave(filename = "other-trends-by-probs_overlap.tiff", grid,
       dpi = 300, width = 12, height = 6)

rgrid = ggarrange(p1, p2, mplot1, mplot2, splot1, splot2, ssplot1, ssplot2, ssp1, ssp2,
                  nrow = 2, ncol = 5, common.legend = T, 
                  legend = "bottom", labels = "AUTO")
ggsave(filename = "recycling-trends-by-probs.tiff", grid,
       dpi = 300, width = 12, height = 18)


#### recycling intensity by probabilities and selection ####
# avg.two.tech = two.tech %>%
#   group_by(model_year, blank_prob, scavenge_prob, strict_selection, size_preference) %>%
#   summarize(mean.RI = mean(total.RI),
#             sd.RI = sd(total.RI),
#             n.RI = n()) %>%
#   mutate(overlap = 1,
#          se.RI = sd.RI / sqrt(n.RI),
#          lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI,
#          upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
# avg.multi.tech = multi.tech %>%
#   group_by(model_year, blank_prob, scavenge_prob, strict_selection, size_preference) %>%
#   summarize(mean.RI = mean(total.RI),
#             sd.RI = sd(total.RI),
#             n.RI = n()) %>%
#   mutate(overlap = 2,
#          se.RI = sd.RI / sqrt(n.RI),
#          lower.ci.RI = mean.RI - qt(1 - (0.05 / 2), n.RI - 1) * se.RI,
#          upper.ci.RI = mean.RI + qt(1 - (0.05 / 2), n.RI - 1) * se.RI)
# 
# ssp = ggplot() +
#   geom_line(data = avg.two.tech %>% filter(strict_selection == TRUE), aes(x = model_year, y = mean.RI, color = as.factor(overlap))) +
#   geom_ribbon(data = avg.two.tech %>% filter(strict_selection == TRUE), aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   geom_line(data = avg.multi.tech %>% filter(strict_selection == TRUE), aes(x = model_year, y = mean.RI, color = as.factor(overlap))) +
#   geom_ribbon(data = avg.multi.tech %>% filter(strict_selection == TRUE), aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
#     blank_prob = blank.labs, scavenge_prob = scvg.labs
#   )) +
#   scale_x_reverse(breaks = c(500000, 350000, 200000), 
#                   labels = label_number(scale_cut = cut_short_scale())) +
#   scale_y_continuous(labels = label_number(accuracy = 0.01)) +
#   scale_color_colorblind() +
#   labs(color = "overlap parameter", x = "model year", y = "average recycling intensity") +
#   theme(strip.text = element_text(size = 5))
# 
# nsp = ggplot() +
#   geom_line(data = avg.two.tech %>% filter(strict_selection == FALSE), aes(x = model_year, y = mean.RI, color = as.factor(overlap))) +
#   geom_ribbon(data = avg.two.tech %>% filter(strict_selection == FALSE), aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   geom_line(data = avg.multi.tech %>% filter(strict_selection == FALSE), aes(x = model_year, y = mean.RI, color = as.factor(overlap))) +
#   geom_ribbon(data = avg.multi.tech %>% filter(strict_selection == FALSE), aes(x = model_year, ymin = lower.ci.RI, ymax = upper.ci.RI), alpha = 0.2) +
#   facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
#     blank_prob = blank.labs, scavenge_prob = scvg.labs
#   )) +
#   scale_x_reverse(breaks = c(500000, 350000, 200000), 
#                   labels = label_number(scale_cut = cut_short_scale())) +
#   scale_y_continuous(labels = label_number(accuracy = 0.01)) +
#   scale_color_colorblind() +
#   labs(color = "overlap parameter", x = "model year", y = "average recycling intensity") +
#   theme(strip.text = element_text(size = 5))
# 
# grid = ggarrange(ssp, nsp, nrow = 1, ncol = 2, common.legend = T, legend = "bottom", labels = "AUTO")
# ggsave(filename = "trends-by-probs-and-selection.tiff", grid,
#        dpi = 300, width = 12, height = 9)
