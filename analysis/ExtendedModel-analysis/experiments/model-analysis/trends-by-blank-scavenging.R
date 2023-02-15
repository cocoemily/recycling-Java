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

####recycling intensity####
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

####recycling events####
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

####scavenging events####
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


####discard events####
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

####retouch events####
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

####blank creation events####
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

grid = ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = T, legend = "bottom")
ggsave(filename = "trends-by-probs.tiff", grid,
       dpi = 300, width = 12, height = 9)
