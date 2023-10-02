library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/updated-recycling-Java/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

data = alldata[which(alldata$overlap == 1 & alldata$num_agents == 100),]

mu.1 = data[which(data$mu == 1),]
mu.2 = data[which(data$mu == 2),]
mu.3 = data[which(data$mu == 3),]

rm(alldata)

blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")

####mu####
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
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "\u00b5", x = "model year", y = "average recycling intensity")

save(mplot1, file = "ri-mu.rdata")

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
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind() +
  labs(color = "\u00b5", x = "model year", y = "average number of recycled objects created")

save(mplot2, file = "re-mu.rdata")
