### Analysis of model outcomes over time by scavenging and blank probabilities

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_bw())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

size.selection = alldata[which(alldata$size_preference == TRUE),]
nosize.selection = alldata[which(alldata$size_preference == FALSE),]

rm(alldata)

blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")

####size preference####
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

save(ssplot1, file = "ri-size.rdata")

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

save(ssplot2, file = "re-size.rdata")
