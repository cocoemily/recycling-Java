### Analysis of model outcomes over time by scavenging and blank probabilities

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

data = alldata[which(alldata$overlap == 1 & alldata$num_agents == 100),]

flake.selection = data[which(data$flake_preference == TRUE),]
nodule.selection = data[which(data$flake_preference == FALSE),]

rm(alldata)


####flake preference####
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
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind(labels = flake.labs) +
  labs(x = "model year", y = "average recycling incidence")

save(splot1, file = "ri-flake.rdata")

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
  scale_x_reverse(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_colorblind(labels = flake.labs) +
  labs(x = "model year", y = "average number of recycled objects created")

save(splot2, file = "re-flake.rdata")
