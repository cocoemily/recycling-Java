##recycled object counts

library(tidyverse)
library(ggthemes)
library(jtools)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-recycled-object-counts.csv")

#strict.count = count.data %>% filter(strict_selection == TRUE)
flake.count = count.data %>% 
  gather(key = "time", value = "count", recycled.flakes.mid, recycled.flakes.end)

ggplot(flake.count) +
  geom_col(aes(x = time, y = count, fill = as.factor(overlap), group =as.factor(overlap)), position = "dodge") +
  facet_grid(size_preference + strict_selection ~ flake_preference, labeller = label_both) +
  scale_x_discrete(labels = c("end", "middle")) +
  labs(y = "recycled flake count")

nodule.count = count.data %>% 
  gather(key = "time", value = "count", recycled.nodules.mid, recycled.nodules.end)
ggplot(nodule.count) +
  geom_col(aes(x = time, y = count, fill = as.factor(overlap), group =as.factor(overlap)), position = "dodge") +
  facet_grid(size_preference + strict_selection ~ flake_preference, labeller = label_both) +
  scale_x_discrete(labels = c("end", "middle")) +
  labs(y = "recycled nodule count")


ad = rbind(flake.count[,-c(14:15)], nodule.count[,-c(14:15)]) %>%
  separate(time, c("rcycl", "type", "time"), sep ="\\.")

ggplot(ad %>% filter(overlap == 2)) +
  geom_col(aes(x = time, y = count, fill = type)) +
  facet_grid(size_preference + strict_selection ~ flake_preference, labeller = label_both) +
  scale_x_discrete(labels = c("end", "middle")) +
  scale_fill_brewer(palette = "Dark2")

