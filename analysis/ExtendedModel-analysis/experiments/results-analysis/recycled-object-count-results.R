##recycled object counts

library(tidyverse)
library(ggthemes)
library(jtools)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-recycled-object-counts.csv")

#what is happening when size_preference is true and flake_preference is false
check.data = count.data %>% filter(flake_preference == F & size_preference == T & strict_selection == T)
summary(check.data$recycled.flakes.end)
summary(check.data$recycled.nodules.end)
#when there is size preferences on the nodules -- no recycling of artifacts happens

#strict.count = count.data %>% filter(strict_selection == TRUE)
flake.count = count.data %>% 
  gather(key = "time", value = "count", recycled.flakes.mid, recycled.flakes.end)

ggplot(flake.count) +
  geom_col(aes(x = time, y = count, fill = as.factor(overlap), group =as.factor(overlap)), position = "dodge") +
  facet_grid(size_preference + flake_preference ~ strict_selection, labeller = label_both) +
  scale_x_discrete(labels = c("end", "middle")) +
  labs(y = "recycled flake count")

nodule.count = count.data %>% 
  gather(key = "time", value = "count", recycled.nodules.mid, recycled.nodules.end)
ggplot(nodule.count) +
  geom_col(aes(x = time, y = count, fill = as.factor(overlap), group =as.factor(overlap)), position = "dodge") +
  facet_grid(size_preference + flake_preference ~ strict_selection, labeller = label_both) +
  scale_x_discrete(labels = c("end", "middle")) +
  labs(y = "recycled nodule count")
