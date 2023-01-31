##recycled object counts

library(tidyverse)
library(ggthemes)
library(jtools)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")


# ggplot(count.data) +
#   geom_smooth(aes(x = model_year, y = flake.count, color =as.factor(overlap), group =as.factor(overlap))) +
#   facet_grid(size_preference + strict_selection ~ flake_preference, labeller = label_both) +
#   labs(y = "flake count") +
#   scale_x_reverse()

# ggplot(count.data) +
#   geom_smooth(aes(x = model_year, y = nodule.count, color =as.factor(overlap), group =as.factor(overlap))) +
#   facet_grid(size_preference + strict_selection ~ flake_preference, labeller = label_both) +
#   labs(y = "nodule count") +
#   scale_x_reverse()


ad = count.data %>% select(model_year == 500000 | model_year == 350000) %>%
  gather(key = "object", value = "count", flake.count, nodule.count)

ggplot(ad %>% filter(overlap == 1)) +
  geom_col(aes(x = time, y = count, fill = type)) +
  facet_grid(flake_preference + size_preference ~ strict_selection , labeller = label_both) +
  scale_x_discrete(labels = c("middle", "end")) +
  scale_fill_brewer(palette = "Dark2")

