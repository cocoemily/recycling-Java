##recycled object counts

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(jtools)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")

ad = count.data %>% filter(model_year == 200000 | model_year == 350000) %>%
  pivot_longer(cols = flake.count:nodule.count, names_to = "object", values_to = "count")
ad$object = ifelse(str_detect(ad$object, "flake"), "flakes", "nodules")

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")

p1 = ggplot(ad %>% filter(overlap == 1)) +
  geom_col(aes(x = as.factor(model_year), y = count, fill = object), position = "dodge") +
  facet_grid(flake_preference + size_preference ~ strict_selection , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs
             )) +
  scale_x_discrete(labels = c("middle", "end"), limits = rev) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "time") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))

p2 = ggplot(ad %>% filter(overlap == 2)) +
  geom_col(aes(x = as.factor(model_year), y = count, fill = object), position = "dodge") +
  facet_grid(flake_preference + size_preference ~ strict_selection , 
             labeller = labeller(
               flake_preference = flake.labs, 
               size_preference = size.labs, 
               strict_selection = strict.labs
             )) +
  scale_x_discrete(labels = c("middle", "end"), limits = rev) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "time") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))

ggsave(filename = "../figures/artifact-counts.tiff", 
       ggarrange(p1, p2, common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)

