##recycled object counts

library(tidyverse)
library(ggthemes)
library(jtools)
library(ggpubr)

theme_set(theme_bw())

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-recycled-object-counts.csv")

#strict.count = count.data %>% filter(strict_selection == TRUE)
flake.count = count.data %>% 
  gather(key = "time", value = "count", recycled.flakes.mid, recycled.flakes.end)

# ggplot(flake.count) +
#   geom_col(aes(x = time, y = count, fill = as.factor(overlap), group =as.factor(overlap)), position = "dodge") +
#   facet_grid(size_preference + strict_selection ~ flake_preference, labeller = label_both) +
#   scale_x_discrete(labels = c("end", "middle")) +
#   labs(y = "recycled flake count")

nodule.count = count.data %>% 
  gather(key = "time", value = "count", recycled.nodules.mid, recycled.nodules.end)
# ggplot(nodule.count) +
#   geom_col(aes(x = time, y = count, fill = as.factor(overlap), group =as.factor(overlap)), position = "dodge") +
#   facet_grid(size_preference + strict_selection ~ flake_preference, labeller = label_both) +
#   scale_x_discrete(labels = c("end", "middle")) +
#   labs(y = "recycled nodule count")


ad = rbind(flake.count[,-c(14:15)], nodule.count[,-c(14:15)]) %>%
  separate(time, c("rcycl", "type", "time"), sep ="\\.")
ad$time = factor(ad$time, levels = c("mid", "end"))

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")

p1 = ggplot(ad %>% filter(overlap == 2)) +
  geom_col(aes(x = time, y = count, fill = type)) +
  facet_grid(flake_preference + size_preference ~ strict_selection , 
             labeller = labeller(flake_preference = flake.labs, 
                                 size_preference = size.labs, 
                                 strict_selection = strict.labs)) +
  scale_x_discrete(labels = c("middle", "end")) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))
plot(p1)

p2 = ggplot(ad %>% filter(overlap == 1)) +
  geom_col(aes(x = time, y = count, fill = type)) +
  facet_grid(flake_preference + size_preference ~ strict_selection , 
             labeller = labeller(flake_preference = flake.labs, 
                                 size_preference = size.labs, 
                                 strict_selection = strict.labs)) +
  scale_x_discrete(labels = c("middle", "end")) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6))
plot(p2)

ggsave(filename = "../figures/recycled-object-counts.tiff", 
       plot = ggarrange(p1, p2, 
                        common.legend = T, labels = "AUTO"), 
       dpi = 300, width = 10, height = 6)
