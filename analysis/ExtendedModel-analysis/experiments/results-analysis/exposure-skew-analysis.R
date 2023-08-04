library(tidyverse)
library(ggthemes)
library(ggpubr)
library(ggpmisc)

theme_set(theme_bw())

flake.labs = c("flake preference", "nodule preference")
names(flake.labs) = c("TRUE", "FALSE")
size.labs = c("size preference", "no size preference")
names(size.labs) = c("TRUE", "FALSE")
strict.labs = c("strict selection", "no strict selection")
names(strict.labs) = c("TRUE", "FALSE")
blank.labs = c("blank probability: 0.25", "blank probability: 0.50", "blank probability: 0.75")
names(blank.labs) = c("0.25", "0.5", "0.75")
scvg.labs = c("scavenging probability: 0.25", "scavenging probability: 0.50", "scavenging probability: 0.75")
names(scvg.labs) = c("0.25", "0.5", "0.75")
mu.labs = c("mu = 1", "mu = 2", "mu = 3")
names(mu.labs) = c("1", "2", "3")

data = read_csv("~/eclipse-workspace/recycling-Java/results/all-object-counts.csv")
parameters = colnames(data[,c(10:22)])


skew = data %>%
  group_by_at(c(parameters, "row", "col", "run")) %>%
  summarize(ri = sum(count_recycled)/sum(total_count), 
            skew = mean(skew))

sub.skew = skew %>% 
  filter(overlap == 1) %>%
  filter(num_agents == 200)
  

da.lr2 = ggplot(sub.skew, aes(x = skew, y = log(count_recycled))) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs, 
    mu = mu.labs
  )) +
  labs(x = "skewness of distribution of years of initial discard", 
       y = "log(recycled artifact count)") +
  theme(legend.position = "none")

ggsave(filename = "SKEW_age-of-discard.tiff",
       plot = da.lr2,
       dpi = 300, width = 7, height = 6)


p2 = ggplot(sub.skew, aes(x = skew, y = log(count_recycled))) +
  geom_point(color = "grey90", size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  # facet_grid(mu ~ flake_preference  + size_preference + strict_selection,
  #            labeller = labeller(
  #              flake_preference = flake.labs,
  #              size_preference = size.labs,
  #              strict_selection = strict.labs, 
  #              mu = mu.labs
  #            )) +
  labs(x = "skewness of distribution of years of initial discard", 
       y = "log(recycled artifact count)") +
  theme(legend.position = "none")
#plot(p2)

save(p2, file = "SUPP-skew.rdata")
