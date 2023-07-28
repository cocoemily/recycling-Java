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

data = read_csv("~/eclipse-workspace/recycling-Java/results/object-counts-with-skew.csv")
parameters = colnames(data[,c(8:19)])

#problems(data)

skew = data %>% filter(time == "end") %>%
  group_by_at(c(parameters, "row", "col", "run")) %>%
  summarize(count_recycled = sum(count_recycled),
            ri = sum(count_recycled)/sum(total_count), 
            skew = mean(skew))

# ggplot(skew, aes(x = skew, y = ri)) +
#   stat_poly_line() +
#   stat_poly_eq(use_label(c("R2")), color = "grey40") +
#   facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
#     blank_prob = blank.labs, scavenge_prob = scvg.labs, 
#     mu = mu.labs
#   ))

# da.lr = ggplot(skew, aes(x = skew, y = log(count_recycled))) +
#   #geom_point(size = 0.1, alpha = 0.25, color = "cadetblue") +
#   stat_poly_line() +
#   stat_poly_eq(use_label(c("R2")), color = "grey40") +
#   facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
#     blank_prob = blank.labs, scavenge_prob = scvg.labs, 
#     mu = mu.labs
#   )) +
#   labs(x = "skewness of distribution of years of initial discard", 
#        y = "log(recycled artifact count)")
# ggsave(filename = "../figures/recycled-obj_age-of-discard.tiff", 
#        plot = da.lr, 
#        dpi = 300, width = 7, height = 6)

p2 = ggplot(skew, aes(x = skew, y = log(count_recycled))) +
  geom_point(mapping = aes(color = interaction(row, col)), size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  facet_grid(mu ~ flake_preference  + size_preference + strict_selection,
             labeller = labeller(
               flake_preference = flake.labs,
               size_preference = size.labs,
               strict_selection = strict.labs, 
               mu = mu.labs
             )) +
  labs(x = "skewness of distribution of years of initial discard", 
       y = "log(recycled artifact count)") +
  theme(legend.position = "none")
#plot(p2)

ggsave(filename = "../figures/supplementary-figures/recycled-obj_age-of-discard_by-mobility-selection.tiff",
       plot = p2,
       dpi = 300, width = 10, height = 6)


da.lr2 = ggplot(skew, aes(x = skew, y = log(count_recycled))) +
  geom_point(mapping = aes(color = interaction(row, col)), size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs, 
    mu = mu.labs
  )) +
  labs(x = "skewness of distribution of years of initial discard", 
       y = "log(recycled artifact count)") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3), nrow=10, byrow=F))
legend = get_legend(da.lr2)
ggsave(filename = "../figures/supplementary-figures/TEST_legend.tiff", 
       plot = legend, 
       dpi = 300)

da.lr2 = ggplot(skew, aes(x = skew, y = log(count_recycled))) +
  geom_point(mapping = aes(color = interaction(row, col)), size = 0.1, alpha = 0.25) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")), color = "grey40") +
  facet_grid(blank_prob ~ scavenge_prob, labeller = labeller(
    blank_prob = blank.labs, scavenge_prob = scvg.labs, 
    mu = mu.labs
  )) +
  labs(x = "skewness of distribution of years of initial discard", 
       y = "log(recycled artifact count)") +
  theme(legend.position = "none")
ggsave(filename = "../figures/recycled-obj_age-of-discard.tiff",
       plot = da.lr2,
       dpi = 300, width = 7, height = 6)
