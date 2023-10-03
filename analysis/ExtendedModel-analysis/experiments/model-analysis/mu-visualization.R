library(tidyverse)
library(ggthemes)
library(ggpubr)

theme_set(theme_void())

mu1 = read.csv("../output/model-testing/run_0_mu1.0_agent-locations.csv")
mu2 = read.csv("../output/model-testing/run_0_mu2.0_agent-locations.csv")
mu3 = read.csv("../output/model-testing/run_0_mu3.0_agent-locations.csv")

background = expand.grid(seq(0,9, by = 1), seq(0,9, by=1))

pmu3 = ggplot(mu3) +
  geom_tile(data = background, aes(x = Var1, y = Var2), color = "red", fill = "white") +
  geom_tile(data = mu3[c(1:nrow(mu3)-1),], aes(x = y, y = x), fill = "grey90", color = "red") +
  geom_path(aes(x = y, y = x), position = position_jitter(w=0.2, h=0.2)) +
  geom_point(data = mu3[1,], aes(x = y, y = x, shape = "start"), size = 4) +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  guides(shape = "none") +
  labs(title = "\u00b5 = 3") +
  xlim(-1,10) +
  ylim(-1,10)
plot(pmu3)
# ggsave(file = "../figures/mu3-viz.jpg", plot = pmu3, dpi = 300, 
#        width = 4, height = 4)

pmu2 = ggplot(mu2) +
  geom_tile(data = background, aes(x = Var1, y = Var2), color = "red", fill = "white") +
  geom_tile(data = mu2[c(1:nrow(mu2)-1),], aes(x = y, y = x), fill = "grey90", color = "red") +
  geom_path(aes(x = y, y = x), position = position_jitter(w=0.2, h=0.2)) +
  geom_point(data = mu2[1,], aes(x = y, y = x, shape = "start"), size = 4) +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  guides(shape = "none") +
  labs(title = "\u00b5 = 2") +
  xlim(-1,10) +
  ylim(-1,10)
plot(pmu2)
# ggsave(file = "../figures/mu2-viz.jpg", plot = pmu2, dpi = 300, 
#        width = 4, height = 4)

mu1[nrow(mu1),] = c(5,10)
pmu1 = ggplot(mu1) +
  geom_tile(data = background, aes(x = Var1, y = Var2), color = "red", fill = "white") +
  geom_tile(data = mu1[c(1:nrow(mu1)-1),], aes(x = y, y = x), fill = "grey90", color = "red") +
  geom_path(aes(x = y, y = x), position = position_jitter(w=0.2, h=0.2)) +
  geom_point(data = mu1[1,], aes(x = y, y = x, shape = "start"), size = 4) +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  guides(shape = "none") +
  labs(title = "\u00b5 = 1") +
  xlim(-1,10) +
  ylim(-1,10)
plot(pmu1)
# ggsave(file = "../figures/mu1-viz.jpg", plot = pmu1, dpi = 300, 
#        width = 4, height = 4)

mu_horz = ggarrange(pmu1, pmu2, pmu3, ncol = 3)
plot(mu_horz)
ggsave(file = "../figures/mu-viz.tiff", plot = ggarrange(pmu1, pmu2, pmu3, ncol = 3), 
       dpi = 300, width = 9, height = 3)

mu_vert = ggarrange(pmu1, pmu2, pmu3, ncol = 1, nrow = 3)
ggsave(file = "../figures/mu-viz_VERT.tiff", plot = ggarrange(pmu1, pmu2, pmu3, ncol = 1, nrow = 3),
       dpi = 300, width = 3, height = 9)
