library(tidyverse)
library(rcompanion)
library(lme4)
library(pbkrtest)

theme_set(theme_bw())

#spatial change analysis between start, middle, end of model run

layers.change = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-spatial-change.csv")

layers.change$time = ifelse(layers.change$timeperiod == "midgrid", 1, 2)

change.long = layers.change %>%
  pivot_longer(cols = starts_with("change."), 
               names_to = "output", values_to = "value")

# ggplot(layers.change) +
#   geom_boxplot(aes(x = time, y = change.RI, group = time)) +
#   geom_smooth(aes(x = as.numeric(time), y = change.RI), method = "glm", se = T) +
#   facet_grid(row ~ col, labeller = label_both)

####MIXED MODELS####
RI.fit0 = lmer(change.RI ~(1 | row:col), data = layers.change)
RI.fit = lmer(change.RI ~ time + (1 | row:col), data = layers.change)
summary(RI.fit)
#bootMer(x=RI.fit,FUN=fixef,nsim=50)
RI.lrt = PBmodcomp(RI.fit, RI.fit0 ,nsim=20)[["test"]]$p.value[2]

CR.fit0 = lmer(change.CR ~ (1 | row:col), data = layers.change)
CR.fit = lmer(change.CR ~ time + (1 | row:col), data = layers.change)
summary(CR.fit)
CR.lrt = PBmodcomp(CR.fit, CR.fit0 ,nsim=20)[["test"]]$p.value[2]

nc.fit0 = lmer(change.nod.cnt ~ (1 | row:col), data = layers.change)
nc.fit = lmer(change.nod.cnt ~ time + (1 | row:col), data = layers.change)
summary(nc.fit)
nc.lrt = PBmodcomp(nc.fit, nc.fit0 ,nsim=20)[["test"]]$p.value[2]

fc.fit0 = lmer(change.flk.cnt ~ (1 | row:col), data = layers.change)
fc.fit = lmer(change.flk.cnt ~ time + (1 | row:col), data = layers.change)
summary(fc.fit)
fc.lrt = PBmodcomp(fc.fit, fc.fit0 ,nsim=20)[["test"]]$p.value[2]

nd.fit0 = lmer(change.num.discards ~ (1 | row:col), data = layers.change)
nd.fit = lmer(change.num.discards ~ time + (1 | row:col), data = layers.change)
summary(nd.fit)
nd.lrt = PBmodcomp(nd.fit, nd.fit0 ,nsim=20)[["test"]]$p.value[2]

ns.fit0 = lmer(change.num.scavenge ~ (1 | row:col), data = layers.change)
ns.fit = lmer(change.num.scavenge ~ time + (1 | row:col), data = layers.change)
summary(ns.fit)
ns.lrt = PBmodcomp(ns.fit, ns.fit0 ,nsim=20)[["test"]]$p.value[2]

ne.fit0 = lmer(change.num.encounter ~ (1 | row:col), data = layers.change)
ne.fit = lmer(change.num.encounter ~ time + (1 | row:col), data = layers.change)
summary(ne.fit)
ne.lrt = PBmodcomp(ne.fit, ne.fit0 ,nsim=20)[["test"]]$p.value[2]

nr.fit0 = lmer(change.num.retouch ~ (1 | row:col), data = layers.change)
nr.fit = lmer(change.num.retouch ~ time + (1 | row:col), data = layers.change)
summary(nr.fit)
nr.lrt = PBmodcomp(nr.fit, nr.fit0 ,nsim=20)[["test"]]$p.value[2]

#### plots ####
RI.plot = ggplot(change.long %>% filter(output == "change.RI")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 0.25, label = paste("italic(P", ifelse(RI.lrt < 0.05, "<", ">"), "0.05)"), parse = T) +
  labs(y = "recycling intensity") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

CR.plot = ggplot(change.long %>% filter(output == "change.CR")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 2, label = paste("italic(P", ifelse(CR.lrt < 0.05, "<", ">"), "0.05)"), parse = T)+
  labs(y = "cortex ratio") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

nc.plot = ggplot(change.long %>% filter(output == "change.nod.cnt")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 75, label = paste("italic(P", ifelse(nc.lrt < 0.05, "<", ">"), "0.05)"), parse = T) +
  labs(y = "nodule counts") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

fc.plot = ggplot(change.long %>% filter(output == "change.flk.cnt")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 175, label = paste("italic(P", ifelse(fc.lrt < 0.05, "<", ">"), "0.05)"), parse = T) +
  labs(y = "flake counts") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

nd.plot = ggplot(change.long %>% filter(output == "change.num.discards")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 300, label = paste("italic(P", ifelse(nd.lrt < 0.05, "<", ">"), "0.05)"), parse = T) +
  labs(y = "number of discards") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

ns.plot = ggplot(change.long %>% filter(output == "change.num.scavenge")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 175, label = paste("italic(P", ifelse(ns.lrt < 0.05, "<", ">"), "0.05)"), parse = T) +
  labs(y = "number of scavenging events") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

ne.plot = ggplot(change.long %>% filter(output == "change.num.encounter")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 20, label = paste("italic(P", ifelse(ne.lrt < 0.05, "<", ">"), "0.05)"), parse = T) +
  labs(y = "layer encounters") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

nr.plot = ggplot(change.long %>% filter(output == "change.num.retouch")) +
  geom_boxplot(aes(x = time, y = value, group = time)) +
  geom_smooth(aes(x = time, y = value), method = "glm", se = T) +
  scale_x_continuous(breaks = c(1, 2), labels = c("change from start to middle", "change from middle to end")) +
  annotate("text", x = 1.5, y = 225, label = paste("italic(P", ifelse(nr.lrt < 0.05, "<", ">"), "0.05)"), parse = T) +
  labs(y = "number of retouches") +
  theme(axis.title = element_text(size = 6.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 5))

ggsave(filename = "../figures/supplementary-figures/change-over-time.tiff",
       ggarrange(RI.plot, CR.plot, nc.plot, fc.plot, nd.plot, ns.plot, ne.plot, nr.plot,
                 ncol = 2, nrow = 4, labels = "AUTO"), 
       dpi = 300, width = 7, height = 10
)



