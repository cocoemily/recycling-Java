library(tidyverse)
library(ggpubr)
library(ggthemes)
library(rcompanion)
library(pscl)
library(Dict)
library(boot)
library(snow)

theme_set(theme_bw())

layers.overlap = read_csv("../results/cell-counts-hotspot-overlap.csv")

layers.overlap = layers.overlap[,-1]
parameters = colnames(layers.overlap[c(2:14)])

summary(layers.overlap$RI.numscvg.overlap)
summary(layers.overlap$high_scvg)
summary(layers.overlap$high_RI)
summary(layers.overlap$RI.numenct.overlap)
summary(layers.overlap$high_enct)


####frequency of overlaps with high recycling intensity####
long.overlap = layers.overlap %>%
  gather(key = "vars", value = "count.overlap", 
         RI.flkcnt.overlap, RI.nodcnt.overlap, RI.numdisc.overlap, RI.numscvg.overlap, RI.numenct.overlap, RI.numret.overlap)

var.labs = c("RI and flake count", "RI and nodule count", "RI and discards", 
             "RI and scavenging events", "RI and square encounters", "RI and retouches")
names(var.labs) = unique(long.overlap$vars)
tech.labs = c("two technologies", "many technologies")
names(tech.labs) = c("1", "2")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)

ccplot = ggplot(long.overlap %>% filter(overlap == 1)) +
  geom_bar(aes(x = count.overlap, fill = as.factor(mu), group = as.factor(mu)), position = "dodge2") +
  facet_grid(num_agents~vars, labeller = labeller(vars = var.labs, num_agents = occup.labs)) + 
  labs(x = "number of overlapping hotspots", y = "", 
       fill = "mu") +
  theme(strip.text = element_text(size = 6), legend.position = "bottom") +
  scale_fill_colorblind()
plot(ccplot)

ggsave(filename = "../figures/overlapping-grid-squares-dist_V2.tiff", ccplot, 
       dpi = 300, width = 11, height = 5)

table(long.overlap$vars, long.overlap$count.overlap, long.overlap$overlap)


long.high = layers.overlap %>%
  gather(key = "vars", value = "count.high", 
         high_RI, high_flkcnt, high_nodcnt, high_disc, high_scvg, high_enct, high_ret)
long.high$vars = factor(long.high$vars, levels = c("high_RI", "high_flkcnt", "high_nodcnt", "high_disc", "high_scvg", "high_enct", "high_ret"))

var.labs = c("recycling intensity", "flake count", "nodule count", 
             "discards", "scavenging events", "square encounters","retouches")
names(var.labs) = unique(long.high$vars)
tech.labs = c("two technologies", "many technologies")
names(tech.labs) = c("1", "2")

ccplot2 = ggplot(long.high %>% filter(overlap == 1)) +
  geom_density(aes(x = count.high, fill = as.factor(mu), group = as.factor(mu)), alpha = 0.4) +
  geom_density(aes(x = count.high, color = as.factor(mu), group = as.factor(mu))) +
  facet_grid(num_agents~vars, labeller = labeller(vars = var.labs, num_agents = occup.labs)) + 
  labs(x = "number of hotspots", y = "", color = "mu", fill = "mu") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  theme(strip.text = element_text(size = 6), legend.position = "bottom")
plot(ccplot2)

ggsave(filename = "../figures/supplementary-figures/hotspot-grid-squares-dist.tiff", 
       ccplot2, 
       dpi = 300, width = 10, height = 4.5)


summ.high = long.high %>% group_by(vars, mu) %>%
  summarize(mean = mean(count.high))

####zero inflated regressions####
layers.overlap[,c(2:14)] = lapply(layers.overlap[,c(2:14)], factor)
layers.overlap$blank_prob = factor(layers.overlap$blank_prob, levels = c(0.5, 0.25, 0.75))
layers.overlap$scavenge_prob = factor(layers.overlap$scavenge_prob, levels = c(0.5, 0.25, 0.75))

r.params = parameters[c(1:3, 5:13)]

flk2 = zeroinfl(RI.flkcnt.overlap ~ ., data = layers.overlap[c(r.params, "RI.flkcnt.overlap")])
summary(flk2)
est = coef(flk2, "zero")
se = sqrt(diag(vcov(flk2, "zero")))
zeroflk.df = as.data.frame(cbind(est, se))
zeroflk.df$name = "RI and flake count"
zeroflk.df$var = rownames(zeroflk.df)
rownames(zeroflk.df) = NULL

est = coef(flk2, "count")
se = sqrt(diag(vcov(flk2, "count")))
cflk.df = as.data.frame(cbind(est, se))
cflk.df$name = "RI and flake count"
cflk.df$var = rownames(cflk.df)
rownames(cflk.df) = NULL



nod2 = zeroinfl(RI.nodcnt.overlap ~ ., data = layers.overlap[c(r.params, "RI.nodcnt.overlap")])
summary(nod2)
est = coef(nod2, "zero")
se = sqrt(diag(vcov(nod2, "zero")))
zeronod.df = as.data.frame(cbind(est, se))
zeronod.df$name = "RI and nodule count"
zeronod.df$var = rownames(zeronod.df)
rownames(zeronod.df) = NULL

est = coef(nod2, "count")
se = sqrt(diag(vcov(nod2, "count")))
cnod.df = as.data.frame(cbind(est, se))
cnod.df$name = "RI and nodule count"
cnod.df$var = rownames(cnod.df)
rownames(cnod.df) = NULL

disc2 = zeroinfl(RI.numdisc.overlap ~ ., data = layers.overlap[c(r.params, "RI.numdisc.overlap")])
summary(disc2)
est = coef(disc2, "zero")
se = sqrt(diag(vcov(disc2, "zero")))
zerodisc.df = as.data.frame(cbind(est, se))
zerodisc.df$name = "RI and discards"
zerodisc.df$var = rownames(zerodisc.df)
rownames(zerodisc.df) = NULL

est = coef(disc2, "count")
se = sqrt(diag(vcov(disc2, "count")))
cdisc.df = as.data.frame(cbind(est, se))
cdisc.df$name = "RI and discards"
cdisc.df$var = rownames(cdisc.df)
rownames(cdisc.df) = NULL

scvg2 = zeroinfl(RI.numscvg.overlap ~ ., data = layers.overlap[c(r.params, "RI.numscvg.overlap")])
summary(scvg2)
est = coef(scvg2, "zero")
se = sqrt(diag(vcov(scvg2, "zero")))
zeroscvg.df = as.data.frame(cbind(est, se))
zeroscvg.df$name = "RI and scavenging events"
zeroscvg.df$var = rownames(zeroscvg.df)
rownames(zeroscvg.df) = NULL

est = coef(scvg2, "count")
se = sqrt(diag(vcov(scvg2, "count")))
cscvg.df = as.data.frame(cbind(est, se))
cscvg.df$name = "RI and scavenging events"
cscvg.df$var = rownames(cscvg.df)
rownames(cscvg.df) = NULL

enct2 = zeroinfl(RI.numenct.overlap ~ ., data = layers.overlap[c(r.params, "RI.numenct.overlap")])
summary(enct2)
est = coef(enct2, "zero")
se = sqrt(diag(vcov(enct2, "zero")))
zeroenct.df = as.data.frame(cbind(est, se))
zeroenct.df$name = "RI and square encounters"
zeroenct.df$var = rownames(zeroenct.df)
rownames(zeroenct.df) = NULL

est = coef(enct2, "count")
se = sqrt(diag(vcov(enct2, "count")))
cenct.df = as.data.frame(cbind(est, se))
cenct.df$name = "RI and square encounters"
cenct.df$var = rownames(cenct.df)
rownames(cenct.df) = NULL

ret2 = zeroinfl(RI.numret.overlap ~ ., data = layers.overlap[c(r.params, "RI.numret.overlap")])
summary(ret2)
est = coef(ret2, "zero")
se = sqrt(diag(vcov(ret2, "zero")))
zeroret.df = as.data.frame(cbind(est, se))
zeroret.df$name = "RI and retouches"
zeroret.df$var = rownames(zeroret.df)
rownames(zeroret.df) = NULL

est = coef(ret2, "count")
se = sqrt(diag(vcov(ret2, "count")))
cret.df = as.data.frame(cbind(est, se))
cret.df$name = "RI and retouches"
cret.df$var = rownames(cret.df)
rownames(cret.df) = NULL


######visualizations#####
allzero = rbind(zeroflk.df, zeronod.df, zerodisc.df, zeroscvg.df, zeroenct.df, zeroret.df)
allzero = allzero  %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper), 
         positive = ifelse(est > 0, "positive", "negative"))


facet.labs =c(
  "intercept", 
  "manufacture events: 30 vs. manufacture events: 15", 
  "carry capacity: 20 vs. carry capacity: 10", 
  "max. flake size: 2 vs. max. flake size: 1", 
  "blank probability: 0.25 vs. blank probability: 0.5", 
  "blank probability: 0.75 vs. blank probability: 0.5", 
  "scavenging probability: 0.25 vs. scavenging probability: 0.5", 
  "scavenging probability: 0.75 vs. scavenging probability: 0.5", 
  "many technologies vs. two technologies", 
  "mu: 2 vs. mu: 1", 
  "mu: 3 vs. mu: 1", 
  "number of agents: 200 vs number of agents: 100", 
  "size preference vs. no size preference", 
  "flake preference vs. nodule preference", 
  "min. selectable flake size: 2 vs. min. selectable flake size: 1",
  "strict selection vs. non-strict selection"
)
names(facet.labs) = unique(allzero$var)

allzero$var = factor(allzero$var, 
                     levels = c(
                       "(Intercept)",
                       "overlap2",
                       "num_agents200",
                       "mu2",
                       "mu3",
                       "max_use_intensity30",     
                       "max_artifact_carry20",
                       "max_flake_size2",         
                       "blank_prob0.25",
                       "blank_prob0.75",          
                       "scavenge_prob0.25",
                       "scavenge_prob0.75",
                       "flake_preferenceTRUE",
                       "size_preferenceTRUE",
                       "min_suitable_flake_size2",
                       "strict_selectionTRUE" 
                     ))

allzero$name = factor(allzero$name, levels = c("RI and flake count", 
                                               "RI and nodule count", 
                                               "RI and discards", 
                                               "RI and scavenging events", 
                                               "RI and square encounters", 
                                               "RI and retouches"))

zeroplot = ggplot(allzero %>% filter(signif == T)) +
  #geom_point(aes(x = var, y = est, fill = name, color = name, group = name, shape = positive)) +
  geom_point(aes(x = name, y = est, fill = name, color = name, group = name, shape = positive)) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(25, 24)) +
  facet_wrap(~var, labeller = labeller(var = facet.labs)) +
  # facet_wrap(~name) +
  # scale_x_discrete(labels = facet.labs) +
  labs(y = "log(odds)") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8),
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) 
plot(zeroplot)
ggsave(filename = "../figures/log-odds_no-overlap.tiff", zeroplot,
       dpi = 300, width = 12.5, height = 8)

allcount = rbind(cflk.df, cnod.df, cdisc.df, cscvg.df, cenct.df, cret.df)
allcount = allcount %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper)) %>%
  mutate(irr = exp(est)) %>%
  mutate(greater = ifelse(irr > 1, "greater", 
                          ifelse(irr < 1, "lower", "equal")))

allcount$var = factor(allcount$var, 
                      levels = c(
                        "(Intercept)",
                        "overlap2",
                        "num_agents200",
                        "mu2",
                        "mu3",
                        "max_use_intensity30",     
                        "max_artifact_carry20",
                        "max_flake_size2",         
                        "blank_prob0.25",
                        "blank_prob0.75",          
                        "scavenge_prob0.25",
                        "scavenge_prob0.75",
                        "flake_preferenceTRUE",
                        "size_preferenceTRUE",
                        "min_suitable_flake_size2",
                        "strict_selectionTRUE" 
                      ))

allcount$name = factor(allcount$name, levels = c("RI and flake count", 
                                                 "RI and nodule count", 
                                                 "RI and discards", 
                                                 "RI and scavenging events", 
                                                 "RI and square encounters", 
                                                 "RI and retouches"))

countplot = ggplot(allcount %>% filter(signif == T)) +
  geom_point(aes(x = name, y = irr, fill = name, color = name, group = name, shape = greater)) +
  #geom_errorbar(aes(x = name, y = irr, ymax = upper, ymin = lower, group = name), width = 0.2, linewidth = 0.25) +
  geom_hline(aes(yintercept = 1), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(24, 25)) +
  facet_wrap(~var, labeller = labeller(var = facet.labs)) +
  labs(x = "parameter", y = "incidence rate ratio") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8), 
        legend.position = "bottom", 
        legend.title = element_blank()) 
plot(countplot)
ggsave(filename = "../figures/IRR_overlap-counts.tiff", countplot,
       dpi = 300, width = 12.5, height = 8)


####high counts regressions####
r.params = parameters[c(1:3, 5:13)]
high.params = colnames(layers.overlap[,c(16,20,22,24,26,28,30)])

layers.overlap2 = layers.overlap
layers.overlap2[r.params] = lapply(layers.overlap2[r.params], factor)
layers.overlap2$blank_prob = factor(layers.overlap2$blank_prob, c(0.5, 0.25, 0.75))
layers.overlap2$scavenge_prob = factor(layers.overlap2$scavenge_prob, c(0.5, 0.25, 0.75))

hflk = zeroinfl(RI.flkcnt.overlap ~ ., data = layers.overlap2[c(r.params, high.params, "RI.flkcnt.overlap")])
summary(hflk)
est = coef(hflk, "zero")
se = sqrt(diag(vcov(hflk, "zero")))
zhflk.df = as.data.frame(cbind(est, se))
zhflk.df$name = "RI and flake count"
zhflk.df$var = rownames(zhflk.df)
rownames(zhflk.df) = NULL
est = coef(hflk, "count")
se = sqrt(diag(vcov(hflk, "count")))
chflk.df = as.data.frame(cbind(est, se))
chflk.df$name = "RI and flake count"
chflk.df$var = rownames(chflk.df)
rownames(chflk.df) = NULL

hnod = zeroinfl(RI.nodcnt.overlap ~ ., data = layers.overlap2[c(r.params, high.params, "RI.nodcnt.overlap")])
summary(hnod)
est = coef(hnod, "zero")
se = sqrt(diag(vcov(hnod, "zero")))
zhnod.df = as.data.frame(cbind(est, se))
zhnod.df$name = "RI and nodule count"
zhnod.df$var = rownames(zhnod.df)
rownames(zhnod.df) = NULL
est = coef(hnod, "count")
se = sqrt(diag(vcov(hnod, "count")))
chnod.df = as.data.frame(cbind(est, se))
chnod.df$name = "RI and nodule count"
chnod.df$var = rownames(chnod.df)
rownames(chnod.df) = NULL

hdisc = zeroinfl(RI.numdisc.overlap ~ ., data = layers.overlap2[c(r.params, high.params, "RI.numdisc.overlap")])
summary(hdisc)
est = coef(hdisc, "zero")
se = sqrt(diag(vcov(hdisc, "zero")))
zhdisc.df = as.data.frame(cbind(est, se))
zhdisc.df$name = "RI and discards"
zhdisc.df$var = rownames(zhdisc.df)
rownames(zhdisc.df) = NULL
est = coef(hdisc, "count")
se = sqrt(diag(vcov(hdisc, "count")))
chdisc.df = as.data.frame(cbind(est, se))
chdisc.df$name = "RI and discards"
chdisc.df$var = rownames(chdisc.df)
rownames(chdisc.df) = NULL

hscvg = zeroinfl(RI.numscvg.overlap ~ ., data = layers.overlap2[c(r.params, high.params, "RI.numscvg.overlap")])
summary(hscvg)
est = coef(hscvg, "zero")
se = sqrt(diag(vcov(hscvg, "zero")))
zhscvg.df = as.data.frame(cbind(est, se))
zhscvg.df$name = "RI and scavenging events"
zhscvg.df$var = rownames(zhscvg.df)
rownames(zhscvg.df) = NULL
est = coef(hscvg, "count")
se = sqrt(diag(vcov(hscvg, "count")))
chscvg.df = as.data.frame(cbind(est, se))
chscvg.df$name = "RI and scavenging events"
chscvg.df$var = rownames(chscvg.df)
rownames(chscvg.df) = NULL

henct = zeroinfl(RI.numenct.overlap ~ ., data = layers.overlap2[c(r.params, high.params, "RI.numenct.overlap")])
summary(henct)
est = coef(henct, "zero")
se = sqrt(diag(vcov(henct, "zero")))
zhenct.df = as.data.frame(cbind(est, se))
zhenct.df$name = "RI and square encounters"
zhenct.df$var = rownames(zhenct.df)
rownames(zhenct.df) = NULL
est = coef(henct, "count")
se = sqrt(diag(vcov(henct, "count")))
chenct.df = as.data.frame(cbind(est, se))
chenct.df$name = "RI and square encounters"
chenct.df$var = rownames(chenct.df)
rownames(chenct.df) = NULL

hret = zeroinfl(RI.numret.overlap ~ ., data = layers.overlap2[c(r.params, high.params, "RI.numret.overlap")])
summary(hret)
est = coef(hret, "zero")
se = sqrt(diag(vcov(hret, "zero")))
zhret.df = as.data.frame(cbind(est, se))
zhret.df$name = "RI and retouches"
zhret.df$var = rownames(zhret.df)
rownames(zhret.df) = NULL
est = coef(hret, "count")
se = sqrt(diag(vcov(hret, "count")))
chret.df = as.data.frame(cbind(est, se))
chret.df$name = "RI and retouches"
chret.df$var = rownames(chret.df)
rownames(chret.df) = NULL


##### visualization####
allzero = rbind(zhflk.df, zhnod.df, zhdisc.df, zhscvg.df, zhenct.df, zhret.df)
allzero = allzero  %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper), 
         positive = ifelse(est > 0, "positive", "negative"))
allzero$name = factor(allzero$name, levels = c("RI and flake count", 
                                               "RI and nodule count", 
                                               "RI and discards", 
                                               "RI and scavenging events", 
                                               "RI and square encounters", 
                                               "RI and retouches"))

highzero = allzero %>% filter(str_detect(var, "high"))

unique(highzero$var)

high.labs = c(
  "count of recycling intensity hotspots", 
  "count of flake count hotspots", 
  "count of nodule count hotspots",
  "count of discard event hotspots", 
  "count of scavenging event hotspots", 
  "count of grid square encounter hotspots", 
  "count of retouch event hotspots"
)
names(high.labs) = unique(highzero$var)

highzero$var = factor(highzero$var, 
                      levels = c(
                        "high_RI", 
                        "high_flkcnt", "high_nodcnt", 
                        "high_disc", "high_scvg", "high_ret", "high_enct"
                      ))

zeroplot = ggplot(highzero %>% filter(signif == T)) +
  #geom_errorbar(aes(x = name, y = est, ymax = upper, ymin = lower, group = name), width = 0.2, linewidth = 0.25) +
  geom_point(aes(x = name, y = est, color = name, fill = name, group = name, shape = positive)) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(25, 24)) +
  facet_wrap(~var, labeller = labeller(var = high.labs)) +
  labs(x = "parameter", y = "log(odds)") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 8), axis.text = element_text(size = 8), 
        axis.title = element_text(size = 7), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.y = element_blank()) 
plot(zeroplot)

ggsave(filename = "../figures/supplementary-figures/odds-zero-overlap_high-counts.tiff", 
       zeroplot, 
       dpi = 300, height = 8, width = 10)


allcount = rbind(chflk.df, chnod.df, chdisc.df, chscvg.df, chenct.df, chret.df)
allcount = allcount  %>%
  mutate(est2 = exp(est), 
         se2 = exp(se)) %>%
  mutate(lower = est2 - se2, 
         upper = est2 + se2) %>%
  rowwise() %>%
  mutate(signif = !between(1, lower, upper))

countplot = ggplot(highcount %>% filter(signif == T)) +
  #geom_point(aes(x = var, y = est, color = name, group = name), position = position_dodge(width = 0.75)) +
  #geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = name, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
  geom_col(aes(x = var_clean, y = est, fill = name, group = name)) +
  geom_errorbar(aes(x = var_clean, y = est, ymax = upper, ymin = lower, group = name), width = 0.2, linewidth = 0.25) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~name, nrow = 1) +
  labs(x = "parameter", y = "estimate") +
  theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6), 
        axis.title = element_text(size = 7), 
        legend.position = "none") 



#####high counts by parameters####
hist(layers.overlap$high_RI)
hRI = zeroinfl(high_RI ~ ., data = layers.overlap2[c(r.params, "high_RI")])
summary(hRI) 

est = coef(hRI, "zero")
se = sqrt(diag(vcov(hRI, "zero")))
zhRI.df = as.data.frame(cbind(est, se))
zhRI.df$name = "RI hotspots"
zhRI.df$var = rownames(zhRI.df)

zhRI.df = zhRI.df  %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper), 
         positive = ifelse(est > 0, "positive", "negative"))

facet.labs =c(
  "intercept", 
  "manufacture events: 30 vs. manufacture events: 15", 
  "carry capacity: 20 vs. carry capacity: 10", 
  "max. flake size: 2 vs. max. flake size: 1", 
  "blank probability: 0.25 vs. blank probability: 0.5", 
  "blank probability: 0.75 vs. blank probability: 0.5", 
  "scavenging probability: 0.25 vs. scavenging probability: 0.5", 
  "scavenging probability: 0.75 vs. scavenging probability: 0.5", 
  "many technologies vs. two technologies", 
  "mu: 2 vs. mu: 1", 
  "mu: 3 vs. mu: 1", 
  "number of agents: 200 vs number of agents: 100", 
  "size preference vs. no size preference", 
  "flake preference vs. nodule preference", 
  "min. selectable flake size: 2 vs. min. selectable flake size: 1",
  "strict selection vs. non-strict selection"
)
names(facet.labs) = unique(zhRI.df$var)

zhRI.df$var = factor(zhRI.df$var, 
                         levels = c(
                           "(Intercept)",
                           "overlap2",
                           "num_agents200",
                           "mu2",
                           "mu3",
                           "max_use_intensity30",     
                           "max_artifact_carry20",
                           "max_flake_size2",         
                           "blank_prob0.25",
                           "blank_prob0.75",          
                           "scavenge_prob0.25",
                           "scavenge_prob0.75",
                           "flake_preferenceTRUE",
                           "size_preferenceTRUE",
                           "min_suitable_flake_size2",
                           "strict_selectionTRUE" 
                         ))

zeroplot = ggplot(zhRI.df %>% filter(signif == T)) +
  geom_point(aes(x = var, y = est, fill = name, color = name, group = name, shape = positive)) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(25, 24)) +
  # facet_wrap(~name) +
  scale_x_discrete(labels = facet.labs) +
  labs(y = "log(odds)") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8),
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) 
plot(zeroplot)


ggsave(filename = "../figures/supplementary-figures/RI-hotspot_regression-output.tiff", 
       zeroplot, 
       dpi = 300, width = 6, height = 3)

######high counts of other hotspots######
hist(layers.overlap$high_enct)
henct = glm(high_enct ~ ., data = layers.overlap[c(r.params, "high_enct")], family = "poisson")
summary(henct) 

hist(layers.overlap$high_ret)
hret = glm(high_ret ~ ., data = layers.overlap[c(r.params, "high_ret")], family = "poisson")
summary(hret) 

hist(layers.overlap$high_scvg)
hscvg = glm(high_scvg ~ ., data = layers.overlap[c(r.params, "high_scvg")], family = "poisson")
summary(hscvg) 

hist(layers.overlap$high_disc)
hdisc = glm(high_disc ~ ., data = layers.overlap[c(r.params, "high_disc")], family = "poisson")
summary(hdisc) 


####encounters and retouched artifacts####
ret.overlap = read_csv("../results/retouch-encounter-hotspot-overlap.csv")
ret.overlap = ret.overlap[,-1]
ret.overlap[,c(1:13)] = lapply(ret.overlap[,c(1:13)], factor)
ret.overlap$blank_prob = factor(ret.overlap$blank_prob, levels = c(0.5, 0.25, 0.75))
ret.overlap$scavenge_prob = factor(ret.overlap$scavenge_prob, levels = c(0.5, 0.25, 0.75))
parameters = colnames(ret.overlap[,c(1:13)])
r.params = parameters[c(1:3, 5:13)]

summary(ret.overlap$ret.enct.overlap)

ccplot = ggplot(ret.overlap %>% filter(overlap == 1)) +
  geom_bar(aes(x = ret.enct.overlap, fill = as.factor(mu), group = as.factor(mu)), position = "dodge2") +
  facet_wrap(~num_agents, labeller = labeller(num_agents = occup.labs), 
             strip.position = "right") + 
  labs(x = "number of overlapping hotspots", y = "", 
       fill = "mu") +
  theme(strip.text = element_text(size = 6), legend.position = "bottom") +
  scale_fill_colorblind() + 
  ylim(0, 50000)
plot(ccplot)

ccplot2 = ggplot(ret.overlap %>% filter(overlap == 1) %>% filter(num_agents == 100)) +
  geom_bar(aes(x = ret.enct.overlap, fill = as.factor(mu), group = as.factor(mu)), position = "dodge2") +
  facet_grid(blank_prob ~ scavenge_prob, labeller = label_both) + 
  labs(x = "number of overlapping hotspots", y = "", 
       fill = "mu") +
  theme(strip.text = element_text(size = 6), legend.position = "bottom") +
  scale_fill_colorblind()
plot(ccplot2)

high.df = ret.overlap %>% 
  pivot_longer(c(high_ret, high_enct), names_to = "hotspots")

h.labs = c("retouched artifact hotspots", "grid square encounters hotspots")
names(h.labs) = c("high_ret", "high_enct")

highplot = ggplot(high.df) +
  geom_bar(aes(x = value, fill = as.factor(mu), group = as.factor(mu)), position = "dodge2") +
  facet_grid(num_agents ~ hotspots, 
             labeller = labeller(
               num_agents = occup.labs, 
               hotspots = h.labs
             )) + 
  labs(x = "number of hotspots", y = "", 
       fill = "mu") +
  theme(strip.text = element_text(size = 6), legend.position = "bottom") +
  scale_fill_colorblind()
plot(highplot)

ggsave(filename = "../figures/retouched-encounter_overlaps.tiff", 
       ggarrange(
         ccplot, highplot, 
         ncol = 1, labels = "AUTO", common.legend = T, legend = "bottom"
       ), 
       dpi = 300, height = 6, width = 8
)


retenct = zeroinfl(ret.enct.overlap ~ ., data = ret.overlap[c(r.params, "ret.enct.overlap")])
summary(retenct)

est = coef(retenct, "zero")
se = sqrt(diag(vcov(retenct, "zero")))
zretenct.df = as.data.frame(cbind(est, se))
zretenct.df$name = "retouch artifact proportion and grid square encounters"
zretenct.df$var = rownames(zretenct.df)
rownames(zretenct.df) = NULL

est = coef(retenct, "count")
se = sqrt(diag(vcov(retenct, "count")))
cretenct.df = as.data.frame(cbind(est, se))
cretenct.df$name = "retouch artifact proportion and grid square encounters"
cretenct.df$var = rownames(cretenct.df)
rownames(cretenct.df) = NULL

zretenct.df = zretenct.df %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper), 
         positive = ifelse(est > 0, "positive", "negative"))

facet.labs =c(
  "intercept", 
  "manufacture events: 30 vs. manufacture events: 15", 
  "carry capacity: 20 vs. carry capacity: 10", 
  "max. flake size: 2 vs. max. flake size: 1", 
  "blank probability: 0.25 vs. blank probability: 0.5", 
  "blank probability: 0.75 vs. blank probability: 0.5", 
  "scavenging probability: 0.25 vs. scavenging probability: 0.5", 
  "scavenging probability: 0.75 vs. scavenging probability: 0.5", 
  "many technologies vs. two technologies", 
  "mu: 2 vs. mu: 1", 
  "mu: 3 vs. mu: 1", 
  "number of agents: 200 vs number of agents: 100", 
  "size preference vs. no size preference", 
  "flake preference vs. nodule preference", 
  "min. selectable flake size: 2 vs. min. selectable flake size: 1",
  "strict selection vs. non-strict selection"
)
names(facet.labs) = unique(zretenct.df$var)

zretenct.df$var = factor(zretenct.df$var, 
                         levels = c(
                           "(Intercept)",
                           "overlap2",
                           "num_agents200",
                           "mu2",
                           "mu3",
                           "max_use_intensity30",     
                           "max_artifact_carry20",
                           "max_flake_size2",         
                           "blank_prob0.25",
                           "blank_prob0.75",          
                           "scavenge_prob0.25",
                           "scavenge_prob0.75",
                           "flake_preferenceTRUE",
                           "size_preferenceTRUE",
                           "min_suitable_flake_size2",
                           "strict_selectionTRUE" 
                         ))

zeroplot = ggplot(zretenct.df %>% filter(signif == T)) +
  geom_point(aes(x = var, y = est, fill = name, color = name, group = name, shape = positive)) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(25, 24)) +
  # facet_wrap(~name) +
  scale_x_discrete(labels = facet.labs) +
  labs(y = "log(odds)") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8),
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) 
plot(zeroplot)


cretenct.df = cretenct.df %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper)) %>%
  mutate(irr = exp(est)) %>%
  mutate(greater = ifelse(irr > 1, "greater", 
                          ifelse(irr < 1, "lower", "equal")))

cretenct.df$var = factor(cretenct.df$var, 
                         levels = c(
                           "(Intercept)",
                           "overlap2",
                           "num_agents200",
                           "mu2",
                           "mu3",
                           "max_use_intensity30",     
                           "max_artifact_carry20",
                           "max_flake_size2",         
                           "blank_prob0.25",
                           "blank_prob0.75",          
                           "scavenge_prob0.25",
                           "scavenge_prob0.75",
                           "flake_preferenceTRUE",
                           "size_preferenceTRUE",
                           "min_suitable_flake_size2",
                           "strict_selectionTRUE" 
                         ))

countplot = ggplot(cretenct.df %>% filter(signif == T)) +
  geom_point(aes(x = var, y = irr, fill = name, color = name, group = name, shape = greater)) +
  #geom_errorbar(aes(x = name, y = irr, ymax = upper, ymin = lower, group = name), width = 0.2, linewidth = 0.25) +
  geom_hline(aes(yintercept = 1), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(24, 25)) +
  scale_x_discrete(labels = facet.labs) +
  labs(y = "incidence rate ratio") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8), 
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) 
plot(countplot)



#### recycling intensity and retouched artifact proportions ####
summary(layers.overlap$RI.retprop.overlap)

tech.labs = c("two technologies", "many technologies")
names(tech.labs) = c("1", "2")
occup.labs = c("100 agents", "200 agents")
names(occup.labs) = c(100, 200)

ccplot = ggplot(layers.overlap %>% filter(overlap == 1)) +
  geom_bar(aes(x = RI.retprop.overlap, fill = as.factor(mu), group = as.factor(mu)), position = "dodge2") +
  facet_wrap(~num_agents, labeller = labeller(num_agents = occup.labs)) + 
  labs(x = "number of overlapping grid squares", y = "", 
       fill = "mu") +
  theme(strip.text = element_text(size = 6), legend.position = "left") +
  scale_fill_colorblind() +
  ylim(0, 40000)
plot(ccplot)


retprop = zeroinfl(RI.retprop.overlap ~ ., data = layers.overlap[c(r.params, "RI.retprop.overlap")])
summary(retprop)
est = coef(retprop, "zero")
se = sqrt(diag(vcov(retprop, "zero")))
zrp.df = as.data.frame(cbind(est, se))
zrp.df$name = "RI and retouched artifact proportion"
zrp.df$var = rownames(zrp.df)
rownames(zrp.df) = NULL
est = coef(retprop, "count")
se = sqrt(diag(vcov(retprop, "count")))
crp.df = as.data.frame(cbind(est, se))
crp.df$name = "RI and retouched artifact proportion"
crp.df$var = rownames(crp.df)
rownames(crp.df) = NULL

zrp.df = zrp.df %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper), 
         positive = ifelse(est > 0, "positive", "negative"))

facet.labs =c(
  "intercept", 
  "manufacture events: 30 vs. manufacture events: 15", 
  "carry capacity: 20 vs. carry capacity: 10", 
  "max. flake size: 2 vs. max. flake size: 1", 
  "blank probability: 0.25 vs. blank probability: 0.5", 
  "blank probability: 0.75 vs. blank probability: 0.5", 
  "scavenging probability: 0.25 vs. scavenging probability: 0.5", 
  "scavenging probability: 0.75 vs. scavenging probability: 0.5", 
  "many technologies vs. two technologies", 
  "mu: 2 vs. mu: 1", 
  "mu: 3 vs. mu: 1", 
  "number of agents: 200 vs number of agents: 100", 
  "size preference vs. no size preference", 
  "flake preference vs. nodule preference", 
  "min. selectable flake size: 2 vs. min. selectable flake size: 1",
  "strict selection vs. non-strict selection"
)
names(facet.labs) = unique(zrp.df$var)

zrp.df$var = factor(zrp.df$var, 
                    levels = c(
                      "(Intercept)",
                      "overlap2",
                      "num_agents200",
                      "mu2",
                      "mu3",
                      "max_use_intensity30",     
                      "max_artifact_carry20",
                      "max_flake_size2",         
                      "blank_prob0.25",
                      "blank_prob0.75",          
                      "scavenge_prob0.25",
                      "scavenge_prob0.75",
                      "flake_preferenceTRUE",
                      "size_preferenceTRUE",
                      "min_suitable_flake_size2",
                      "strict_selectionTRUE" 
                    ))

zeroplot = ggplot(zrp.df %>% filter(signif == T)) +
  geom_point(aes(x = var, y = est, fill = name, color = name, group = name, shape = positive)) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(25, 24)) +
  # facet_wrap(~name) +
  scale_x_discrete(labels = facet.labs) +
  labs(y = "log(odds)") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8),
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) 
plot(zeroplot)


crp.df = crp.df %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper)) %>%
  mutate(irr = exp(est)) %>%
  mutate(greater = ifelse(irr > 1, "greater", 
                          ifelse(irr < 1, "lower", "equal")))

crp.df$var = factor(crp.df$var, 
                    levels = c(
                      "(Intercept)",
                      "overlap2",
                      "num_agents200",
                      "mu2",
                      "mu3",
                      "max_use_intensity30",     
                      "max_artifact_carry20",
                      "max_flake_size2",         
                      "blank_prob0.25",
                      "blank_prob0.75",          
                      "scavenge_prob0.25",
                      "scavenge_prob0.75",
                      "flake_preferenceTRUE",
                      "size_preferenceTRUE",
                      "min_suitable_flake_size2",
                      "strict_selectionTRUE" 
                    ))

countplot = ggplot(crp.df %>% filter(signif == T)) +
  geom_point(aes(x = var, y = irr, fill = name, color = name, group = name, shape = greater)) +
  #geom_errorbar(aes(x = name, y = irr, ymax = upper, ymin = lower, group = name), width = 0.2, linewidth = 0.25) +
  geom_hline(aes(yintercept = 1), color = "red", linewidth = 0.25) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(24, 25)) +
  scale_x_discrete(labels = facet.labs) +
  labs(y = "incidence rate ratio") +
  guides(color = "none", fill = "none") +
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8), 
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) 
plot(countplot)

ggsave(filename = "../figures/ri_retouched-prop_overlap.tiff",
       ggarrange(ccplot,
                 ggarrange(zeroplot, countplot, labels = c("B", "C")), labels = "A", 
                 ncol = 1, heights = c(0.5, 1)
       ), 
       dpi = 300, width = 10, height = 8
)

