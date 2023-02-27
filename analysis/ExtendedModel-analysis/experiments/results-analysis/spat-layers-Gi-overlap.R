library(tidyverse)
library(rcompanion)
library(cowplot)
library(raster)
library(fitdistrplus)
library(pscl)
library(AICcmodavg)
library(Dict)
library(ggpubr)
library(broom)

theme_set(theme_bw())

layers.overlap = read_csv("../results/cell-counts-hotspot-overlap.csv")
odata = read_csv("../results/retouch-encounter-overlap.csv")

layers.overlap = layers.overlap[,-1]

#CLEAN COLS
# oldcols = colnames(layers.overlap)
# cols = oldcols[-c(1, 14)]
# colnames(layers.overlap) = c("exp", "run", cols)
# layers.overlap$flake_preference = as.logical(layers.overlap$flake_preference)
# layers.overlap$strict_selection = as.logical(layers.overlap$strict_selection)


parameters = colnames(layers.overlap[c(3:14)])



##frequency of overlaps with high recycling intensity
long.overlap = layers.overlap %>%
  gather(key = "vars", value = "count.overlap", 
         RI.flkcnt.overlap, RI.nodcnt.overlap, RI.numdisc.overlap, RI.numscvg.overlap, RI.numenct.overlap, RI.numret.overlap)

var.labs = c("RI and flake count", "RI and nodule count", "RI and discards", 
             "RI and scavenging events", "RI and square encounters", "RI and retouches")
names(var.labs) = unique(long.overlap$vars)
tech.labs = c("two technologies", "many technologies")
names(tech.labs) = c("1", "2")

ccplot = ggplot(long.overlap) +
  geom_bar(aes(x = count.overlap)) +
  facet_grid(overlap~vars, labeller = labeller(vars = var.labs, overlap = tech.labs)) + 
  labs(x = "number of overlapping grid squares", y = "") +
  theme(strip.text = element_text(size = 6))
plot(ccplot)

ggsave(filename = "../figures/overlapping-grid-squares-dist.tiff", ccplot, 
       dpi = 300, width = 8, height = 4.5)

table(long.overlap$vars, long.overlap$count.overlap, long.overlap$overlap)

##how to see which variable has most overlap with high RI values?
summary(layers.overlap$RI.flkcnt.overlap)
summary(layers.overlap$RI.nodcnt.overlap)
summary(layers.overlap$RI.numdisc.overlap)
summary(layers.overlap$RI.numenct.overlap)
summary(layers.overlap$RI.numscvg.overlap)
summary(layers.overlap$RI.numret.overlap)

var(layers.overlap$RI.flkcnt.overlap)
var(layers.overlap$RI.nodcnt.overlap)
var(layers.overlap$RI.numdisc.overlap)
var(layers.overlap$RI.numenct.overlap)
var(layers.overlap$RI.numscvg.overlap)
var(layers.overlap$RI.numret.overlap)

##at the end of model run, what parameters result in overlap of the output variables
# with high recycling intensity hotspots?
chisq.test(x = (layers.overlap %>% filter(overlap == 1))$RI.flkcnt.overlap,
           y = (layers.overlap %>% filter(overlap == 2))$RI.flkcnt.overlap, 
           simulate.p.value = T)

chisq.test(x = (layers.overlap %>% filter(overlap == 1))$RI.nodcnt.overlap,
           y = (layers.overlap %>% filter(overlap == 2))$RI.nodcnt.overlap, 
           simulate.p.value = T)


chisq.test(x = (layers.overlap %>% filter(overlap == 1))$RI.numdisc.overlap,
           y = (layers.overlap %>% filter(overlap == 2))$RI.numdisc.overlap, 
           simulate.p.value = T)

chisq.test(x = (layers.overlap %>% filter(overlap == 1))$RI.numscvg.overlap,
           y = (layers.overlap %>% filter(overlap == 2))$RI.numscvg.overlap, 
           simulate.p.value = T)

chisq.test(x = (layers.overlap %>% filter(overlap == 1))$RI.numenct.overlap,
           y = (layers.overlap %>% filter(overlap == 2))$RI.numenct.overlap, 
           simulate.p.value = T)

chisq.test(x = (layers.overlap %>% filter(overlap == 1))$RI.numret.overlap,
           y = (layers.overlap %>% filter(overlap == 2))$RI.numret.overlap, 
           simulate.p.value = T)

#overlap of recycling and scavenging events is the only set of variables that has differences
# depending on whether there are two technology types or multiple technology types

strict.lo = layers.overlap %>% filter(strict_selection == TRUE) %>%
  gather(key = "vars", value = "count.overlap", 
         RI.flkcnt.overlap, RI.nodcnt.overlap, RI.numdisc.overlap, RI.numscvg.overlap, RI.numenct.overlap, RI.numret.overlap)

ggplot(strict.lo) +
  geom_bar(aes(x = count.overlap)) +
  facet_grid(size_preference*flake_preference ~ vars, labeller = label_both)

lax.lo = layers.overlap %>% filter(strict_selection == FALSE) %>%
  gather(key = "vars", value = "count.overlap", 
         RI.flkcnt.overlap, RI.nodcnt.overlap, RI.numdisc.overlap, RI.numscvg.overlap, RI.numenct.overlap, RI.numret.overlap)

ggplot(lax.lo) +
  geom_bar(aes(x = count.overlap)) +
  facet_grid(size_preference*flake_preference ~ vars, labeller = label_both)


####zero inflated regressions####
flk2 = zeroinfl(RI.flkcnt.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 18)])
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



nod2 = zeroinfl(RI.nodcnt.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 20)])
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

disc2 = zeroinfl(RI.numdisc.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 22)])
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

scvg2 = zeroinfl(RI.numscvg.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 24)])
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

enct2 = zeroinfl(RI.numenct.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 26)])
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

ret2 = zeroinfl(RI.numret.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 28)])
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

allzero = rbind(zeroflk.df, zeronod.df, zerodisc.df, zeroscvg.df, zeroenct.df, zeroret.df)
allzero = allzero  %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper))
allzero$name = factor(allzero$name, var.labs)

allcount = rbind(cflk.df, cnod.df, cdisc.df, cscvg.df, cenct.df, cret.df)
allcount = allcount  %>%
  mutate(lower = est - se, 
         upper = est + se)  %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper))
allcount$name = factor(allcount$name, var.labs)


terms_dict = dict(
  "overlap" = "technology overlap scenario",
  "mu" = "mu",
  "strict_selectionTRUE" = "strict selection: TRUE",
  "size_preferenceTRUE" = "size preference: TRUE",
  "flake_preferenceTRUE" = "flake preference: TRUE",
  "scavenge_prob" = "scavenging probability",
  "min_suitable_flake_size" = "min. selectable flake size",
  "max_use_intensity" = "max. use intensity",
  "max_flake_size" = "max. flake size",
  "max_artifact_carry" = "max. artifact carry",
  "blank_prob" = "blank probability",
  "(Intercept)" = "(intercept)",
  "max_flake_size:min_suitable_flake_size" = "max. flake size:min. selectable flake size",
  .class = "character", 
  .overwrite = FALSE
)
term_levels = c(
  "(intercept)",
  "technology overlap scenario",
  "mu", 
  "blank probability", 
  "scavenging probability", 
  "max. artifact carry", 
  "max. use intensity", 
  "max. flake size", 
  "min. selectable flake size", 
  "flake preference: TRUE", 
  "size preference: TRUE", 
  "strict selection: TRUE", 
  "max. flake size:min. selectable flake size"
)




zeroplot = ggplot(allzero %>% filter(signif == T)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = 2, linewidth = 0.25) +
  #geom_point(aes(x = var, y = est, color = name, group = name), position = position_dodge(width = 0.75)) +
  geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = name, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~name, nrow = 1) +
  labs(x = "parameter", y = "estimate") +
  theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6), 
        axis.title = element_text(size = 7), 
        legend.position = "none") 
plot(zeroplot)
# ggsave(filename = "../figures/zero-est-G-overlap.tiff", zeroplot, 
#        dpi = 300, width = 8.5, height = 4)


countplot = ggplot(allcount %>% filter(signif == T)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = 2, linewidth = 0.25) +
  #geom_point(aes(x = var, y = est, color = name, group = name), position = position_dodge(width = 0.75)) +
  geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = name, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~name, nrow = 1) +
  labs(x = "parameter", y = "estimate") +
  theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6), 
        axis.title = element_text(size = 7), 
        legend.position = "none") 
# ggsave(filename = "../figures/count-est-G-overlap.tiff", countplot, 
#        dpi = 300, width = 8.5, height = 4)

pgrid = ggarrange(zeroplot, countplot, ncol = 1, nrow = 2, labels = "AUTO")
plot(pgrid)
ggsave(filename = "../figures/local-G-overlap-regs.tiff", pgrid,
       dpi = 300, width = 9, height = 6)


####encounters and retouched artifacts####
hist(odata$ret.enct.overlap)
reover = zeroinfl(ret.enct.overlap ~ ., data = odata[c(2:4,6:13,17)])
summary(reover)
summary(odata$ret.enct.overlap)


####high counts####
hflk = zeroinfl(RI.flkcnt.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 15, 18, 19)])
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

hnod = zeroinfl(RI.nodcnt.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 15, 20, 21)])
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

hdisc = zeroinfl(RI.numdisc.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 15, 22, 23)])
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

hscvg = zeroinfl(RI.numscvg.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 15, 24, 25)])
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

henct = zeroinfl(RI.numenct.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 15, 26, 27)])
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

hret = zeroinfl(RI.numret.overlap ~ ., data = layers.overlap[c(2:4, 6:13, 15, 28, 29)])
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


allzero = rbind(zhflk.df, zhnod.df, zhdisc.df, zhscvg.df, zhenct.df, zhret.df)
allzero = allzero  %>%
  mutate(lower = est - se, 
         upper = est + se) %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper))
allzero$name = factor(allzero$name, var.labs)

allcount = rbind(chflk.df, chnod.df, chdisc.df, chscvg.df, chenct.df, chret.df)
allcount = allcount  %>%
  mutate(lower = est - se, 
         upper = est + se)  %>%
  rowwise() %>%
  mutate(signif = !between(0, lower, upper))
allcount$name = factor(allcount$name, var.labs)

hterms_dict = dict(
  "overlap" = "technology overlap scenario",
  "mu" = "mu",
  "strict_selectionTRUE" = "strict selection: TRUE",
  "size_preferenceTRUE" = "size preference: TRUE",
  "flake_preferenceTRUE" = "flake preference: TRUE",
  "scavenge_prob" = "scavenging probability",
  "min_suitable_flake_size" = "min. selectable flake size",
  "max_use_intensity" = "max. use intensity",
  "max_flake_size" = "max. flake size",
  "max_artifact_carry" = "max. artifact carry",
  "blank_prob" = "blank probability",
  "(Intercept)" = "(intercept)",
  "max_flake_size:min_suitable_flake_size" = "max. flake size:min. selectable flake size",
  "high_RI" = "high RI squares", 
  "high_flkcnt" = "high flake count squares", 
  "high_nodcnt" = "high nodule count squares", 
  "high_disc" = "high discard event squares",
  "high_scvg" = "high scavenging event squares",
  "high_enct" = "high encounter event squares", 
  "high_ret" = "high retouch event squares",
  
  .class = "character", 
  .overwrite = FALSE
)
hterm_levels = c(
  "(intercept)",
  "technology overlap scenario",
  "mu", 
  "blank probability", 
  "scavenging probability", 
  "max. artifact carry", 
  "max. use intensity", 
  "max. flake size", 
  "min. selectable flake size", 
  "flake preference: TRUE", 
  "size preference: TRUE", 
  "strict selection: TRUE", 
  "max. flake size:min. selectable flake size",
  "high RI squares",
  "high flake count squares",
  "high nodule count squares",
  "high discard event squares",
  "high scavenging event squares",
  "high encounter event squares",
  "high retouch event squares"
)


for(i in 1:nrow(allzero)) {
  if(!is.na(allzero$var[i])) {
    allzero$var_clean[i] = hterms_dict[allzero$var[i]]
    allcount$var_clean[i] = hterms_dict[allcount$var[i]]
  }
}
allzero$var_clean = factor(allzero$var_clean, levels = hterm_levels)
allcount$var_clean = factor(allcount$var_clean, levels = hterm_levels)

highzero = allzero %>% filter(str_detect(var, "high"))
highcount = allcount %>% filter(str_detect(var, "high"))

zeroplot = ggplot(highzero %>% filter(signif == T)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = 2, linewidth = 0.25) +
  #geom_point(aes(x = var, y = est, color = name, group = name), position = position_dodge(width = 0.75)) +
  geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = name, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~name, nrow = 1) +
  labs(x = "parameter", y = "estimate") +
  theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6), 
        axis.title = element_text(size = 7), 
        legend.position = "none") 
plot(zeroplot)

countplot = ggplot(highcount %>% filter(signif == T)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = 2, linewidth = 0.25) +
  #geom_point(aes(x = var, y = est, color = name, group = name), position = position_dodge(width = 0.75)) +
  geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = name, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~name, nrow = 1) +
  labs(x = "parameter", y = "estimate") +
  theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6), 
        axis.title = element_text(size = 7), 
        legend.position = "none") 

pgrid2 = ggarrange(zeroplot, countplot, ncol = 1, nrow = 2, labels = "AUTO")
plot(pgrid2)
ggsave(filename = "../figures/supplementary-figures/local-G-overlap-regs_high-square.tiff", pgrid2,
       dpi = 300, width = 9, height = 6)


hist(layers.overlap$high_RI)
hRI = glm(high_RI ~ ., data = layers.overlap[c(2:4, 6:13, 15)], family = "poisson")
summary(hRI)                                                  
