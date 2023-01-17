library(tidyverse)
library(rcompanion)
library(cowplot)
library(raster)
library(fitdistrplus)
library(pscl)
library(AICcmodavg)

theme_set(theme_bw())

layers.overlap = read_csv("../results/cell-counts-hotspot-overlap.csv")
parameters = colnames(layers.overlap[c(3:15)])

##frequency of overlaps with high recycling intensity
long.overlap = layers.overlap %>%
  gather(key = "vars", value = "count.overlap", 
         RI.flkcnt.overlap, RI.nodcnt.overlap, RI.numdisc.overlap, RI.numscvg.overlap, RI.numenct.overlap, RI.numret.overlap)

ggplot(long.overlap) +
  geom_bar(aes(x = count.overlap)) +
  facet_grid(overlap~vars)

table(long.overlap$vars, long.overlap$count.overlap)

##how to see which variable has most overlap with high RI values?
mean(layers.overlap$RI.flkcnt.overlap)
mean(layers.overlap$RI.nodcnt.overlap)
mean(layers.overlap$RI.numdisc.overlap)
mean(layers.overlap$RI.numenct.overlap)
mean(layers.overlap$RI.numscvg.overlap)
mean(layers.overlap$RI.numret.overlap)

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

#overlap of recycling and nodule counts is the only set of variables that has differences
# depending on whether there are two technology types or multiple technology types

strict.lo = layers.overlap %>% filter(strict_selection == TRUE) %>%
  gather(key = "vars", value = "count.overlap", 
         RI.flkcnt.overlap, RI.nodcnt.overlap, RI.numdisc.overlap, RI.numscvg.overlap, RI.numenct.overlap, RI.numret.overlap)

ggplot(strict.lo) +
  geom_bar(aes(x = count.overlap)) +
  facet_grid(size_preference*flake_preference ~ vars, labeller = label_both)

strict.lo = layers.overlap %>% filter(strict_selection == TRUE)

flk2 = zeroinfl(RI.flkcnt.overlap ~ ., data = strict.lo[c(3:8, 10:14, 17)])
summary(flk2)

nod2 = zeroinfl(RI.nodcnt.overlap ~ ., data = strict.lo[c(3:8, 10:14, 18)])
summary(nod2)

disc2 = zeroinfl(RI.numdisc.overlap ~ ., data = strict.lo[c(3:8, 10:14, 19)])
summary(disc2)

scvg2 = zeroinfl(RI.numscvg.overlap ~ ., data = strict.lo[c(3:8, 10:14, 20)])
summary(scvg2)

enct2 = zeroinfl(RI.numenct.overlap ~ ., data = strict.lo[c(3:8, 10:14, 21)])
summary(enct2)

ret2 = zeroinfl(RI.numret.overlap ~ ., data = strict.lo[c(3:8, 10:14, 22)])
summary(ret2)



lax.lo = layers.overlap %>% filter(strict_selection == FALSE) %>%
  gather(key = "vars", value = "count.overlap", 
         RI.flkcnt.overlap, RI.nodcnt.overlap, RI.numdisc.overlap, RI.numscvg.overlap, RI.numenct.overlap, RI.numret.overlap)

ggplot(lax.lo) +
  geom_bar(aes(x = count.overlap)) +
  facet_grid(size_preference*flake_preference ~ vars, labeller = label_both)


lax.lo = layers.overlap %>% filter(strict_selection == FALSE)
#non-strict selection experiments
flk1 = zeroinfl(RI.flkcnt.overlap ~ ., data = lax.lo[c(3:8, 10:14, 17)])
summary(flk1)

nod1 = zeroinfl(RI.nodcnt.overlap ~ ., data = lax.lo[c(3:8, 10:14, 18)])
summary(nod1)

disc1 = zeroinfl(RI.numdisc.overlap ~ ., data = lax.lo[c(3:8, 10:14, 19)])
summary(disc1)

scvg1 = zeroinfl(RI.numscvg.overlap ~ ., data = lax.lo[c(3:8, 10:14, 20)])
summary(scvg1)

enct1 = zeroinfl(RI.numenct.overlap ~ ., data = lax.lo[c(3:8, 10:14, 21)])
summary(enct1)

ret1 = zeroinfl(RI.numret.overlap ~ ., data = lax.lo[c(3:8, 10:14, 22)])
summary(ret1)
