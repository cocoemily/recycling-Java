library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(rstatix)

source("ExtendedModel-analysis/experiments/results-analysis/helper-functions.R")

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")
cor.names = colnames(layer.cor[,17:25])
cor.names = cor.names[-length(cor.names)] #removing occupations correlation because encounters = occupations

layer.cor1 = layer.cor[-c(26:27)]
layer.cor1 = layer.cor1 %>% rename("row" = "row...1", 
                                   "col" = "col...2")
layer.cor = layer.cor1

#####end of model run#####
layer.cor.end = layer.cor[which(layer.cor$time == "end"),]
plotNormalHistogram(layer.cor.end$ri.num.scvg.cor)
fit1 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,21)])
summary(fit1)

fit1.2 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit1.2)

#ri.cr.cor
plotNormalHistogram(layer.cor.end$ri.cr.cor)
fit2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,19)])
summary(fit2)

fit2.2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit2.2)

#cortex ratio and object count
plotNormalHistogram(layer.cor.end$cr.obj.cnt.cor)
fit3 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,17)])
summary(fit3)
fit3.2 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit3.2)

plotNormalHistogram(layer.cor.end$ri.obj.cnt.cor)
fit4 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,18)])
summary(fit4)
fit4.2 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit4.2)


plotNormalHistogram(layer.cor.end$ri.num.disc.cor)
fit5 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,20)])
summary(fit5)
fit5.2 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit5.2)

plotNormalHistogram(layer.cor.end$ri.num.enct.cor)
fit6 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,22)])
summary(fit6)
fit6.2 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit6.2)

plotNormalHistogram(layer.cor.end$ri.num.manu.cor)
fit7 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,23)])
summary(fit7)
fit7.2 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit7.2)

plotNormalHistogram(layer.cor.end$ri.num.ret.cor)
fit8 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.end[,c(1,2,4:16,24)])
summary(fit8)
fit8.2 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col), data = layer.cor.end)
summary(fit8.2)

#####middle of model run#####
layer.cor.mid = layer.cor[which(layer.cor$time == "mid"),]
plotNormalHistogram(layer.cor.mid$ri.num.scvg.cor)
fit1 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,21)])
summary(fit1)

fit1.2 = lm(ri.num.scvg.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit1.2)

#ri.cr.cor
plotNormalHistogram(layer.cor.mid$ri.cr.cor)
fit2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,19)])
summary(fit2)

fit2.2 = lm(ri.cr.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit2.2)

#cortex ratio and object count
plotNormalHistogram(layer.cor.mid$cr.obj.cnt.cor)
fit3 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,17)])
summary(fit3)
fit3.2 = lm(cr.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit3.2)

plotNormalHistogram(layer.cor.mid$ri.obj.cnt.cor)
fit4 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,18)])
summary(fit4)
fit4.2 = lm(ri.obj.cnt.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit4.2)


plotNormalHistogram(layer.cor.mid$ri.num.disc.cor)
fit5 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,20)])
summary(fit5)
fit5.2 = lm(ri.num.disc.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit5.2)

plotNormalHistogram(layer.cor.mid$ri.num.enct.cor)
fit6 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,22)])
summary(fit6)
fit6.2 = lm(ri.num.enct.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit6.2)

plotNormalHistogram(layer.cor.mid$ri.num.manu.cor)
fit7 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,23)])
summary(fit7)
fit7.2 = lm(ri.num.manu.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit7.2)

plotNormalHistogram(layer.cor.mid$ri.num.ret.cor)
fit8 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col) + overlap + mu + scavenge_prob +
            blank_prob + max_use_intensity + max_artifact_carry + max_flake_size + 
            max_nodules_size + flake_preference + size_preference + strict_selection +
            min_suitable_flake_size + min_suitable_nodule_size , data = layer.cor.mid[,c(1,2,4:16,24)])
summary(fit8)
fit8.2 = lm(ri.num.ret.cor ~ as.factor(row):as.factor(col), data = layer.cor.mid)
summary(fit8.2)

####ANALYSIS OF CORRELATIONS BETWEEN OUTPUTS BY SCAVENGING PROBABILITY
mid.cor.ri = layer.cor %>% filter(time == "mid") %>%
  gather(key = "comparison", value = "correlation", starts_with("ri"))

end.cor.ri = layer.cor %>% filter(time == "end") %>%
  gather(key = "comparison", value = "correlation", starts_with("ri"))


ggplot(mid.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(~scavenge_prob)

ggplot(end.cor.ri) +
  geom_boxplot(aes(x = comparison, y = scavenge_prob, group = comparison, color = comparison)) +
  facet_wrap(~correlation)


#### ANALYSIS OF END CORRELATION BY PARAMETER ####
##### two technology types #####
two.tech.cor = layer.cor %>% filter(overlap == 1) %>% filter(time == "end") %>% 
  group_by(row, col) %>% mutate(square_num = cur_group_id())


# wtest = pairwise.wilcox.test(two.tech.cor$ri.obj.cnt.cor, two.tech.cor$square_num, p.adjust.method = "BH", 
#                      na.action = na.omit())
# different = wtest[["p.value"]] < 0.05
# table(different)

for(i in 1:length(cor.names)) {
  var = cor.names[i]
  wtest = pairwise.wilcox.test(as.vector(two.tech.cor[var])[[1]], two.tech.cor$square_num, p.adjust.method = "BH", 
                               na.action = na.omit())
  different = wtest[["p.value"]] < 0.05
  print(paste0(var, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
}


for(i in 1:length(cor.names)) {
  var = cor.names[i]
  for(mu in c(1:3)) {
    mu.cor = two.tech.cor[which(two.tech.cor$mu == mu), ]
    wtest = pairwise.wilcox.test(as.vector(mu.cor[var])[[1]], mu.cor$square_num, p.adjust.method = "BH", 
                                 na.action = na.omit())
    different = wtest[["p.value"]] < 0.05
    if(length(table(different)) == 2) {
      print(paste0(var, " when mu = ", mu, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
    } else {
      if(names(table(different)) == "TRUE") {
        print(paste0(var, " when mu = ", mu, ": all squares have significant differences"))
      } else {
        print(paste0(var, " when mu = ", mu, ": no significant differences among squares"))
      }
    }
  }
}

flake.selection = two.tech.cor %>% filter(
  flake_preference == TRUE &
    size_preference == FALSE &
    strict_selection == FALSE
)
for(i in 1:length(cor.names)) {
  var = cor.names[i]
  wtest = pairwise.wilcox.test(as.vector(flake.selection[var])[[1]], flake.selection$square_num, p.adjust.method = "BH", 
                               na.action = na.omit())
  different = wtest[["p.value"]] < 0.05
  if(length(table(different)) == 2) {
    print(paste0(var, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
  } else {
    if(names(table(different)) == "TRUE") {
      print(paste0(var,  ": all squares have significant differences"))
    } else {
      print(paste0(var, ": no significant differences among squares"))
    }
  }
}


nod.selection = two.tech.cor %>% filter(
  flake_preference == FALSE &
    size_preference == FALSE &
    strict_selection == FALSE
)
for(i in 1:length(cor.names)) {
  var = cor.names[i]
  wtest = pairwise.wilcox.test(as.vector(nod.selection[var])[[1]], nod.selection$square_num, p.adjust.method = "BH", 
                               na.action = na.omit())
  different = wtest[["p.value"]] < 0.05
  if(length(table(different)) == 2) {
    print(paste0(var, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
  } else {
    if(names(table(different)) == "TRUE") {
      print(paste0(var,  ": all squares have significant differences"))
    } else {
      print(paste0(var, ": no significant differences among squares"))
    }
  }
}


##### many technology types #####
many.tech.cor = layer.cor %>% filter(overlap == 2) %>% filter(time == "end") %>% 
  group_by(row, col) %>% mutate(square_num = cur_group_id())

for(i in 1:length(cor.names)) {
  var = cor.names[i]
  wtest = pairwise.wilcox.test(as.vector(many.tech.cor[var])[[1]], many.tech.cor$square_num, p.adjust.method = "BH", 
                               na.action = na.omit())
  different = wtest[["p.value"]] < 0.05
  print(paste0(var, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
}

for(i in 1:length(cor.names)) {
  var = cor.names[i]
  for(mu in c(1:3)) {
    mu.cor = many.tech.cor[which(many.tech.cor$mu == mu), ]
    wtest = pairwise.wilcox.test(as.vector(mu.cor[var])[[1]], mu.cor$square_num, p.adjust.method = "BH", 
                                 na.action = na.omit())
    different = wtest[["p.value"]] < 0.05
    if(length(table(different)) == 2) {
      print(paste0(var, " when mu = ", mu, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
    } else {
      if(names(table(different)) == "TRUE") {
        print(paste0(var, " when mu = ", mu, ": all squares have significant differences"))
      } else {
        print(paste0(var, " when mu = ", mu, ": no significant differences among squares"))
      }
    }
  }
}

flake.selection = many.tech.cor %>% filter(
  flake_preference == TRUE &
    size_preference == FALSE &
    strict_selection == FALSE
)
for(i in 1:length(cor.names)) {
  var = cor.names[i]
  wtest = pairwise.wilcox.test(as.vector(flake.selection[var])[[1]], flake.selection$square_num, p.adjust.method = "BH", 
                               na.action = na.omit())
  different = wtest[["p.value"]] < 0.05
  if(length(table(different)) == 2) {
    print(paste0(var, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
  } else {
    if(names(table(different)) == "TRUE") {
      print(paste0(var,  ": all squares have significant differences"))
    } else {
      print(paste0(var, ": no significant differences among squares"))
    }
  }
}


nod.selection = many.tech.cor %>% filter(
  flake_preference == FALSE &
    size_preference == FALSE &
    strict_selection == FALSE
)
for(i in 1:length(cor.names)) {
  var = cor.names[i]
  wtest = pairwise.wilcox.test(as.vector(nod.selection[var])[[1]], nod.selection$square_num, p.adjust.method = "BH", 
                               na.action = na.omit())
  different = wtest[["p.value"]] < 0.05
  if(length(table(different)) == 2) {
    print(paste0(var, ": ", ((table(different)[[2]]/4950)*100), "% of comparisons produced significant differences"))
  } else {
    if(names(table(different)) == "TRUE") {
      print(paste0(var,  ": all squares have significant differences"))
    } else {
      print(paste0(var, ": no significant differences among squares"))
    }
  }
}


##### direction of correlations #####
###### recycling intensity and object count ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.obj.cnt.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
table(overlap.test.less$signf) #group1 is less than group2

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.obj.cnt.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
table(overlap.test.greater$signf) #group1 is less than group2

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.obj.cnt.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf) #for some, group1 < group2
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf) #for all, cannot reject null

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.obj.cnt.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf) #for some, group1 > group2
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf) #for some, group1 > group2

###### recycling intensity and cortex ratio ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
table(overlap.test.less$signf) #for all cannot reject null

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
table(overlap.test.greater$signf) #for all, group1 > group2

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf) #for all cannot reject null
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf) #for all cannot reject null

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf) #for all, group1 > group2
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf) #for all, group1 > group2

###### recycling intensity and number of discards ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
table(overlap.test.less$signf) #for majority, group1 < group2

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
table(overlap.test.greater$signf) # for all, cannot reject null

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf) #for majority, cannot reject null
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf) #for all, cannot reject null

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf) #for some, group1 > group2
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf) #for some, group1 > group2

###### recycling intensity and number of scavenging events ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
table(overlap.test.less$signf) #for all, cannot reject null

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
table(overlap.test.greater$signf) #for most, group1 > group2

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf) #for majority, cannot reject the null
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf) #for all but 1, cannot reject null

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf) #for some, group1 > group2
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf) #for about half, group1 > group2

###### recycling intensity and number of encounters ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
table(overlap.test.less$signf) #for most, group1 < group2

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
table(overlap.test.greater$signf) #for all, cannot reject the null

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf) #for all, cannot reject the null
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf) #for all, cannot reject null

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf) #for most, group1 > group2
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf) #for most, group1 > group2

###### recycling intensity and number of manufacture events ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
table(overlap.test.less$signf) #for most, group1 < group2

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
table(overlap.test.greater$signf) #for none, cannot reject the null

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 >= group2
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf) #for most, cannot reject the null
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf) #for most, cannot reject the null

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
#null hypothesis group1 <= group2
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf) #for majority, cannot reject null
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf) #for majority cannot reject null
