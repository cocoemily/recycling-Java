library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(betareg)
library(jtools)
library(groupedstats)
library(rstatix)

source("ExtendedModel-analysis/experiments/results-analysis/helper-functions.R")

theme_set(theme_bw())

#### ANALYSIS OF CORRELATION BETWEEN OUTPUTS WITHIN GRID SQUARES ACROSS MODEL RUNS ####
layer.cor = read_csv("~/eclipse-workspace/recycling-Java/results/all-layer-gridded-cor-output.csv")
cor.names = colnames(layer.cor[,17:25])
cor.names = cor.names[-length(cor.names)] #removing occupations correlation because encounters = occupations

mid.cor.ri = layer.cor %>% filter(time == "mid") %>% 
  gather(key = "comparison", value = "correlation", starts_with("ri"))

end.cor.ri = layer.cor %>% filter(time == "end") %>% 
  gather(key = "comparison", value = "correlation", starts_with("ri"))

ggplot(mid.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)

ggplot(end.cor.ri) +
  geom_boxplot(aes(x = comparison, y = correlation, group = comparison, color = comparison)) +
  facet_grid(row ~ col)


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
table(overlap.test.less$signf)

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.obj.cnt.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.obj.cnt.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.obj.cnt.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)

###### recycling intensity and cortex ratio ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
table(overlap.test.less$signf)

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
table(overlap.test.greater$signf)

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf)
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf)

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.cr.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf)
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf)

###### recycling intensity and number of discards ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
table(overlap.test.less$signf)

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
table(overlap.test.greater$signf)

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf)
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf)

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.disc.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf)
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf)

###### recycling intensity and number of scavenging events ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
table(overlap.test.less$signf)

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
table(overlap.test.greater$signf)

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
table(mu.less.overlap1$signf)
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)
table(mu.less.overlap2$signf)

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.scvg.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
table(mu.greater.overlap1$signf)
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
table(mu.greater.overlap2$signf)

###### recycling intensity and number of encounters ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.enct.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)

###### recycling intensity and number of manufacture events ######
overlap.test.less = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$overlap, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)

overlap.test.greater = layer.cor %>% group_by(row, col) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$overlap, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)

mu.test.less = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$mu, alternative = c("less")))) %>%
  mutate(signf = p.value < 0.05)
mu.less.overlap1 = mu.test.less %>% filter(overlap == 1)
mu.less.overlap2 = mu.test.less %>% filter(overlap == 2)

mu.test.greater = layer.cor %>% group_by(row, col, overlap) %>%
  group_modify(~ broom::tidy(pairwise.wilcox.test(.x$ri.num.manu.cor, .x$mu, alternative = c("greater")))) %>%
  mutate(signf = p.value < 0.05)
mu.greater.overlap1 = mu.test.greater %>% filter(overlap == 1)
mu.greater.overlap2 = mu.test.greater %>% filter(overlap == 2)
