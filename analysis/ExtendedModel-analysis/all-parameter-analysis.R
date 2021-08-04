library(tidyverse)
library(TTR)
library(forecast)
library(rstatix)
library(psych)
library(nlme)
library(car)
library(multcompView)
library(lsmeans)
library(rcompanion)

theme_set(theme_minimal())

load("~/eclipse-workspace/recycling-Java/analysis/test-data.RData")
parameters = colnames(alldata[,c(1,4:9,11:17)])
results = colnames(alldata[,c(25:33)])

#alldata$exp = alldata %>% group_indices_(unlist(parameters))
alldata = alldata %>% group_by_at(unlist(parameters)) %>%
  mutate(exp = cur_group_id())

# ggplot(data = alldata, aes(x=model_year, y=recycling.intensity, group = exp, color = exp)) + 
#   geom_smooth(se = F) +
#   scale_x_reverse()

##regression over time 
tavg = alldata %>% group_by_at(unlist(c(parameters, "exp", "model_year"))) %>%
  summarize(cr = mean(cortex.ratio, na.rm = T), 
            ri = mean(recycling.intensity, na.rm = T), 
            nd = mean(num.discards, na.rm = T), 
            ne = mean(num.encounters, na.rm = T),
            nm = mean(num.encounters, na.rm = T), 
            nr = mean(num.retouch, na.rm = T), 
            no = mean(num.occupation, na.rm = T))

# ggplot(data = tavg, aes(x = model_year, y = ri)) +
#   geom_point(size = 0.1) +
#   geom_smooth() +
#   scale_x_reverse()

#see how different output measurements change over time based on specific parameters
# with(tavg, interaction.plot(model_year, size, cr))
# with(tavg, interaction.plot(model_year, size, nd))
# with(tavg, interaction.plot(model_year, size, ne))
# with(tavg, interaction.plot(model_year, size, nm))
# with(tavg, interaction.plot(model_year, size, nr))
# with(tavg, interaction.plot(model_year, size, no))

with(tavg, interaction.plot(model_year, size, ri))
with(tavg, interaction.plot(model_year, max_use_intensity, ri))
with(tavg, interaction.plot(model_year, max_artifact_carry, ri))
with(tavg, interaction.plot(model_year, max_flake_size, ri)) #differences
with(tavg, interaction.plot(model_year, max_nodules_size, ri))
with(tavg, interaction.plot(model_year, blank_prob, ri)) #differences
with(tavg, interaction.plot(model_year, discard_prob, ri))
with(tavg, interaction.plot(model_year, overlap, ri)) #similar curves, but aov says significant differences
with(tavg, interaction.plot(model_year, mu, ri)) #differences but not as pronounced
with(tavg, interaction.plot(model_year, size_preference, ri))
with(tavg, interaction.plot(model_year, flake_preference, ri))
with(tavg, interaction.plot(model_year, min_suitable_nodule_size, ri)) #curves aren't that different but, aov says significant
with(tavg, interaction.plot(model_year, min_suitable_flake_size, ri)) #curves aren't that different but, aov says significant
with(tavg, interaction.plot(model_year, strict_selection, ri))


ovl.aov = aov(ri ~ overlap*model_year, data=tavg)
summary(ovl.aov)
pval = summary(ovl.aov)[[1]][["Pr(>F)"]]
p.adjust(pval, method = "bonferroni", n = 2)

mu.aov = aov(ri ~mu*model_year, data=tavg)
summary(mu.aov)
pval = summary(mu.aov)[[1]][["Pr(>F)"]]
p.adjust(pval, method = "bonferroni", n = length(pval))

min.ns.aov = aov(ri ~ min_suitable_nodule_size*model_year, data=tavg)
summary(min.ns.aov)
min.fl.aov = aov(ri ~ min_suitable_flake_size*model_year, data=tavg)
summary(min.fl.aov)


#Repeated Measures of ANOVA
test = alldata %>% filter(model_year < 450000 & model_year > 449000) 
ggplot(test, aes(x = as.factor(model_year), y = recycling.intensity, color = as.factor(blank_prob))) +
  geom_boxplot()
ggpubr::ggqqplot(test, "recycling.intensity", ggtheme = theme_bw()) +
  facet_grid(model_year ~ blank_prob + max_flake_size, labeller = "label_both")

#cannot do this without equal sized parameter sets
pwc <- test2 %>%
  group_by(model_year) %>%
  pairwise_t_test(
    recycling.intensity ~ blank_prob, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

##regression of end state
enddata = alldata %>% filter(model_year == min(unique(alldata$model_year))) %>%
  filter(!is.nan(recycling.intensity))

lm1 = lm(recycling.intensity ~ max_use_intensity + max_artifact_carry + max_flake_size +
     max_nodules_size + blank_prob + discard_prob + overlap + mu  + 
     min_suitable_nodule_size + min_suitable_flake_size + size, data = enddata)
summary(lm1)
plot(lm1, which=2)

lm2 = lm(num.encounters ~ size + max_use_intensity + max_artifact_carry + max_flake_size +
           max_nodules_size + blank_prob + discard_prob + overlap + mu  + 
           min_suitable_nodule_size + min_suitable_flake_size, data = enddata)
summary(lm2)
