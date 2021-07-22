library(tidyverse)
library(TTR)
library(forecast)
library(rstatix)

theme_set(theme_minimal())

#setwd("..")
data = list()
index = 1

#for reading from USB
dirs = list.dirs(path = "/Volumes/USB-64/output/")
#layerfiles = c()
for(d in 2:length(dirs)) {
  files = list.files(path = dirs[d], full.names = T)
  for(f in files)  {
    if(!is.null(f)) {
      data[[index]] = read.csv(f)
      index = index + 1
    }
  }
}

#setwd("analysis")

alldata = bind_rows(data)
parameters = colnames(alldata[,c(1,4:9,11:17)])
results = colnames(alldata[,c(25:33)])

alldata$exp = alldata %>% group_indices_(unlist(parameters))

##regression over time 
tavg = alldata %>% group_by_at(unlist(c(parameters, "exp", "model_year"))) %>%
  summarize(cr = mean(cortex.ratio, na.rm = T), 
            ri = mean(recycling.intensity, na.rm = T), 
            nd = mean(num.discards, na.rm = T), 
            ne = mean(num.encounters, na.rm = T),
            nm = mean(num.encounters, na.rm = T), 
            nr = mean(num.retouch, na.rm = T), 
            no = mean(num.occupation, na.rm = T))

ggplot(data = tavg, aes(x = model_year, y = ri)) +
  geom_point(size = 0.1) +
  geom_smooth() +
  scale_x_reverse()

#see how different output measurements change over time based on specific parameters
with(tavg, interaction.plot(model_year, size, cr))
with(tavg, interaction.plot(model_year, size, ri))
with(tavg, interaction.plot(model_year, size, nd))
with(tavg, interaction.plot(model_year, size, ne))
with(tavg, interaction.plot(model_year, size, nm))
with(tavg, interaction.plot(model_year, size, nr))
with(tavg, interaction.plot(model_year, size, no))

with(tavg, interaction.plot(model_year, max_use_intensity, ri))
with(tavg, interaction.plot(model_year, max_artifact_carry, ri))
with(tavg, interaction.plot(model_year, max_flake_size, ri))
with(tavg, interaction.plot(model_year, max_nodules_size, ri))
with(tavg, interaction.plot(model_year, blank_prob, ri))
with(tavg, interaction.plot(model_year, discard_prob, ri))
with(tavg, interaction.plot(model_year, overlap, ri))
with(tavg, interaction.plot(model_year, mu, ri))
with(tavg, interaction.plot(model_year, size_preference, ri))
with(tavg, interaction.plot(model_year, flake_preference, ri))
with(tavg, interaction.plot(model_year, min_suitable_nodule_size, ri))
with(tavg, interaction.plot(model_year, min_suitable_flake_size, ri))
with(tavg, interaction.plot(model_year, strict_selection, ri))


min.ns.aov = aov(ne ~ min_suitable_nodule_size*model_year, data=tavg)
summary(min.ns.aov)


#Two-way Repeated Measures of ANOVA
#https://www.r-bloggers.com/2021/04/repeated-measures-of-anova-in-r-complete-tutorial/
blankprob = alldata %>% 
  select_at(unlist(c("exp", "blank_prob", "model_year", results))) %>%
  filter(!is.nan(recycling.intensity)) %>%
  
summary = blankprob %>%
  group_by(blank_prob, model_year) %>%
  get_summary_stats(recycling.intensity, type = "mean_sd")
outliers = blankprob %>%
  group_by(blank_prob, model_year) %>%
  identify_outliers(recycling.intensity)
#normality not working
normality = blankprob %>% 
  group_by(blank_prob, model_year) %>%
  shapiro_test(recycling.intensity) #specifically this function

blankprobtest = blankprob %>% mutate(id = rownames(blankprob))
res.aov <- anova_test( #not working
  data = blankprobtest, dv = recycling.intensity, wid = id,
  within = c(blank_prob, model_year)
)
get_anova_table(res.aov)

pwc = blankprob %>%
  group_by(model_year) %>%
  pairwise_t_test(
    recycling.intensity ~ blank_prob, paired = T, 
    p.adjust.method = "bonferroni"
  )
pwc2 = blankprob %>%
  group_by(blank_prob) %>%
  pairwise_t_test(
    recycling.intensity ~ model_year, paired = T, 
    p.adjust.method = "bonferroni"
  )



overlap = alldata %>% select_at(unlist(c("overlap", "model_year", results, "exp")))
summary = overlap %>%
  group_by(overlap, model_year) %>%
  get_summary_stats(recycling.intensity, type = "mean_sd")
outliers = overlap %>%
  group_by(overlap, model_year) %>%
  identify_outliers(recycling.intensity)
normality = overlap %>% #not working
  group_by(overlap, model_year) %>%
  shapiro_test(recycling.intensity)

res.aov <- anova_test( #not working
  data = overlap, dv = recycling.intensity, wid = exp,
  within = c(overlap, model_year)
)
get_anova_table(res.aov)


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
