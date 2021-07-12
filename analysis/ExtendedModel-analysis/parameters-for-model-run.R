library(tidyverse)
library(TTR)
library(forecast)

theme_set(theme_minimal())

setwd("..")
data = list()
index = 1

#for reading on HPC or local paths
for(d in list.files(path = "output", pattern = 'exp[0-9]')) {
  for(f in list.files(path = paste0("output/", d), full.names = T))  {
    data[[index]] = read.csv(f)
    index = index + 1
  }
}

#for reading from USB
# files = list.files(path = "/Volumes/USB-64/output", pattern = 'exp[0-9]')
# for(d in files) {
#   for(f in list.files(path = paste0("output/", d), full.names = T))  {
#     data[[index]] = read.csv(f)
#     index = index + 1
#   }
# }

setwd("analysis")

alldata = bind_rows(data)
parameters = colnames(alldata[,c(1,4:9,11:17)])

##regression over time 
tavg = alldata %>% group_by_at(unlist(c(parameters, "model_year"))) %>%
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
with(tavg, interaction.plot(model_year, overlap, ne))

overlap.aov = aov(ne ~ overlap*model_year, data=tavg)
summary(overlap.aov)

##regression of end state
enddata = alldata %>% filter(model_year == min(unique(alldata$model_year))) %>%
  filter(recycling.intensity != "NaN")

lm1 = lm(recycling.intensity ~ max_use_intensity + max_artifact_carry + max_flake_size +
     max_nodules_size + blank_prob + discard_prob + overlap + mu  + 
     min_suitable_nodule_size + min_suitable_flake_size + size, data = enddata)
summary(lm1)
plot(lm1, which=2)

lm2 = lm(num.encounters ~ size + max_use_intensity + max_artifact_carry + max_flake_size +
           max_nodules_size + blank_prob + discard_prob + overlap + mu  + 
           min_suitable_nodule_size + min_suitable_flake_size, data = enddata)
summary(lm2)
