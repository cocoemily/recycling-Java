#analysis of variation among model runs
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggthemes)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

allruns = alldata[which(alldata$model_year == 500000),]
exp_test = distinct(allruns[parameters])
print(nrow(exp))
exp = readr::read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
colnames(exp) = c("exp", "run", "size", "start_year", "timestep", parameters, "erosion_ratio", "geo_freq", "total_steps")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")


# oneexp = alldata[ which(
#   alldata[parameters[1]] == as.numeric(exp[1, 1]) &
#     alldata[parameters[2]] == as.numeric(exp[1, 2]) &
#     alldata[parameters[3]] == as.numeric(exp[1, 3]) &
#     alldata[parameters[4]]== as.numeric(exp[1, 4]) &
#     alldata[parameters[5]] == as.numeric(exp[1, 5]) &
#     alldata[parameters[6]] == as.numeric(exp[1, 6]) &
#     alldata[parameters[7]] == as.numeric(exp[1, 7]) &
#     alldata[parameters[8]] == as.numeric(exp[1, 8]) &
#     alldata[parameters[9]] == as.numeric(exp[1, 9]) &
#     alldata[parameters[10]] == as.numeric(exp[1, 10]) &
#     alldata[parameters[11]] == as.numeric(exp[1, 11]) &
#     alldata[parameters[12]] == as.numeric(exp[1, 12]) &
#     alldata[parameters[13]] == as.numeric(exp[1, 13])
# ),]


# allvar = oneexp %>% group_by(model_year) %>%
#   summarize(num.scav.events.var = var(num.scav.events), 
#             total.recycled.var = var(total.recycled), 
#             num.deposits.var = var(num.deposits), 
#             total.encounters.var = var(total.encounters), 
#             total.discards.var = var(total.discards), 
#             total.manu.events.var = var(total.manu.events), 
#             total.retouches.var = var(total.retouches), 
#             total.CR.var = var(total.CR), 
#             total.RI.var = var(total.RI)) %>%
#   mutate(exp = 1)

allvar = data.frame(
  num.scav.events.var = numeric(), 
  total.recycled.var = numeric(), 
  num.deposits.var = numeric(), 
  total.encounters.var = numeric(), 
  total.discards.var = numeric(), 
  total.manu.events.var = numeric(), 
  total.retouches.var = numeric(), 
  total.CR.var = numeric(), 
  total.RI.var = numeric(), 
  exp = numeric(), 
  model_year = numeric()
)

modelyears = unique(alldata$model_year)
# oneyear = oneexp[which(oneexp$model_year == modelyears[1]),]
# var = c(
#   var(oneyear$num.scav.events), 
#   var(oneyear$total.recycled), 
#   var(oneyear$num.deposits), 
#   var(oneyear$total.encounters), 
#   var(oneyear$total.discards), 
#   var(oneyear$total.manu.events), 
#   var(oneyear$total.retouches), 
#   var(oneyear$total.CR), 
#   var(oneyear$total.RI)
# )
# 
# allvar[nrow(allvar) + 1, ] <- var


for(row in 1:nrow(exp)) {
  oneexp = alldata[which(
    alldata[parameters[1]] == c(exp[row, parameters[1]]) &
      alldata[parameters[2]] == c(exp[row, parameters[2]]) &
      alldata[parameters[3]] == c(exp[row, parameters[3]]) &
      alldata[parameters[4]] == c(exp[row, parameters[4]]) &
      alldata[parameters[5]] == c(exp[row, parameters[5]]) &
      alldata[parameters[6]] == c(exp[row, parameters[6]]) &
      alldata[parameters[7]] == c(exp[row, parameters[7]]) &
      alldata[parameters[8]] == c(exp[row, parameters[8]]) &
      alldata[parameters[9]] == c(exp[row, parameters[9]]) &
      alldata[parameters[10]] == c(exp[row, parameters[10]]) &
      alldata[parameters[11]] == c(exp[row, parameters[11]]) &
      alldata[parameters[12]] == c(exp[row, parameters[12]]) &
      alldata[parameters[13]] == c(exp[row, parameters[13]])
  ),]
  
  for(year in 1:length(modelyears)) {
    oneyear = oneexp[which(oneexp$model_year == modelyears[year]),]
    var = c(
      var(oneyear$num.scav.events), 
      var(oneyear$total.recycled), 
      var(oneyear$num.deposits), 
      var(oneyear$total.encounters), 
      var(oneyear$total.discards), 
      var(oneyear$total.manu.events), 
      var(oneyear$total.retouches), 
      var(oneyear$total.CR), 
      var(oneyear$total.RI), 
      row, 
      modelyears[year]
    )
    print(var)
    
    allvar[nrow(allvar) + 1, ] <- var
  }
}

ggsave(filename = "total-RI-variation.png", 
       plot = ggplot(allvar, aes(x = model_year, y = total.RI.var, group = exp, color = exp)) +
         geom_line() +
         scale_color_colorblind()
)
