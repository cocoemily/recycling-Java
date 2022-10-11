#analysis of variation among model runs
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggthemes)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

# exp = alldata %>% group_by_at(parameters) %>%
#   filter(row_number() == 1) %>%
#   select_at(parameters) %>%
#   filter(!is.na(max_use_intensity))

allruns = alldata[which(alldata$model_year == 500000),]
exp2 = distinct(allruns[parameters])

#this gives all of the unique combinations of all of the parameters
#but not all combinations are tested in my model
# exp_grid = expand.grid(max_use_intensity = unique(alldata[[parameters[1]]]), 
#                        max_artifact_carry = unique(alldata[[parameters[2]]]), 
#                        max_flake_size = unique(alldata[[parameters[3]]]), 
#                        max_nodules_size = unique(alldata[[parameters[4]]]), 
#                        blank_prob = unique(alldata[[parameters[5]]]), 
#                        scavenge_prob = unique(alldata[[parameters[6]]]), 
#                        overlap = unique(alldata[[parameters[7]]]), 
#                        mu = unique(alldata[[parameters[8]]]), 
#                        size_preference = unique(alldata[[parameters[9]]]), 
#                        flake_preference = unique(alldata[[parameters[10]]]), 
#                        min_suitable_flake_size = unique(alldata[[parameters[11]]]), 
#                        min_suitable_nodule_size = unique(alldata[[parameters[12]]]), 
#                        strict_selection = unique(alldata[[parameters[13]]]))
# test = distinct(exp_grid)

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
  oneexp = alldata[ which(
    alldata[parameters[1]] == as.numeric(exp[row, 1]) &
      alldata[parameters[2]] == as.numeric(exp[row, 2]) &
      alldata[parameters[3]] == as.numeric(exp[row, 3]) &
      alldata[parameters[4]]== as.numeric(exp[row, 4]) &
      alldata[parameters[5]] == as.numeric(exp[row, 5]) &
      alldata[parameters[6]] == as.numeric(exp[row, 6]) &
      alldata[parameters[7]] == as.numeric(exp[row, 7]) &
      alldata[parameters[8]] == as.numeric(exp[row, 8]) &
      alldata[parameters[9]] == as.numeric(exp[row, 9]) &
      alldata[parameters[10]] == as.numeric(exp[row, 10]) &
      alldata[parameters[11]] == as.numeric(exp[row, 11]) &
      alldata[parameters[12]] == as.numeric(exp[row, 12]) &
      alldata[parameters[13]] == as.numeric(exp[row, 13])
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
    
    allvar[nrow(allvar) + 1, ] <- var
  }
}

ggsave(filename = "total-RI-variation.png", 
       plot = ggplot(allvar, aes(x = model_year, y = total.RI.var, group = exp, color = exp)) +
         geom_line() +
         scale_color_colorblind()
)
