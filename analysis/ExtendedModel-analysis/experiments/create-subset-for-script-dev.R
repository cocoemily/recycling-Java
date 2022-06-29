library(tidyverse)


alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

exp = alldata %>% group_by_at(parameters) %>%
  filter(row_number() == 1) %>%
  select_at(parameters) %>%
  filter(!is.na(max_use_intensity))

##CREATE MODEL DATA SUBSET
# 
# subset = data.frame()
# 
# for(row in 1:nrow(exp)) {
#   oneexp = alldata[ which( 
#     alldata[parameters[1]] == as.numeric(exp[row, 1]) & 
#       alldata[parameters[2]] == as.numeric(exp[row, 2]) &
#       alldata[parameters[3]] == as.numeric(exp[row, 3]) &
#       alldata[parameters[4]]== as.numeric(exp[row, 4]) & 
#       alldata[parameters[5]] == as.numeric(exp[row, 5]) &
#       alldata[parameters[6]] == as.numeric(exp[row, 6]) &
#       alldata[parameters[7]] == as.numeric(exp[row, 7]) &
#       alldata[parameters[8]] == as.numeric(exp[row, 8]) &
#       alldata[parameters[9]] == as.numeric(exp[row, 9]) &
#       alldata[parameters[10]] == as.numeric(exp[row, 10]) &
#       alldata[parameters[11]] == as.numeric(exp[row, 11]) &
#       alldata[parameters[12]] == as.numeric(exp[row, 12]) &
#       alldata[parameters[13]] == as.numeric(exp[row, 13])
#   ),]
#   
#   subset = rbind(subset, oneexp[1:3001,])
#   
# }
# 
# write_csv(subset, file = "/scratch/ec3307/recycling-Java/output/exp-data-subset.csv")


##CREATE LAYERS DATA SUBSET
rm(alldata)

layerdata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_layers_data.csv", locale = locale(encoding = "ASCII"))
layerdata = layerdata[layerdata$size != "size",]
layerdata = layerdata[!is.na(layerdata$max_artifact_carry),]

subset = data.frame()

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
  
  subset = rbind(subset, oneexp[1:30201,])
  
}

write_csv(subset, file = "/scratch/ec3307/recycling-Java/output/layer-data-subset.csv")
