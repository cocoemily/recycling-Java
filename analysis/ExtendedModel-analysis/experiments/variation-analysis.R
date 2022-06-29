#analysis of variation among model runs
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggthemes)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
alldata = alldata[alldata$size != "size",]
alldata = alldata[!is.na(alldata$max_artifact_carry),]

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

exp = alldata %>% group_by_at(parameters) %>%
  filter(row_number() == 1) %>%
  select_at(parameters) %>%
  filter(!is.na(max_use_intensity))

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

move_outputs = c("num.deposits", "total.encounters")
scavenge_outputs = c("num.scav.events", "total.recycled", "total.discards", "total.manu.events", "total.retouches")


oneexp = alldata[ which(
  alldata[parameters[1]] == as.numeric(exp[1, 1]) &
    alldata[parameters[2]] == as.numeric(exp[1, 2]) &
    alldata[parameters[3]] == as.numeric(exp[1, 3]) &
    alldata[parameters[4]]== as.numeric(exp[1, 4]) &
    alldata[parameters[5]] == as.numeric(exp[1, 5]) &
    alldata[parameters[6]] == as.numeric(exp[1, 6]) &
    alldata[parameters[7]] == as.numeric(exp[1, 7]) &
    alldata[parameters[8]] == as.numeric(exp[1, 8]) &
    alldata[parameters[9]] == as.numeric(exp[1, 9]) &
    alldata[parameters[10]] == as.numeric(exp[1, 10]) &
    alldata[parameters[11]] == as.numeric(exp[1, 11]) &
    alldata[parameters[12]] == as.numeric(exp[1, 12]) &
    alldata[parameters[13]] == as.numeric(exp[1, 13])
),]


allvar = oneexp %>% group_by(model_year) %>%
  summarize(num.scav.events.var = var(num.scav.events), 
            total.recycled.var = var(total.recycled), 
            num.deposits.var = var(num.deposits), 
            total.encounters.var = var(total.encounters), 
            total.discards.var = var(total.discards), 
            total.manu.events.var = var(total.manu.events), 
            total.retouches.var = var(total.retouches), 
            total.CR.var = var(total.CR), 
            total.RI.var = var(total.RI)) %>%
  mutate(exp = 1)

for(row in 2:nrow(exp)) {
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
  
  
  var = oneexp %>% group_by(model_year) %>%
    summarize(num.scav.events.var = var(num.scav.events), 
              total.recycled.var = var(total.recycled), 
              num.deposits.var = var(num.deposits), 
              total.encounters.var = var(total.encounters), 
              total.discards.var = var(total.discards), 
              total.manu.events.var = var(total.manu.events), 
              total.retouches.var = var(total.retouches), 
              total.CR.var = var(total.CR), 
              total.RI.var = var(total.RI)) %>%
    mutate(exp = row)
  
  allvar = rbind(allvar, var)
}

ggsave(filename = "total-RI-variation.png", 
plot = ggplot(allvar, aes(x = model_year, y = total.RI.var, group = exp, color = exp)) +
  geom_line() +
  scale_color_colorblind()
)
  