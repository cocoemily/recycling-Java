library(tidyverse)
theme_set(theme_minimal())

maxAC = list.files(path="../output/test-maxAC", full.names = T)
maxUI = list.files(path="../output/test-maxUI", full.names = T)
numAg = list.files(path="../output/test-numAgents", full.names = T)
int = list.files(path="../output/test-INT", full.names = T)


# artifact_data = function(filename) { #not working
#   afiles = filename[str_detect(filename, pattern = "artifacts")]
#   adata = as.data.frame(read.csv(afiles[1]))
#   for(a in 2:length(adata)) {
#     adf = as.data.frame(read.csv(afiles[a]))
#     adata = rbind(adata, adf)
#   }
#   
#   return(adata)
# }

layers_data = function(filename) {
  lfiles = filename[str_detect(filename, pattern = "layers")]
  ldata = read.csv(lfiles[1])
  for(l in 2:length(lfiles)) {
    ldata = rbind(ldata, read.csv(lfiles[l]))
  }
  layers1 = ldata %>% filter(ldata[,29:33] != 0) %>% 
    filter(model_year %in% head(unique(layers$model_year), n=(length(unique(layers$model_year)))/3))
  layers2 = ldata %>% filter(ldata[,29:33] != 0) %>% 
    filter(model_year %in% tail(unique(layers$model_year), n=(length(unique(layers$model_year)))/3))
  layers = layers2 = ldata %>% filter(ldata[,29:33] != 0)
  
  return(layers)
}

layers.ac = layers_data(maxAC)

# test = tail(unique(layers$model_year), n=(length(unique(layers$model_year)))/3)
# ltest = layers %>% filter(model_year %in% test) 

l.ac = layers.ac %>% group_by(max_artifact_carry, model_year) %>%
  summarize(avg.ncount = mean(nodule.count), 
            avg.fcount = mean(flake.count), 
            avg.cr = mean(cortex.ratio, na.rm=T), 
            avg.ri = mean(recycling.intensity, na.rm=T), 
            var.ri = var(recycling.intensity, na.rm=T), 
            avg.dis = mean(num.discards), 
            avg.ret = mean(num.retouch), 
            avg.occ = mean(num.occupation))


ggplot(l.ac, aes(x = model_year, y = avg.ri)) +
  geom_line() +
  facet_wrap(~max_artifact_carry) + 
  scale_x_reverse()


layers.ui = layers_data(maxUI)

l.ui = layers.ui %>% group_by(max_use_intensity, model_year) %>%
  summarize(avg.ncount = mean(nodule.count), 
            avg.fcount = mean(flake.count), 
            avg.cr = mean(cortex.ratio, na.rm=T), 
            avg.ri = mean(recycling.intensity, na.rm=T), 
            var.ri = var(recycling.intensity, na.rm=T),
            avg.dis = mean(num.discards), 
            avg.ret = mean(num.retouch), 
            avg.occ = mean(num.occupation))


ggplot(l.ui, aes(x = model_year, y = var.ri)) +
  geom_line() +
  facet_wrap(~max_use_intensity) + 
  scale_x_reverse()


layers.int = layers_data(int)
l.int = layers.int %>% group_by(max_use_intensity, max_artifact_carry, model_year) %>%
  summarize(avg.ncount = mean(nodule.count), 
            avg.fcount = mean(flake.count), 
            avg.cr = mean(cortex.ratio, na.rm=T), 
            avg.ri = mean(recycling.intensity, na.rm=T), 
            var.ri = var(recycling.intensity, na.rm=T),
            avg.dis = mean(num.discards), 
            avg.ret = mean(num.retouch), 
            avg.occ = mean(num.occupation))

ggplot(l.int, aes(x = model_year, y = var.ri)) +
  geom_line() +
  facet_grid(max_artifact_carry~max_use_intensity) + 
  scale_x_reverse()


  

##model data
model_data = function(filename) {
  mfiles = filename[str_detect(filename, pattern = "model")]
  mdata = read.csv(mfiles[1])
  for(l in 2:length(mfiles)) {
    mdata = rbind(mdata, read.csv(mfiles[l]))
  }
  
  return(mdata)
}

maxAC.m = model_data(maxAC)


##equations for determining number of agents
experiment_parameters <- read_excel("~/eclipse-workspace/recycling-Java/output/experiment-parameters.xlsx", sheet = "test-numAgents")

eq = lm(last_agent ~ timesteps, data = experiment_parameters)
summary(eq)



