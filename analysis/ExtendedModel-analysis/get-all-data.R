library(tidyverse)

# data = list()
# index = 1
# 
#for reading on HPC or local paths
# for(d in list.dirs(path = "../output")) {
#   files = list.files(path = dirs[d], full.names = T)
#   for(f in files)  {
#     if(!is.null(f)) {
#       data[[index]] = read.csv(f)
#       index = index + 1
#     }
#   }
# }
# alldata = bind_rows(data)


alldata = data.frame()
for(d in list.dirs(path = "../output")) {
  files = list.files(path = dirs[d], full.names = T)
  for(f in files)  {
    if(!is.null(f)) {
      alldata = rbind(alldata, read.csv(f))
    }
  }
}
save(alldata, file = "alldata.RData")

parameters = colnames(alldata[,c(1,4:9,11:17)])
tavg = alldata %>% group_by_at(unlist(c(parameters, "exp", "model_year"))) %>%
  summarize(cr = mean(cortex.ratio, na.rm = T), 
            ri = mean(recycling.intensity, na.rm = T), 
            nd = mean(num.discards, na.rm = T), 
            ne = mean(num.encounters, na.rm = T),
            nm = mean(num.encounters, na.rm = T), 
            nr = mean(num.retouch, na.rm = T), 
            no = mean(num.occupation, na.rm = T))

save(tavg, file = "averaged-parameters-time.RData")