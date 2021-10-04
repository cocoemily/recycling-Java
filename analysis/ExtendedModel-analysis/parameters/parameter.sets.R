library(tidyverse)
library(batman)

parameter.list <- readxl::read_excel("experiment-parameters.xlsx", sheet = "ExtendedModel")
parameter.list = as.data.frame(parameter.list[1:2, 2:22])
colnames(parameter.list) = parameter.list[1,]
parameters = parameter.list[1,]
parameter.list = parameter.list[2,]

#size.values = as.numeric(unlist((parameter.list %>% select(size))[1,1] %>% str_split(", ")))
size.values = c(5)
maxUI.values = as.numeric(unlist((parameter.list %>% select(maxUI))[1,1] %>% str_split(", ")))
maxAC.values = as.numeric(unlist((parameter.list %>% select(maxAC))[1,1] %>% str_split(", ")))
maxFS.values = as.numeric(unlist((parameter.list %>% select(maxFS))[1,1] %>% str_split(", ")))
maxNS.values = as.numeric(unlist((parameter.list %>% select(maxNS))[1,1] %>% str_split(",")))
bProb.values = as.numeric(unlist((parameter.list %>% select(bProb))[1,1] %>% str_split(", ")))
sProb.values = as.numeric(unlist((parameter.list %>% select(sProb))[1,1] %>% str_split(", ")))
overlap.values = as.numeric(unlist((parameter.list %>% select(overlap))[1,1] %>% str_split(", ")))
mu.values = as.numeric(unlist((parameter.list %>% select(mu))[1,1] %>% str_split(", ")))
sizePref.values = c("true", "false")
flakePref.values = c("true", "false")
minFS.values = as.numeric(unlist((parameter.list %>% select(minFS))[1,1] %>% str_split(", ")))
minNS.values = as.numeric(unlist((parameter.list %>% select(minNS))[1,1] %>% str_split(", ")))
strict.values = c("true", "false")

#iterate through all values lists to output a string
exp.num = 1
for(s in size.values) {
  for(ui in maxUI.values) {
    for(ac in maxAC.values) {
      for(max.fs in maxFS.values) {
        for(max.ns in maxNS.values) {
          for(bp in bProb.values) {
            for(sp in sProb.values) {
              for(o in overlap.values) {
                for(m in mu.values) {
                  for(sprf in sizePref.values) {
                    for(fprf in flakePref.values) {
                      for(min.fs in minFS.values) {
                        for(min.ns in minNS.values) {
                          for(st in strict.values) {
                            newrow = c(
                              exp.num, #of
                              0, #name
                              s, #size
                              500000, #startYear
                              100, #timestep
                              ui, #maxUI
                              ac, #maxAC
                              max.fs, #maxFS
                              max.ns, #maxNS
                              bp, #bProb
                              sp, #sProb
                              o, #overlap
                              m, #mu
                              sprf, #sizePref
                              fprf, #flakePref
                              min.fs, #minFS
                              min.ns, #minNS
                              st, #strict
                              0.5, #ED 
                              0, #GF
                              5000 #totalSteps 
                            )
                            parameters = rbind(parameters, newrow)
                            exp.num = exp.num + 1
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}


save(parameters, file = "ExtendedModel-analysis/parameters/parameters.RData")

options(scipen=10)
parameters[,c(2:13, 16, 17, 19:ncol(parameters))] = lapply(parameters[,c(2:13, 16, 17, 19:ncol(parameters))], function(x) as.numeric(as.character(x)))
parameters = parameters[-1,]

#need different parameter files to run on HPC
paramcsv = list()
rows = 2000
firstrow = 1
for(i in 1:28) {
  toadd = parameters[firstrow:(rows+firstrow),]
  paramcsv[[i]] = toadd
  firstrow = firstrow + rows + 1
}



setwd("..")
for(i in 1:length(paramcsv)) {
  filename = paste0("run-scripts/ExtendedModel-sensitivity/params", i, ".csv")
  write.csv(paramcsv[[i]], file = filename, row.names=F)
}
setwd("analysis") #move back to analysis folder


missing = parameters %>% filter(of %in% c(98, 99, 4033))
setwd("..")
write.csv(missing, file = "run-scripts/ExtendedModel-sensitivity/mparams.csv", row.names=F)
setwd("analysis") #move back to analysis folder
