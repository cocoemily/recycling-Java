library(tidyverse)

options(scipen=10)
parameters = data.frame(of = character(),
                        name = character(),
                        size = numeric(),
                        startYear = numeric(),
                        timestep = numeric(),
                        maxUI = numeric(),
                        maxAC = numeric(),
                        maxFS = numeric(),
                        maxNS = numeric(),
                        bProb = numeric(),
                        sProb = numeric(),
                        overlap = numeric(),
                        mu = numeric(),
                        sizePref = character(),
                        flakePref = character(),
                        minFS = numeric(),
                        minNS = numeric(),
                        strict = character(),
                        ED = numeric(),
                        GF = numeric(),
                        totalSteps = numeric()) 

size.values = c(5)
maxUI.values = c(15, 30)
maxAC.values = c(10, 20)
maxFS.values = c(1, 2)
maxNS.values = c(10, 20)
bProb.values = c(0.25, 0.5, 0.75)
sProb.values = c(0.25, 0.5, 0.75)
overlap.values = c(1, 2) #1 = randomly alternating between 2 tech types, 2 = all different tech types
mu.values = c(1, 2, 3)
sizePref.values = c("true", "false")
flakePref.values = c("true", "false")
minFS.values = c(1, 2)
minNS.values = c(10)
strict.values = c("true", "false")

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
                  for(fprf in flakePref.values) {
                    for(st in strict.values) {
                      for(sprf in sizePref.values) {
                        for(min.fs in minFS.values) {
                          for(min.ns in minNS.values) {
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
                              3000 #totalSteps 
                            )
                            parameters[nrow(parameters) + 1, ] = newrow
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

fparameters = parameters %>% filter(!(minFS > maxFS)) %>%
  filter(!(sizePref == "false" & maxFS != minFS))
fparameters = fparameters %>%
  mutate(of = seq(1, nrow(fparameters), by = 1))

save(fparameters, file = "ExtendedModel-analysis/parameters/modelrun-params.RData")

setwd("..") #moves up a directory
write.csv(fparameters, file = "run-scripts/ExtendedModel-model-runs/parameters.csv", row.names=F)


#split up parameters into smaller bit to be run on HPC
write.csv(fparameters[1:1000,], file = "run-scripts/ExtendedModel-model-runs/parameters1.csv", row.names = F)
write.csv(fparameters[1001:2000,], file = "run-scripts/ExtendedModel-model-runs/parameters2.csv", row.names = F)
write.csv(fparameters[2001:3000,], file = "run-scripts/ExtendedModel-model-runs/parameters3.csv", row.names = F)
write.csv(fparameters[3001:4000,], file = "run-scripts/ExtendedModel-model-runs/parameters4.csv", row.names = F)
write.csv(fparameters[4001:5000,], file = "run-scripts/ExtendedModel-model-runs/parameters5.csv", row.names = F)
write.csv(fparameters[5001:6000,], file = "run-scripts/ExtendedModel-model-runs/parameters6.csv", row.names = F)
write.csv(fparameters[6001:7000,], file = "run-scripts/ExtendedModel-model-runs/parameters7.csv", row.names = F)
write.csv(fparameters[7001:8000,], file = "run-scripts/ExtendedModel-model-runs/parameters8.csv", row.names = F)
write.csv(fparameters[8001:8640,], file = "run-scripts/ExtendedModel-model-runs/parameters9.csv", row.names = F)
