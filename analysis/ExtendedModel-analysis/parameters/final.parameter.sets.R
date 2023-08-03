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
                        #minNS = numeric(),
                        strict = character(),
                        ED = numeric(),
                        GF = numeric(),
                        totalSteps = numeric(), 
                        totalAgents = numeric()) 

size.values = c(10)
maxUI.values = c(15, 30)
maxAC.values = c(10, 20)
maxFS.values = c(1, 2)
maxNS.values = c(20)
bProb.values = c(0.25, 0.5, 0.75)
sProb.values = c(0.25, 0.5, 0.75)
overlap.values = c(1, 2) #1 = randomly alternating between 2 tech types, 2 = all different tech types
mu.values = c(1, 2, 3)
sizePref.values = c("true", "false")
flakePref.values = c("true", "false")
minFS.values = c(1, 2)
##minNS.values = c(5, 10)
strict.values = c("true", "false")
agent.values = c(100, 200)

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
                          for(agents in agent.values) {
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
                              ##min.ns, #minNS
                              st, #strict
                              0.5, #ED 
                              0, #GF
                              3000, #totalSteps, 
                              agents
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

# fparameters = parameters %>% filter(!(minFS > maxFS)) %>%
#   filter(!(sizePref == "false" & maxFS != minFS)) %>% 
#   filter(!(sizePref == "false" & maxNS != minNS))

fparameters = parameters %>% filter(!(flakePref == "false" & sizePref == "true"))
fparameters = fparameters %>%
  mutate(of = seq(1, nrow(fparameters), by = 1))

setwd("..") #moves up a directory
save(fparameters, file = "analysis/ExtendedModel-analysis/parameters/modelrun-params.RData")
write.csv(fparameters, file = "run-scripts/ExtendedModel-model-runs/parameters.csv", row.names=F)


#split up parameters into smaller bit to be run on HPC
for(i in c(1:12)) {
  startrow = 1 + ((i*935) - 935)
  endrow = i * 935
  filename = paste0("run-scripts/ExtendedModel-model-runs/parameters", i, ".csv")
  write.csv(fparameters[startrow:endrow,], file = filename, row.names = F)
}
