library(tidyverse)
library(batman)

parameter.list <- readxl::read_excel("experiment-parameters.xlsx", sheet = "ExtendedModel")
parameter.list = as.data.frame(parameter.list[1:2, 2:22])
colnames(parameter.list) = parameter.list[1,]
parameters = parameter.list[1,]
parameter.list = parameter.list[2,]

size.values = as.numeric(unlist((parameter.list %>% select(size))[1,1] %>% str_split(", ")))
maxUI.values = as.numeric(unlist((parameter.list %>% select(maxUI))[1,1] %>% str_split(", ")))
maxAC.values = as.numeric(unlist((parameter.list %>% select(maxAC))[1,1] %>% str_split(", ")))
maxFS.values = as.numeric(unlist((parameter.list %>% select(maxFS))[1,1] %>% str_split(", ")))
maxNS.values = as.numeric(unlist((parameter.list %>% select(maxNS))[1,1] %>% str_split(",")))
bProb.values = as.numeric(unlist((parameter.list %>% select(bProb))[1,1] %>% str_split(", ")))
sProb.values = as.numeric(unlist((parameter.list %>% select(sProb))[1,1] %>% str_split(", ")))
overlap.values = as.numeric(unlist((parameter.list %>% select(overlap))[1,1] %>% str_split(", ")))
mu.values = as.numeric(unlist((parameter.list %>% select(mu))[1,1] %>% str_split(", ")))
sizePref.values = as.numeric(to_logical(unlist((parameter.list %>% select(sizePref))[1,1] %>% str_split(", "))))
flakePref.values = as.numeric(to_logical(unlist((parameter.list %>% select(flakePref))[1,1] %>% str_split(", "))))
minFS.values = as.numeric(unlist((parameter.list %>% select(minFS))[1,1] %>% str_split(", ")))
minNS.values = as.numeric(unlist((parameter.list %>% select(minNS))[1,1] %>% str_split(", ")))
strict.values = as.numeric(to_logical(unlist((parameter.list %>% select(strict))[1,1] %>% str_split(", "))))

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

parameters = parameters %>%
  mutate(sizePref = tolower(as.character(as.logical(sizePref))), 
         flakePref = tolower(as.character(as.logical(flakePref))), 
         strict = tolower(as.character(as.logical(strict))))

save(parameters, file = "ExtendedModel-analysis/parameters/parameters.RData")

options(scipen=10)
parameters[,c(2:13, 16, 17, 19:ncol(parameters))] = lapply(parameters[,c(2:13, 16, 17, 19:ncol(parameters))], function(x) as.numeric(as.character(x)))

setwd("..") #moves up a directory
write.csv(parameters, file = "run-scripts/parameters.csv", row.names=F)
write.csv(parameters[1:10,], file = "run-scripts/test.csv", row.names=F)
setwd("analysis") #move back to analysis folder


#need different parameter files to run on HPC
paramcsv = list()
rows = 2765
firstrow = 1
for(i in 1:40) {
  toadd = parameters[firstrow:(rows+firstrow),]
  paramcsv[[i]] = toadd
  firstrow = firstrow + rows
}

setwd("..")
for(i in 1:length(paramcsv)) {
  filename = paste0("run-scripts/ExtendedModel/params", i, ".csv")
  write.csv(paramcsv[[i]], file = filename, row.names=F)
}
setwd("analysis") #move back to analysis folder


##get csv with missing parameters from previous model runs
missing = c(5533, 16598, 33168, 47305, 47308, 47312, 47457, 47503, 
            47515, 47554, 47555, 47558, 47560, 47561, 47562, 47563, 
            47564, 47570, 47571, 47574, 47576, 47577, 47578, 47579, 
            47580, 47581, 47587, 47589, 47590, 47594, 47596, 47599, 
            47601, 47605, 47607, 47608, 47611, 47612, 47616, 88709, 
            88995, 88996, 89003, 89014, 89016, 89019, 89020, 89021, 
            89023, 102529, 102532, 102534, 102537, 102541, 102543, 102545, 
            102546, 102548, 102549, 102551, 102553, 102554, 102556, 102557, 
            102558, 102560, 102625, 102627, 102628, 102632, 102634, 102639, 
            102640, 102641, 102645, 102649, 102652, 102654, 102655, 102656, 
            102724, 102726, 102728, 102729, 102732, 102733, 102735, 102736, 
            102739, 102740, 102741, 102743, 102744, 102745, 102746, 102748, 
            102751, 102752, 102817, 102818, 102819, 102820, 102821, 102822, 
            102823, 102824, 102825, 102826, 102827, 102828, 102829, 102830, 
            102831, 102832, 102833, 102834, 102835, 102836, 102837, 102838, 
            102839, 102840, 102841, 102842, 102843, 102844, 102845, 102846, 
            102847, 102848, 102868)
mparams = parameters %>%
  filter(of %in% missing)

setwd("..")
write.csv(mparams, file = "run-scripts/ExtendedModel/mparams.csv", row.names = F)
setwd("analysis") #move back to analysis folder


#due to problems with the heap memory size
stillmissing = c(33186, 47554, 47558, 47560, 47562, 47564, 47571, 47574, 
                 47577, 47580, 47599, 47616, 89014, 89019, 89021, 102529, 
                 102532, 102534, 102537, 102541, 102545, 102546, 102549, 
                 102551, 102553, 102558, 102560, 102628, 102632, 102634, 
                 102654, 102656, 102728, 102729, 102732, 102735, 102736, 
                 102739, 102740, 102741, 102746, 102751, 102752, 102817, 
                 102818, 102819, 102820, 102821, 102822, 102823, 102824, 
                 102825, 102826, 102827, 102828, 102829, 102830, 102831, 
                 102832, 102833, 102834, 102835, 102836, 102837, 102838, 
                 102839, 102840, 102841, 102842, 102843, 102844, 102845, 
                 102846, 102847, 102848)

mparams2 = parameters %>%
  filter(of %in% stillmissing)

setwd("..")
write.csv(mparams2, file = "run-scripts/ExtendedModel/mparams2.csv", row.names = F)
setwd("analysis") #move back to analysis folder


test = mparams2 %>% filter(of %in% c(47554, 47558, 47560, 47562)) %>%
  mutate(of = c("test1", "test2", "test3", "test4"))

setwd("..")
write.csv(test, file = "run-scripts/ExtendedModel/test.csv", row.names = F)
setwd("analysis") #move back to analysis folder
