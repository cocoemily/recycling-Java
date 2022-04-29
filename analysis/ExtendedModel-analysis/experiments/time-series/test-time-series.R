#testing time series analysis
library(distantia)
library(tidyverse)
library(lmtest)

theme_set(theme_minimal())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
#alldata = readr::read_csv("~/eclipse-workspace/recycling-Java/output/ss_model_data.csv")

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size","max_nodules_size", "blank_prob", "scavenge_prob", "overlap","mu", "size_preference", "flake_preference","min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

outputs = c("num.scav.events","total.recycled", "num.deposits",	"total.encounters",	"total.discards",	"total.manu.events", "total.retouches", "total.CR",	"total.RI")

alldata = alldata[alldata$size != "size",]

alldata = alldata[!is.na(alldata$max_artifact_carry),]


start = 1
end = 3001
step = 3001
allseq = alldata[0, c("model_year", outputs)] %>% mutate(exp = "none")

for(i in 1:12096) {
  for(j in 1:50) {
  
  oneexp = alldata[start:end,c("model_year", outputs)] %>% mutate(exp = paste0(i, "_", j))
  
  allseq = rbind(allseq, oneexp)
  
  
  start = start + step
  end = start + step - 1
  
  
  }
}

saveRDS(allseq, file = "allseq_ts.rds")

aseq.SE = allseq %>% select(model_year, exp, num.scav.events)
aseq.TRec = allseq %>% select(model_year, exp, total.recycled)
aseq.ND = allseq %>% select(model_year, exp, num.deposits)
aseq.TE = allseq %>% select(model_year, exp, total.encounters)
aseq.TD = allseq %>% select(model_year, exp, total.discards)
aseq.ME = allseq %>% select(model_year, exp, total.manu.events)
aseq.TRet = allseq %>% select(model_year, exp, total.retouches)
aseq.CR = allseq %>% select(model_year, exp, total.CR)
aseq.RI = allseq %>% select(model_year, exp, total.RI)


#functions for comparing all sequences
sequence_distances = function(seq.dataset) {
  seqs = prepareSequences(
    sequences = as.data.frame(seq.dataset),
    grouping.column = "exp",
    time.column = "model_year",
    paired.samples = TRUE,
    transformation = "scale"
  )
  dist = distancePairedSamples(
    sequences = seqs, 
    grouping.column = "exp", 
    time.column = "model_year", 
    method = "euclidean"
  )
  return(dist)
}

sequence_psi = function(seq.dataset) {
  seqs = prepareSequences(
    sequences = as.data.frame(seq.dataset),
    grouping.column = "exp",
    time.column = "model_year",
    paired.samples = TRUE,
    transformation = "scale"
  )
  psi = workflowPsi(
    sequences = as.data.frame(seq.dataset),
    grouping.column = "exp",
    time.column = "model_year",
    method = "euclidean",
    paired.samples = TRUE, #this bit is important
    same.time = TRUE, #removes samples with unequal time
    format = "dataframe"
  )
  return(psi)
}

#test 
seq.SE = prepareSequences(
    sequences = as.data.frame(aseq.SE),
    grouping.column = "exp",
    time.column = "model_year",
    paired.samples = TRUE,
    transformation = "scale"
)

test.seq = seq.SE %>%
  filter(exp %in% c(
    "1_1", "2_1", "3_1", "4_1", "5_1", "6_1", 
    "7_1", "8_1", "9_1", "10_1", "11_1", "12_1", 
    "13_1", "14_1", "15_1", "16_1", "17_1", "18_1", 
    "19_1", "20_1", "21_1", "22_1", "23_1", "24_1", "25_1"
  ))

##calculate psi dissimilarity score
psi = workflowPsi(
  sequences = test.seq,
  grouping.column = "exp",
  time.column = "model_year",
  method = "euclidean",
  paired.samples = TRUE, #this bit is important
  same.time = TRUE, #removes samples with unequal time
  format = "dataframe"
)

#split psi values by experiment and run number
#split psi values by parameters?
parameters = readr::read_csv("../run-scripts/ExtendedModel-model-runs/parameters.csv")
mod.psi = psi %>% 
  separate(A, c("exp_A", "run_A"), sep = "_") %>%
  separate(B, c("exp_B", "run_B"), sep = "_") %>% 
  mutate(exp_A = as.numeric(exp_A), 
         exp_B = as.numeric(exp_B), 
         run_A = as.numeric(run_A), 
         run_B = as.numeric(run_B)) %>%
  left_join(parameters, by = c("exp_A" = "of")) %>%
  left_join(parameters, by = c("exp_B" = "of"), suffix = c(".A", ".B"))
  

#plotting dissimilarity scores
ggplot(data=na.omit(mod.psi), aes(x=exp_A, y=exp_B, fill=psi)) + 
  geom_tile() +
  viridis::scale_fill_viridis(direction = -1) +
  guides(size = "none") +
  scale_x_continuous("exp_A", labels = as.character(mod.psi$exp_A), breaks = mod.psi$exp_A)+
  scale_y_continuous("exp_B", labels = as.character(mod.psi$exp_B), breaks = mod.psi$exp_B) +
  ggtitle("Title")

