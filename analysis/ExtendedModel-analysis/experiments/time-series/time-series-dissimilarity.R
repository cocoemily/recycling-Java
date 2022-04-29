library(distantia)
library(tidyverse)
library(lmtest)

theme_set(theme_minimal())

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
#alldata = readr::read_csv("~/eclipse-workspace/recycling-Java/output/ss_model_data.csv")

parameters = readr::read_csv("../run-scripts/ExtendedModel-model-runs/parameters.csv")
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
  
  mod.psi = psi %>% 
    separate(A, c("exp_A", "run_A"), sep = "_") %>%
    separate(B, c("exp_B", "run_B"), sep = "_") %>% 
    mutate(exp_A = as.numeric(exp_A), 
           exp_B = as.numeric(exp_B), 
           run_A = as.numeric(run_A), 
           run_B = as.numeric(run_B)) %>%
    left_join(parameters, by = c("exp_A" = "of")) %>%
    left_join(parameters, by = c("exp_B" = "of"), suffix = c(".A", ".B"))
  
  return(mod.psi)
}

plot_psi_values = function(seq.dataset, name) {
  mod.psi = sequence_psi(seq.dataset)
  
  ggplot(data=na.omit(mod.psi), aes(x=exp_A, y=exp_B, fill=psi)) + 
    geom_tile() +
    viridis::scale_fill_viridis(direction = -1) +
    guides(size = "none") +
    scale_x_continuous("exp_A", labels = as.character(mod.psi$exp_A), breaks = mod.psi$exp_A)+
    scale_y_continuous("exp_B", labels = as.character(mod.psi$exp_B), breaks = mod.psi$exp_B) +
    ggtitle(name)
}


# aseq.SE = allseq %>% select(model_year, exp, num.scav.events)
# aseq.TRec = allseq %>% select(model_year, exp, total.recycled)
# aseq.ND = allseq %>% select(model_year, exp, num.deposits)
# aseq.TE = allseq %>% select(model_year, exp, total.encounters)
# aseq.TD = allseq %>% select(model_year, exp, total.discards)
# aseq.ME = allseq %>% select(model_year, exp, total.manu.events)
# aseq.TRet = allseq %>% select(model_year, exp, total.retouches)
# aseq.CR = allseq %>% select(model_year, exp, total.CR)
# aseq.RI = allseq %>% select(model_year, exp, total.RI)

# psi = sequence_psi(allseq %>% select(model_year, exp, num.scav.events))
# psi = sequence_psi(allseq %>% select(model_year, exp, total.recycled))
# psi = sequence_psi(allseq %>% select(model_year, exp, num.deposits))
# psi = sequence_psi(allseq %>% select(model_year, exp, total.encounters))
# psi = sequence_psi(allseq %>% select(model_year, exp, total.discards))
# psi = sequence_psi(allseq %>% select(model_year, exp, total.manu.events))
# psi = sequence_psi(allseq %>% select(model_year, exp, total.retouches))
# psi = sequence_psi(allseq %>% select(model_year, exp, total.CR))
# psi = sequence_psi(allseq %>% select(model_year, exp, total.RI))

ggsave("psi_num-scav-events.png", 
       plot = plot_psi_values((allseq %>% select(model_year, exp, num.scav.events)), "Number of scavenging events"), 
       dpi = 300, height = 7, width = 7)
# ggsave("psi_total-recycled.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, total.recycled)), "Number of recycled objects"), 
#        dpi = 300, height = 7, width = 7)
# ggsave("psi_num-desposits.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, num.deposits)), "Number of assemblages"), 
#        dpi = 300, height = 7, width = 7)
# ggsave("psi_total-encounters.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, total.encounters)), "Number of encounters with grid squares"), 
#        dpi = 300, height = 7, width = 7)
# ggsave("psi_total-discards.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, total.discards)), "Number of discard events"), 
#        dpi = 300, height = 7, width = 7)
# ggsave("psi_total-manu-events.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, total.manu.events)), "Number of manufacturing events"), 
#        dpi = 300, height = 7, width = 7)
# ggsave("psi_total-retouches.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, total.retouches)), "Number of retouch events"), 
#        dpi = 300, height = 7, width = 7)
# ggsave("psi_total-CR.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, total.CR)), "Cortex ratio values"), 
#        dpi = 300, height = 7, width = 7)
# ggsave("psi_total-RI.png", 
#        plot = plot_psi_values((allseq %>% select(model_year, exp, total.RI)), "Recycling intensity values"), 
#        dpi = 300, height = 7, width = 7)


