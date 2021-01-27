library(tidyr)
library(dplyr)
library(ggplot2)

setwd("/Users/emilycoco/eclipse-workspace/recycling-Java/analysis")
load("site-types.RData")
st = site.types

#calculation of important variables
st$length = nchar(st$sitecode)
st$A = str_detect(st$sitecode, pattern = "1")
st$B = str_detect(st$sitecode, pattern = "2")
st$AB = str_detect(st$sitecode, pattern = "3")
st$M1 = str_detect(st$sitecode, pattern = "4")
st$M2 = str_detect(st$sitecode, pattern = "5")
st$fulltrans = str_detect(st$sitecode, pattern = "2+4+1+")
st$trans.UP = str_detect(st$sitecode, pattern = "2+4+")
st$MP.trans = str_detect(st$sitecode, pattern = "4+1+")
st$notrans = !st$M1 & !st$M2
st$allmix = !str_detect(st$sitecode, pattern = "1") &
  !str_detect(st$sitecode, pattern = "2") & 
  !str_detect(st$sitecode, pattern = "3")
st$trans.MP = str_detect(st$sitecode, pattern = "1+4+") |
  str_detect(st$sitecode, pattern = "1+5+") #this is important
st$UP.trans = str_detect(st$sitecode, pattern = "4+2+") |
  str_detect(st$sitecode, pattern = "5+2+")
st$back.trans = str_detect(st$sitecode, pattern = "1+4+2+")

randomED = st %>% filter(ed == 0.5)
EDdif = st %>% filter(ed == 0.25 | ed == 0.75)

counts = randomED %>% group_by(overlap, total.groups, ed.freq) %>%
  summarize(A = sum(A, na.rm = TRUE)/n(),
            B = sum(B, na.rm = TRUE)/n(),
            AB = sum(AB, na.rm = TRUE)/n(),
            M1 = sum(M1, na.rm = TRUE)/n(), 
            M2 = sum(M2, na.rm = TRUE)/n(), 
            ft = sum(fulltrans, na.rm = TRUE)/n(), 
            tU = sum(trans.UP, na.rm = TRUE)/n(), 
            Mt = sum(MP.trans, na.rm = TRUE)/n(), 
            nt = sum(notrans, na.rm = TRUE)/n(), 
            allmix = sum(allmix, na.rm = TRUE)/n(), 
            tM = sum(trans.MP, na.rm = TRUE)/n(), 
            Ut = sum(UP.trans, na.rm = TRUE)/n())

atypes = counts %>% select(overlap, total.groups, ed.freq, A, B, AB, M1, M2) %>% 
  pivot_longer(c(A, B, AB, M1, M2), names_to = "type", values_to = "proportion")

ggplot(atypes, aes(x = as.factor(type), y = proportion)) +
  geom_point()
