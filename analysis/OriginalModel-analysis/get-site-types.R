##For Java model

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

site.types = data.frame(
  overlap = numeric(),
  total.groups = numeric(),
  ed.freq = numeric(),
  ed = numeric(), 
  sitecode = numeric()
)

for(run.num in 1:50) {
  setwd(paste0("/Users/emilycoco/eclipse-workspace/recycling-Java/output/run", run.num))
  
  files = list.files()
  
  for(f in files) {
    
    site = read.csv(f)
    site = site[order(site$year),]
    assemblages = site %>% filter(tools != 0)
    
    if(nrow(assemblages) != 0) {
      assemblages$code = NA
      
      for(a in 1:nrow(assemblages)) {
        #determine composition of each assemblage
        # 1 = only type 1 tools
        # 2 = only type 2 tools
        # 3 = mix of type 1 and 2 tools but no mixed typology tools
        # 4 = presence of only 12 tools
        # 5 = presence of 12 and/or 21 tools
        
        if(with(assemblages[a,], sum(retouch.12, retouch.21)) != 0) {
          if(assemblages$retouch.21[a] != 0) {
            assemblages$code[a] = 5
          } else {assemblages$code[a] = 4}
        }else {
          if(assemblages$retouch.11[a] != 0 & assemblages$retouch.22[a] == 0) {
            assemblages$code[a] = 1
          }else if(assemblages$retouch.22[a] != 0 & assemblages$retouch.11[a] == 0) {
            assemblages$code[a] = 2
          }else {
            assemblages$code[a] = 3
          }
        }
      }
      sitetype = str_c(as.character(assemblages$code), collapse = "")

      site.types[nrow(site.types) + 1, ] = c(assemblages$overlap[1],
                                             assemblages$total.groups[1],
                                             assemblages$ed.freq[1],
                                             assemblages$ed[1],
                                             sitetype)
    }
  }
}

save(site.types, file = "/Users/emilycoco/eclipse-workspace/recycling-Java/analysis/site-types.RData")

