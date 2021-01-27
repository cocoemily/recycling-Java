##From original model

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0 ) {
  stop("Need arguments", call. = FALSE)
}

run.num <- paste0("run_", args[1])

#set working directory to where results from model runs were saved
#setwd(paste0("/scratch/ec3307/trans_industries/results/", run.num))
#setwd(paste0("~/Desktop/NYU/SYP-Models/transitional_industries/results/", run.num))
setwd(paste0("results/", run.num))

files <- list.dirs(full.names = F)
files = files[-1]

#set up databases
site.types <- data.frame(run = integer(),
                         experiment = integer(), 
                         source = integer(),
                         groups = integer(),
                         geofreq = integer(),
                         overlap = integer(), 
                         ed.prop = integer(),
                         radius = integer(),
                         sitecode = integer())
composition = data.frame(blanks1 = integer(), 
                         blanks2 = integer(), 
                         retouch.11 = integer(), 
                         retouch.12 = integer(), 
                         retouch.21 = integer(), 
                         retouch.22 = integer(),
                         code = integer(), 
                         experiment = integer())

for(f in 1:length(files)) {
  numfiles = length(list.files(path = files[f]))
  for(s in 1:numfiles) {
    source <- read.csv(paste0(files[f], "/source_", s, ".csv"))
    
    #determining visible sequence
    #order years to create timeline to follow
    timeline <- source$year
    timeline <- sort(timeline, decreasing = TRUE)
    timeline <- unique(timeline)
    visible <- source
    #find and remove chunks bounded by years 
    for(year in timeline) {
      ind1 <- NULL
      ind2 <- NULL
      for(i in 1:length(visible$year)) {
        if(year == visible$year[i]) {
          ind1 <- i
          break
        }
      }
      for(i in length(visible$year):1) {
        if(year == visible$year[i]) {
          ind2 <- i
          break
        }
      }
      if(!is.null(ind1) && !is.null(ind2)) {
        if(ind1 != ind2) {
          visible <- visible[-((ind1+1):(ind2)),]
        }
      }
    }
    visible[,7:10] <- sapply(visible[,7:10], as.numeric)
    
    #select layers with assemblages (those that have tools)
    assemblages = visible %>% filter(tools != 0)
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
    
    #add information to databases
    site.types[nrow(site.types) + 1, ] <- c(args[1], files[f], s, source$groups[s], 
                                            source$ed.freq[s], source$overlap[s], 
                                            source$ed.prop[s], source$radius[s],
                                            sitetype)
    addrow = assemblages %>% dplyr::select(blanks1, blanks2, retouch.11, retouch.12, 
                                    retouch.21, retouch.22, code)
    addrow$experiment = files[f]
    composition = rbind(composition, addrow)
    }
  }
}

#write databases to CSV files
write.csv(site.types, file.path(getwd(), "site_types2.csv"))
write.csv(composition, file.path(getwd(), "composition.csv"))
