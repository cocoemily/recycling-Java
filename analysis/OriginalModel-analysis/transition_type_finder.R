library(tidyr)
library(dplyr)
library(ggplot2)

args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0 ) {
  stop("Need arguments", call. = FALSE)
}

run.num <- paste0("run_", args[1])

#set working directory to where results from model runs were saved
#setwd(paste0("/scratch/ec3307/trans_industries/results/", run.num))
setwd(paste0("~/Desktop/NYU/SYP-Models/transitional_industries/acc_results/", run.num))

files <- list.files()

#set up databases
site.types <- data.frame(run = integer(),
                         experiment = integer(), 
                         source = integer(), 
                         mixed.tech = integer(), 
                         preced = integer(), 
                         subsq = integer())
transitional.composition <- data.frame(experiment = integer(), 
                                       source = integer(), 
                                       time.depth = integer(), 
                                       type.11 = integer(), 
                                       type.12 = integer(), 
                                       type.21 = integer(), 
                                       type.22 = integer())

for(f in 1:length(files)) {
  for(s in 1:50) {
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
    
    #determine composition of various components
    #mixed typology
      # 0 = no tools
      # 1 = only 12 or 22 tools
      # 2 = mix of 12 and/or 21 tools and 11 and/or 22 tools
    #preceding and subsequent layers
      # 0 = no tools
      # 1 = only type 1 tools
      # 2 = only type 2 tools
      # 3 = mix of type 1 and 2 tools 
    mt <- NULL
    p <- NULL
    sb <- NULL
    
    #get mixed typology layers
    mixed.tech <- visible[visible$mixed != 0, ] 
    if(nrow(mixed.tech) != 0) {
      preced <- visible[visible$X > mixed.tech$X[nrow(mixed.tech)], ]
      subsq <- visible[visible$X < mixed.tech$X[1], ]
      #mixed typology layers
      mt12 <- sum(mixed.tech$retouch.12)
      mt21 <- sum(mixed.tech$retouch.21)
      mt11 <- sum(mixed.tech$retouch.11)
      mt22 <- sum(mixed.tech$retouch.22)
      #preceding layers
      p11 <- sum(preced$retouch.11)
      p22 <- sum(preced$retouch.22)
      #subsequent layers
      s11 <- sum(subsq$retouch.11)
      s22 <- sum(subsq$retouch.22)
      
      #assign codes based on presence/absence of tool types
      if(mt11 == 0 & mt22 == 0) {
        mt <- 1
      } else {
        mt <- 2
      }
      if(nrow(preced) == 0) {
        p <- 0
      } else {
        if(p11 != 0 & p22 == 0) {
          p <- 1
        } else if(p11 == 0 & p22 != 0) {
          p <- 2
        } else {
          p <- 3
        }
      }
      if(nrow(subsq) == 0) {
        sb <- 0
      } else {
        if(s11 != 0 & s22 == 0) {
          sb <- 1
        } else if(s11 == 0 & s22 != 0) {
          sb <- 2
        } else {
          sb <- 3
        }
      }
    } else {
      mt <- 0
      p <- NA
      sb <- NA
    }
    
    #add information to database  
    site.types[nrow(site.types) + 1, ] <- c(args[1], files[f], s, mt, p, sb)
    
    #composition and time depth of transitional layers
    if(nrow(mixed.tech) != 0) {
      td <- mixed.tech$year[nrow(mixed.tech)] - mixed.tech$year[1]
      t11 <- sum(mixed.tech$retouch.11)
      t12 <- sum(mixed.tech$retouch.12)
      t21 <- sum(mixed.tech$retouch.21)
      t22 <- sum(mixed.tech$retouch.22)
      transitional.composition[nrow(transitional.composition) + 1,] <-
        c(f, s, td, t11, t12, t21, t22)
    }
  }
}

#write databases to CSV files
write.csv(site.types, file.path(getwd(), "site_types.csv"))
write.csv(transitional.composition, file.path(getwd(), "transitional_comp.csv"))



