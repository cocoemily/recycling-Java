library(rmarkdown)
library(tidyverse)
library(data.table)
library(knitr)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size", 
               "max_nodules_size", "blank_prob", "scavenge_prob", "overlap", 
               "mu", "size_preference", "flake_preference", 
               "minx_suitable_flake_size", "strict_selection")

setwd("..")
alldata = read_csv("output/joined_sensitivity-data.csv")
setwd("analysis")
rmarkdown::render("parameter-interaction.Rmd", output_file = "sensitivity-analysis.html")
#knitr::knit2html("parameter-interaction.Rmd", output = "sensitivity-analysis.html")

#alldata = as.data.frame(fread("output/joined_model_data.csv"))
#setwd("analysis")
#rmarkdown::render("experiments-with-recycling.Rmd", output_file = "missing-recycling.html")
