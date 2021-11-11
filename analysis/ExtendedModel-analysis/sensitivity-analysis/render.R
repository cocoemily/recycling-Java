library(rmarkdown)
library(tidyverse)
library(data.table)
library(knitr)

parameters = c("max_use_intensity", "max_artifact_carry", "max_flake_size", 
               "max_nodules_size", "blank_prob", "scavenge_prob", "overlap", 
               "mu", "size_preference", "flake_preference", 
               "min_suitable_flake_size", "min_suitable_nodule_size", "strict_selection")

alldata = read_csv("../output/joined_sensitivity-data.csv")
#rmarkdown::render("parameter-interaction.Rmd", output_file = "sensitivity-analysis-interactions.html")
rmarkdown::render("sensitivity-regressions.Rmd", output_file = "sensitivity-regressions.html")

