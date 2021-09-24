library(rmarkdown)
library(tidyverse)
library(data.table)

setwd("..")
#alldata = as.data.frame(fread("output/joined_data-small.csv"))
#setwd("analysis")
#rmarkdown::render("parameter-interaction.Rmd", output_file = "small-size_parameter-interaction.html")


alldata = as.data.frame(fread("output/joined_model_data.csv"))
setwd("analysis")
rmarkdown::render("experiments-with-recycling.Rmd", output_file = 
"missing-recycling.html")
