library(rmarkdown)
library(tidyverse)
library(data.table)
library(knitr)

alldata = read_csv("../output/joined_model_data.csv")
rmarkdown::render("experiments-with-recycling.Rmd", output_file = "missing-recycling.html")