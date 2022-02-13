library(rmarkdown)
library(tidyverse)
library(data.table)
library(readr)
library(knitr)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
#rmarkdown::render("preliminary-data-analysis.Rmd", output_file = "prelim-data-analysis.html")

rmarkdown::render("where-why-recycling.Rmd", output_file = "recycling-correlations-at-mid-end.html")
