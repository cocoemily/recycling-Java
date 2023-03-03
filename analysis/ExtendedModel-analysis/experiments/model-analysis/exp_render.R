library(rmarkdown)
library(tidyverse)
library(data.table)
library(readr)
library(knitr)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")
#rmarkdown::render("preliminary-data-analysis-two-techs.Rmd", output_file = "prelim-analysis-two-techs.html")
rmarkdown::render("preliminary-data-analysis-many-techs.Rmd", output_file = "prelim-analysis-many-techs.html")

