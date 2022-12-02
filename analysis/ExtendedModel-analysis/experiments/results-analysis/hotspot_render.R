library(rmarkdown)
library(tidyverse)
library(data.table)
library(readr)
library(knitr)


rmarkdown::render("hotspot-graphs.Rmd", output_file = "hotspots-by-experiment.html")

