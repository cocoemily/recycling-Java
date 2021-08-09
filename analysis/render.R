library(rmarkdown)
library(tidyverse)

alldata = read_csv(file = "output/joined_data-small.csv")
rmarkdown::render("parameter-interaction.Rmd", output_file = "small-size_parameter-interaction.html")


alldata = read_csv(file = "output/joined_data-large.csv")
rmarkdown::render("parameter-interaction.Rmd", output_file = "large-size_parameter-interaction.html")