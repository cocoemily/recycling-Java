library(rmarkdown)
library(tidyverse)

setwd("..")
#alldata = read_csv(file = "output/joined_data-small.csv")
#setwd("analysis")
#rmarkdown::render("parameter-interaction.Rmd", output_file = "small-size_parameter-interaction.html")

#alldata = read_csv(file = "output/joined_data-small.csv")
#alldata = alldata %>% filter(model_year < 394000)
#alldata = alldata %>% filter(!is.na(total.RI))
#setwd("analysis")
#rmarkdown::render("parameter-interaction.Rmd", output_file = "small-size_minus-initial-time.html")


alldata = read_csv(file = "output/joined_model_data.csv")
setwd("analysis")
rmarkdown::render("experiments-with-recycling.Rmd", output_file = "small-size_parameter-interaction.html")