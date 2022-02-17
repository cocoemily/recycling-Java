library(tidyverse)
library(readr)

alldata = readr::read_csv("/scratch/ec3307/recycling-Java/output/joined_model_data.csv")

facet_data = alldata[,c(parameters, outputs)]

missing = facet_data %>% group_by_at(parameters) %>% miss_var_summary %>%
  filter(n_miss != 0)

save(missing, file = "missing-values-by-experiment.csv")