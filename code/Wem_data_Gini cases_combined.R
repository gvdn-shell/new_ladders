
# WEM data latest

data_wem <- readRDS("data/all_data_wem_espcap_imputation_wem_urban.rds")
View(data_wem)

data_Gini <- read.csv("data/Gini_fitted_data.csv")
View(data_Gini)

library(dplyr)


# Append only unique columns from Gini to data wem based on common keys

data <- left_join(
    data_wem,
    data_Gini %>% select(any_of(c("year", "country_name")), 
                     setdiff(names(data_Gini), names(data_wem))),
    by = c("year", "country_name")
  )
View(data)

saveRDS(data, file = "all_data_wem_espcap_imputation_wem_urban_Gini.rds")
