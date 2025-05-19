# Load necessary libraries
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)

data <- readRDS("data/all_data_wem.rds")

# Define the Gompertz function with additional variables
# gompertz <- function(t, a, b, c, d, e, pop_density, gini_index) {
#   a * exp(-b * exp(-c * t)) + d * pop_density + e * gini_index
# }

gompertz <- function(t, a, b, c, pop_density, gini_index) {
  a * exp(-b * exp(-c * t * pop_density)) ^ gini_index
}


# Filter data for country_id <= 15
data <- data %>%
  filter(country_id <= 15)


# Impute missing values with the mean of the respective columns by country
data <- data %>%
  group_by(country_name) %>%
  mutate(
    pop_density = ifelse(is.na(density_psqkm), mean(density_psqkm, na.rm = TRUE), density_psqkm),
    gini_index = ifelse(is.na(Gini), mean(Gini, na.rm = TRUE), Gini)
  ) %>%
  ungroup()



data <- data %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()

# Fit model by country
models <- data %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      mod <- nlsLM(ES_pcap ~ a * exp(-b * exp(-c * t * pop_density)) ^ gini_index,
                   data = .x,
                   start = list(a=5000, b=1, c=0.1))
      list(model = mod, data = .x)
    }, error = function(e) NULL)
  }, .keep = TRUE)


# Predict next 20 years
predictions <- map_dfr(models, function(entry) {
  if(is.null(entry)) return(NULL)
  model <- entry$model
  data <- entry$data
  country <- unique(data$country_name)
  last_year <- max(data$year)
  new_years <- (last_year + 1):(last_year + 20)
  new_data <- tibble(
    year = new_years,
    Year0 = new_years - min(data$year),
    country_name = country,
    pop_density = mean(data$pop_density) * (1 + 0.02)^(new_years - last_year), # Assuming a 2% annual increase
    gini_index = mean(data$gini_index) * (1 + 0.01)^(new_years - last_year) # Assuming a 1% annual increase
  )
  new_data$ES_pcap <- predict(model, newdata = new_data)
  new_data
})


# Combine original and predicted data
combined <- data %>%
  select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm, Gini) %>%
  mutate(Source = "Observed") %>%
  bind_rows(
    predictions %>%
      left_join(data %>% group_by(country_name) %>%
                  summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
      mutate(
        GDP_PPP_pcap = last_GDP * (1 + 0.03)^(year - max(data$year)),
        Source = "Predicted"
      ) %>%
      select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm, Gini, Source)
  )


# Plot historical and predicted data
ggplot(combined, aes(x = GDP_PPP_pcap, y = energy_service, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  labs(title = "Energy Service Over Time",
       x = "GDP per Capita",
       y = "Energy Service",
       color = "Country") +
  theme_minimal()

# Plot historical and predicted data Es vs Year
ggplot(combined, aes(x = year, y = energy_service, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  labs(title = "Energy Service Over Time",
       x = "Year",
       y = "Energy Service",
       color = "Country") +
  theme_minimal()
