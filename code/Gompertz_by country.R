# Load necessary libraries
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)

data <- readRDS("all_data_wem.rds")

# Define the Gompertz function
gompertz <- function(t, a, b, c) {
  a * exp(-b * exp(-c * t))
}

# Filter data for country_id <= 10
data <- data %>%
  filter(country_id <= 15)

data <- data %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()

# Fit model by country

models <- data %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      mod <- nlsLM(energy_service ~ a * exp(-b * exp(-c * Year0)),
                   data = .x,
                   start = list(a=500,b=1,c=0.1))
      list(model = mod, data = .x)
      
    } , error = function(e) NULL)
  }, .keep = TRUE)

# Predict next 10 years

predictions <- map_dfr(models, function(entry) {
  if(is.null(entry)) return(NULL)
  model <- entry$model
  data <- entry$data
  country<- unique(data$country_name)
  last_year <- max(data$year)
  new_years <- (last_year +1):(last_year + 20)
  new_data<- tibble(
    year = new_years,
    Year0 = new_years - min(data$year),
    country_name = country
  )
  new_data$energy_service <- predict(model, newdata = new_data)
  new_data
})

# combine original and predicted data

combined<- data %>%
  select(country_name, year, energy_service, GDP_PPP_pcap) %>%
  mutate(Source = "Observed") %>%
  bind_rows( 
    predictions %>%
      left_join(data %>% group_by(country_name) %>%
                  summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
      mutate(
        GDP_PPP_pcap = last_GDP * (1 + 0.03)^(year - max(data$year)),
        Source = "Predicted"
      ) %>%
      select(country_name, year,energy_service, GDP_PPP_pcap, Source)
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

