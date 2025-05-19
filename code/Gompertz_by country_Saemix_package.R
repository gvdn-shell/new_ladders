#install.packages("saemix")
library(saemix)
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)

gompertz <- function(t, a, b, c) {
  a * exp(-b * exp(-c * t))
}

data <- readRDS("data/all_data_wem.rds")

data <- data %>%
  filter(country_id <= 15) %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()

# Fit the model using saemixModel with regularization
saemix.model <- saemixModel(
  model = function(psi, id, xidep) {
    t <- xidep[, 1]
    a <- psi[id, 1]
    b <- psi[id, 2]
    c <- psi[id, 3]
    ypred <- a * exp(-b * exp(-c * t))
    return(ypred)
  },
  description = "Gompertz model",
  psi0 = matrix(c(500, 1, 0.1), ncol = 3, byrow = TRUE),
  transform.par = c(0, 0, 0),
  name.modpar = c("a", "b", "c")  # Specify the names of the model parameters
)

# Prepare the data for saemix
saemix.data <- saemixData(
  name.data = data,
  name.group = "country_name",
  name.predictors = "Year0",
  name.response = "energy_service"
)


# Set control options
saemix.control <- saemixControl(seed = 12345)

# Fit the model
saemix.fit <- saemix(saemix.model, saemix.data, saemix.control)



predictions <- map_dfr(unique(data$country_name), function(country) {
  country_data <- data %>% filter(country_name == country)
  last_year <- max(country_data$year)
  new_years <- (last_year + 1):(last_year + 20)
  new_data <- tibble(
    year = new_years,
    Year0 = new_years - min(country_data$year),
    country_name = country
  )
  new_data$energy_service <- predict(saemix.fit, newdata = new_data)
  new_data
})

combined <- data %>%
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
      select(country_name, year, energy_service, GDP_PPP_pcap, Source)
  )

ggplot(combined, aes(x = GDP_PPP_pcap, y = energy_service, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  labs(title = "Energy Service Over Time",
       x = "GDP per Capita",
       y = "Energy Service",
       color = "Country") +
  theme_minimal()

ggplot(combined, aes(x = year, y = energy_service, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  labs(title = "Energy Service Over Time",
       x = "Year",
       y = "Energy Service",
       color = "Country") +
  theme_minimal()
