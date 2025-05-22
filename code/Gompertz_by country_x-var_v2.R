# Load necessary libraries
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)

data <- readRDS("data/all_data_wem.rds")

# 
# gompertz <- function(t, a, b, c, pop_density, gini_index) {
#   a * exp(-b * exp(-c * t * pop_density)) ^ gini_index
# }



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
      mod <- nlsLM(
        ES_pcap ~ (alpha + beta1 * pop_density) * exp(-b * exp(-(gamma * gini_index) * Year0)),
        data = .x,
        start = list(alpha = 5000, beta1 = 0.1, b = 1, gamma = 0.01)
      )
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
    gini_index = mean(data$gini_index) * (1 - 0.01)^(new_years - last_year) # Assuming a 1% annual increase
  )
  new_data$ES_pcap <- predict(model, newdata = new_data)
  new_data
})


# Combine original and predicted data
combined <- data %>%
  select(country_name, year, ES_pcap, GDP_PPP_pcap, pop_density,gini_index ) %>%
  mutate(Source = "Observed") %>%
  bind_rows(
    predictions %>%
      left_join(data %>% group_by(country_name) %>%
                  summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
      mutate(
        GDP_PPP_pcap = last_GDP * (1 + 0.03)^(year - max(data$year)),
        Source = "Predicted"
      ) %>%
      select(country_name, year, ES_pcap, GDP_PPP_pcap, pop_density, gini_index, Source)
  )



# Extract the last point for each country in the Observed data
line_labels <- combined %>%
  filter(Source == "Observed") %>%
  group_by(country_name) %>%
  filter(GDP_PPP_pcap == max(GDP_PPP_pcap)) %>%
  ungroup()


# Plot historical and predicted data
ggplot(combined, aes(x = GDP_PPP_pcap, y = ES_pcap, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  geom_text(data = line_labels, aes(label = country_name), hjust = -0.1, size = 3, show.legend = FALSE) +
  labs(title = "Energy Service per Capita vs GDP per Capita",
       x = "GDP per Capita",
       y = "Energy Service per Capita",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "none")
# scale_y_continuous(transform = "sqrt") +
# scale_x_continuous(transform = "sqrt")
# scale_color_brewer(palette = "Set1") # Optional: use a colorblind-friendly palette

# Plot historical and predicted data Es vs Year
ggplot(combined, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  geom_text(data = line_labels, aes(label = country_name), hjust = -0.1, size = 3, show.legend = FALSE) +
  labs(title = "Energy Service per Capita Over Time",
       x = "Year",
       y = "Energy Service per Capita",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "none")

