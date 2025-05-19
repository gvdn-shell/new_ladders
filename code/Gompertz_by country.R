# Load necessary libraries
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)

data <- readRDS("data/all_data_wem.rds")

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
      mod <- nlsLM(ES_pcap ~ a * exp(-b * exp(-c * Year0)),
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
  new_data$ES_pcap <- predict(model, newdata = new_data)
  new_data
})

# combine original and predicted data

# combined<- data %>%
#   dplyr::select(country_name, year, ES_pcap, GDP_PPP_pcap) %>%
#   mutate(Source = "Observed") %>%
#   bind_rows( 
#     predictions %>%
#       left_join(data %>% group_by(country_name) %>%
#                   summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
#       mutate(
#         GDP_PPP_pcap = last_GDP * (1 + 0.03)^(year - max(data$year)),
#         Source = "Predicted"
#       ) %>%
#       select(country_name, year, ES_pcap, GDP_PPP_pcap, Source)
#                 )


step1 <- data %>%
  dplyr::select(country_name, year, ES_pcap, GDP_PPP_pcap)

step2 <- predictions %>%
  left_join(data %>% group_by(country_name) %>%
              summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
  mutate(
    GDP_PPP_pcap = last_GDP * (1 + 0.03)^(year - max(data$year)),
    Source = "Predicted"
  ) %>%
  dplyr::select(country_name, year, ES_pcap, GDP_PPP_pcap, Source)

combined <- step1 %>%
  mutate(Source = "Observed") %>%
  bind_rows(step2)


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

