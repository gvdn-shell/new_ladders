# Load necessary libraries
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)
library(nlme)

data <- readRDS("data/all_data_wem.rds")

# Define the Gompertz function
gompertz <-   function(Year0, a, b, c) {
    a * exp(-b * exp(-c * Year0))
  }


# Filter data for country_id <= 10
data <- data %>%
  filter(country_id <= 15)

data <- data %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()


# Fit the nonlinear mixed-effects model

mixed_model <- nlme(
  ES_pcap ~ gompertz(Year0, a, b, c),
  data = data,
  fixed = a + b + c ~ 1,
  random = a + b  ~ 1 | country_name, # Only 'a' varies by country
  start = c(a = 500, b = 1.2, c = 0.1),
  control = nlmeControl(maxIter = 50, pnlsTol = 1e-6)
)



# Predict next 10 years

future_data <- data %>%
  group_by(country_name) %>%
  summarise(max_year = max(Year0)) %>%
  rowwise() %>%
  do({
    country <- .$country_name
    max_year <- .$max_year
    data.frame(
      country_name = country,
      Year0 = seq(max_year + 1, max_year + 20)
    )
  }) %>%
  ungroup()


future_data$ES_pcap_pred <- predict(mixed_model, newdata = future_data, level = 0) # level = 0 for population-level prediction



# data_labeled <- data %>%
#   mutate(source = "observed",
#          ES_pcap_value = ES_pcap,
#          Year = Year0 + 1960)
# 
# 
# 
# future_labeled <- future_data %>%
#   mutate(source = "predicted",
#          ES_pcap_value = ES_pcap_pred,
#          Year = Year0 + 1960)
# 
# 
# 
# combined <- bind_rows(data_labeled, future_labeled)


# combine original and predicted data

combined<- data_labeled %>%
  dplyr::select(country_name, Year, ES_pcap_value, GDP_PPP_pcap) %>%
  mutate(Source = "Observed") %>%
  bind_rows(
    future_labeled %>%
      left_join(data_labeled %>% group_by(country_name) %>%
                  summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
      mutate(
        GDP_PPP_pcap = last_GDP * (1 + 0.03)^(Year - max(data_labeled$Year)),
        Source = "Predicted"
      ) %>%
      select(country_name, Year, ES_pcap_value, GDP_PPP_pcap, Source)
                )


# Extract the last point for each country in the Observed data
line_labels <- combined %>%
  filter(Source == "Predicted") %>%
  group_by(country_name) %>%
  filter(GDP_PPP_pcap == max(GDP_PPP_pcap)) %>%
  ungroup()


# Plot historical and predicted data
ggplot(combined, aes(x = GDP_PPP_pcap, y = ES_pcap_value, color = country_name)) +
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
ggplot(combined, aes(x = Year, y = ES_pcap_value, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  geom_text(data = line_labels, aes(label = country_name), hjust = -0.1, size = 3, show.legend = FALSE) +
  labs(title = "Energy Service per Capita Over Time",
       x = "Year",
       y = "Energy Service per Capita",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "none")

