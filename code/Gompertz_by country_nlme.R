# Load necessary libraries
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)
library(nlme)

data <- readRDS("data/all_data_wem_espcap_imputation.rds")

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

data_input <- subset(data, data$year <= 2023)
data_predict <- subset(data,data$year > 2023)

# Fit the nonlinear mixed-effects model

mixed_model <- nlme(
  ES_pcap ~ gompertz(Year0, a, b, c),
  data = data_input,
  fixed = a + b + c ~ 1,
  random = a + b  ~ 1 | country_name, # Only 'a' varies by country
  start = c(a = 500, b = 1.2, c = 0.1),
  control = nlmeControl(maxIter = 50, pnlsTol = 1e-6)
)

summary(mixed_model)




# Predict next 10 years

# future_data <- data_predict %>%
#   group_by(country_name) %>%
#   summarise(max_year = max(Year0)) %>%
#   rowwise() %>%
#   do({
#     country <- .$country_name
#     max_year <- .$max_year
#     data.frame(
#       country_name = country,
#       Year0 = year
#     )
#   }) %>%
#   ungroup()


data_predict$ES_pcap <- predict(mixed_model, newdata = data_predict, level = 1) # level = 0 for population-level prediction



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

combined<- data_input %>%
  dplyr::select(country_name, year, ES_pcap, GDP_PPP_pcap) %>%
  mutate(Source = "Observed") %>%
  bind_rows(
    data_predict %>%
      left_join(data_input %>% group_by(country_name) %>%
                  summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
      mutate(
            Source = "Predicted"
      ) %>%
      select(country_name, year, ES_pcap, GDP_PPP_pcap, Source)
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

