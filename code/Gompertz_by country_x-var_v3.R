# Load necessary libraries
library(dplyr)
library(minpack.lm)
library(tidyr)
library(purrr)
library(ggplot2)
library(imputeTS)

data <- readRDS("data/all_data_wem_espcap_gapfill.rds")


# Filter data for country_id <= 15
data_top <- data %>%
  filter(country_id <= 15)

# Impute missing values with the mean of the respective columns by country

data_top$density_psqkm <- na_interpolation(data_top$density_psqkm, option = "linear")
data_top$Gini <- na_interpolation(data_top$Gini, option = "linear")


data_top <- data_top %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()


data_input <- subset(data_top, data_top$year <= 2024)
data_pred <- subset(data_top, data_top$year > 2024)

# Fit model by country
models <- data_input %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      mod <- nlsLM(
        ES_pcap ~ (alpha + beta1 * density_psqkm) * exp(-b * exp(-(gamma0 + gamma1 * Gini) * Year0)),
        data = .x,
        start = list(alpha = 5000, beta1 = 0.1, b = 1, gamma0 = 0.01,gamma1 = 0.001)
      )
      list(model = mod, data = .x)
    }, error = function(e) NULL)
  }, .keep = TRUE)


model_summaries <- lapply(models, function(x) {
  if (!is.null(x) && !is.null(x$model)) {
    tryCatch({
      summary(x$model)
    }, error = function(e) NULL)
  } else {
    NULL
  }
})

model_summaries


# Add predictions to data_pred
data_pred <- data_pred %>%
  group_by(country_name) %>%
  group_split()

# Apply predictions using the corresponding model
predictions <- map2(models, data_pred, function(model_entry, pred_data) {
  if (!is.null(model_entry) && !is.null(model_entry$model)) {
    mod <- model_entry$model
    tryCatch({
      pred_data$ES_pcap <- predict(mod, newdata = pred_data)
    }, error = function(e) {
      pred_data$ES_pcap_pred <- NA
    })
  } else {
    pred_data$ES_pcap_pred <- NA
  }
  return(pred_data)
})

# Combine all predictions into a single data frame
data_pred1 <- bind_rows(predictions)



# Combine original and predicted data
combined <- data_input %>%
  select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm,Gini ) %>%
  mutate(Source = "Observed") %>%
  bind_rows(
    data_pred1 %>%
      left_join(data_input %>% group_by(country_name) %>%
                  summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
      mutate(
        GDP_PPP_pcap = data_pred1$GDP_PPP_pcap,
        Source = "Predicted"
      ) %>%
      select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm, Gini, Source)
  )



# Extract the last point for each country in the Observed data
line_labels <- combined %>%
  filter(Source == "Predicted") %>%
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
 
