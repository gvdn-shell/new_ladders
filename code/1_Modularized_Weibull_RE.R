# Setup Function: Load Packages and Fonts
setup_environment <- function(packages, font_path = "ShellMedium.ttf") {
  # Install missing packages
  missing_packages <- packages[!(packages %in% rownames(installed.packages()))]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE, Ncpus = 12)
  }
  
  # Load all required packages
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Load fonts for plotting
  extrafont::loadfonts(device = "win")
  font_add("ShellMedium", font_path)
  showtext_auto()  # Automatically use showtext for new devices
}

# Function to load and preprocess data
load_and_preprocess_data <- function(file_path) {
  data <- readRDS(file_path) %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    mutate(
      lag_ES_pcap = lag(ES_pcap, order_by = year),
      Gini_00 = Gini / 100,
      Gini_01 = gini_case1 / 100,
      Gini_02 = gini_case2 / 100,
      Gini_03 = gini_case3 / 100,
      GDP_PPP_pcap_thousands = GDP_PPP_pcap / 1000
    ) %>%
    ungroup()
  
  return(data)
}

# Function to merge Shell brand palette
merge_brand_palette <- function(data, palette_path) {
  shell_brand_palette <- readRDS(palette_path) %>%
    mutate(Hex = paste0("#", Hex), country_id = row_number()) %>%
    select(country_id, Hex) %>%
    distinct()
  
  merged_data <- merge(shell_brand_palette, data, by = "country_id")
  return(merged_data)
}

# Function for data filtering
filter_data <- function(data) {
  set.seed(1234)
  
  data_filtered <- data %>%
    group_by(country_name) %>%
    filter(
      sum(!is.na(Gini_01)) >= 2,
      sum(!is.na(urbanization_perc)) >= 2,
      sum(!is.na(density_psqkm)) >= 2
    ) %>%
    ungroup() %>%
    mutate(Gini_noise = Gini + rnorm(1, mean = 0, sd = 0.01))
  
  return(data_filtered)
}

# Create Custom Plot Theme
create_plot_theme <- function(text_size = 14) {
  theme(
    axis.title.y = element_text(size = text_size, family = "ShellMedium, sans"),
    axis.title.x = element_text(size = text_size, family = "ShellMedium, sans"),
    axis.text.y = element_text(size = text_size - 2, family = "ShellMedium, sans"),
    axis.text.x = element_text(size = text_size - 2, family = "ShellMedium, sans", angle = 45, hjust = 1),
    legend.position = c(1.05, 0.9),
    legend.text = element_text(size = text_size - 2, family = "ShellMedium, sans"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = text_size, family = "ShellMedium, sans", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = text_size - 4, family = "ShellMedium, sans"),
    plot.caption = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    panel.border = element_rect(color = "black", fill = NA),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
}

# Function to plot GDP vs Energy Service per Capita
plot_gdp_vs_espcap <- function(data, theme_function) {
  ggplotly(
    ggplot(data = data, aes(x = GDP_PPP_pcap, y = ES_pcap, colour = Hex)) +
      geom_line() +
      geom_point(aes(shape = country_name), size = 1.5, alpha = 0.7) +
      geom_text(
        data = data %>% filter(year == max(year) - 5),
        aes(label = country_name),
        hjust = -0.1, size = 2
      ) +
      labs(
        title = "GDP per Capita vs Energy Service per Capita",
        x = "GDP per Capita",
        y = "Energy Service per Capita"
      ) +
      theme_minimal() +
      theme_function(1) +
      scale_color_identity() +
      scale_x_continuous(labels = scales::comma, trans = "log") +
      theme(legend.position = "none")
  )
}

# Function for Forecasting
forecast_espcap <- function(data, model, known_ids, start_year = 2024, end_year = 2100) {
  data.orig <- data
  
  for (i in start_year:end_year) {
    data.orig <- data.orig %>%
      group_by(country_id) %>%
      arrange(year) %>%
      mutate(lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)) %>%
      ungroup()
    
    data.used <- filter(data.orig, year == i)
    
    # Split data into known and new countries
    data.known <- data.used %>% filter(country_id %in% known_ids)
    data.new <- data.used %>% filter(!country_id %in% known_ids)
    
    pred.known <- if (nrow(data.known) > 0) {
      predict(model, newdata = data.known, level = 1)
    } else {
      numeric(0)
    }
    
    pred.new <- if (nrow(data.new) > 0) {
      predict(model, newdata = data.new, level = 0)
    } else {
      numeric(0)
    }
    
    prediction_df <- bind_rows(
      data.frame(country_id = data.known$country_id, year = data.known$year, predicted_ES_pcap = pred.known),
      data.frame(country_id = data.new$country_id, year = data.new$year, predicted_ES_pcap = pred.new)
    )
    
    # Update ES_pcap with predicted values
    data.orig <- data.orig %>%
      left_join(prediction_df, by = c("country_id", "year")) %>%
      group_by(country_id) %>%
      arrange(country_id, year) %>%
      mutate(
        ES_pcap = if_else(!is.na(predicted_ES_pcap), predicted_ES_pcap, ES_pcap),
        lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)
      ) %>%
      select(-predicted_ES_pcap)
  }
  
  return(data.orig)
}

# Function to define Weibull Model 1
weibull_model_1 <- function() {
  deriv(
    ~ (alpha_0 + alpha_1 * density_psqkm) * (k * Gini) / lambda *
      (GDP_PPP_pcap / lambda)^(k * Gini - 1) *
      exp(-(GDP_PPP_pcap / lambda)^(k * Gini)),
    namevec = c("alpha_0", "alpha_1", "k", "lambda"),
    function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", "alpha_0", "alpha_1", "k", "lambda")
  )
}

# Function to define Weibull Model 2
weibull_model_2 <- function() {
  deriv(
    ~ (alpha_0 + alpha_1 * density_psqkm) *
      k / exp(gamma + lambda * Gini) *
      (GDP_PPP_pcap / exp(gamma + lambda * Gini))^(k - 1) *
      exp(-(GDP_PPP_pcap / exp(gamma + lambda * Gini))^k),
    namevec = c("alpha_0", "alpha_1", "k", "lambda", "gamma"),
    function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", "alpha_0", "alpha_1", "k", "lambda", "gamma")
  )
}

# Function to define Weibull Model 3
weibull_model_3 <- function() {
  deriv(
    ~ (alpha_0 + alpha_1 * density_psqkm) *
      exp(gamma + k * Gini) / lambda * (GDP_PPP_pcap / lambda)^(exp(gamma + k * Gini) - 1) *
      exp(- (GDP_PPP_pcap / lambda)^(exp(gamma + k * Gini))),
    namevec = c("alpha_0", "alpha_1", "k", "lambda", "gamma"),
    function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", "lag_ES_pcap", "alpha_0", "alpha_1", "k", "lambda", "gamma")
  )
}

# Function to define Weibull Model 4 (with lagged ES_pcap)
weibull_model_4 <- function() {
  deriv(
    ~ (alpha_0 + alpha_1 * density_psqkm + alpha_2 * lag_ES_pcap) *
      k / exp(gamma + lambda * Gini) *
      (GDP_PPP_pcap / exp(gamma + lambda * Gini))^(k - 1) *
      exp(-(GDP_PPP_pcap / exp(gamma + lambda * Gini))^k),
    namevec = c("alpha_0", "alpha_1", "alpha_2", "k", "lambda", "gamma"),
    function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", "lag_ES_pcap", "alpha_0", "alpha_1", "alpha_2", "k", "lambda", "gamma")
  )
}

# Function to fit Weibull model using nlsLM
fit_weibull_model <- function(data, model_func, start_vals) {
  fit <- nlsLM(
    ES_pcap ~ model_func(),
    data = data,
    start = start_vals,
    na.action = na.exclude,
    control = nls.lm.control(maxiter = 500)
  )
  
  return(fit)
}

# Function to fit Weibull model with random effects using nlme
fit_weibull_model_nlme <- function(data, model_func, start_vals) {
  fit_nlme <- nlme(
    model = ES_pcap ~ model_func(),
    data = data,
    fixed = alpha_0 + alpha_1 + alpha_2 + k + lambda + gamma ~ 1,
    random = alpha_0 ~ 1 | country_id,  # Random effect on alpha_0
    start = start_vals,
    na.action = na.exclude,
    control = nlmeControl(
      pnlsTol = 0.5,
      maxIter = 500,
      minFactor = 1e-10,
      msMaxIter = 500,
      warnOnly = TRUE
    )
  )
  
  return(fit_nlme)
}

# Function to summarize model output
summarize_model <- function(fit) {
  summary(fit)
}

# Main function to fit and evaluate the models
run_model_fitting <- function(data) {
  # Define starting values
  start_vals_weibull_2 <- c(alpha_0 = 1000, alpha_1 = 0, k = 1, lambda = 1, gamma = 1)
  start_vals_weibull_4 <- c(alpha_0 = 1000, alpha_1 = 0, alpha_2 = 0, k = 1, lambda = 1, gamma = 1)
  
  # Fit Weibull Model 2
  fit_weibull_2 <- fit_weibull_model(data, weibull_model_2, start_vals_weibull_2)
  summarize_model(fit_weibull_2)
  
  # Fit Weibull Model 4
  fit_weibull_4 <- fit_weibull_model(data, weibull_model_4, start_vals_weibull_4)
  summarize_model(fit_weibull_4)
  
  # Fit Weibull Model 4 with random effects
  fit_weibull_4_nlme <- fit_weibull_model_nlme(data, weibull_model_4, start_vals_weibull_4)
  summarize_model(fit_weibull_4_nlme)
  
  return(list(fit_weibull_2 = fit_weibull_2, fit_weibull_4 = fit_weibull_4, fit_weibull_4_nlme = fit_weibull_4_nlme))
}


# Define required packages
required_packages <- c(
  "dplyr", "minpack.lm", "tidyr", "purrr", "ggplot2", "nlme", "broom.mixed",
  "gt", "prophet", "readr", "here", "stargazer", "plotly", "lme4", "car",
  "sysfonts", "extrafont", "showtext", "stringr", "knitr", "kableExtra"
)

# Set up environment
setup_environment(required_packages)

# Load and preprocess data
data <- load_and_preprocess_data("data/all_data_wem_espcap_imputation_wem_urban.rds")

# Merge brand palette
data <- merge_brand_palette(data, "data/shell_brand_palette_extended.rds")

# Filter data
data <- filter_data(data)

# Plot GDP vs Energy Service per Capita
plot_gdp_vs_espcap(data, create_plot_theme)

# Forecast Energy Service per Capita
known_ids <- unique(data$country_id)
forecasted_data <- forecast_espcap(data, fit_weibull_4, known_ids)

# Fit models and summarize
model_fits <- run_model_fitting(data)


