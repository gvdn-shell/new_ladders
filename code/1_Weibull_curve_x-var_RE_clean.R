# Random Effects: https://stats.stackexchange.com/questions/242759/calculate-random-effect-predictions-manually-for-a-linear-mixed-model

################################################################################
# SETUP: Clean Environment and Load Required Packages
################################################################################

# Remove all objects from the workspace (use with caution!)
rm(list = ls())

# Define number of CPU cores for parallel installation
ncpus <- 12

# List of required packages
packages <- c(
  "dplyr", "minpack.lm", "tidyr", "purrr", "ggplot2", "nlme", "broom.mixed",
  "gt", "prophet", "readr", "here", "stargazer", "plotly", "lme4", "car",
  "sysfonts", "extrafont", "showtext", "stringr", "knitr", "kableExtra", "stargazer"
)

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load all required packages
invisible(lapply(packages, library, character.only = TRUE))

# Load fonts for plotting
extrafont::loadfonts(device = "win")
font_add("ShellMedium", "ShellMedium.ttf")  # Add custom font
showtext_auto()  # Automatically use showtext for new devices

################################################################################
# DATA PREPARATION
################################################################################

# Load and preprocess main dataset
data <- readRDS("data/all_data_wem_espcap_imputation_wem_urban.rds") %>%
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

################################################################################
# BRAND PALETTE MERGE
################################################################################

# Load Shell brand palette from Excel and save as RDS (one-time setup)
# shell.brand.palette <- readxl::read_excel(
#   path = here::here("Data", "Shell Scenarios v14.6 2023_06_23.xlsm"),
#   sheet = "Settings",
#   range = "L10:M270",
#   col_names = TRUE
# ) %>%
#   dplyr::select(-Colour)
# 
# saveRDS(shell.brand.palette, file = here::here("data", "shell_brand_palette_extended.rds"))

# Load Shell brand color palette
shell.brand.palette <- readRDS(here::here("data", "shell_brand_palette_extended.rds")) %>%
  mutate(
    Hex = paste0("#", Hex),
    country_id = row_number()
  ) %>%
  select(country_id, Hex) %>%
  distinct()

# Merge palette with main dataset
data <- merge(shell.brand.palette, data, by = "country_id")

################################################################################
# PLOT: Standardized theme
################################################################################

create_theme <- function(text_size = 14) {
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

################################################################################
# DATA FILTERING AND PREPARATION
################################################################################

# Set seed for reproducibility
set.seed(1234)

# Filter out countries with fewer than 2 non-missing values for key variables
data <- data %>%
  group_by(country_name) %>%
  filter(
    sum(!is.na(Gini_01)) >= 2,
    sum(!is.na(urbanization_perc)) >= 2,
    sum(!is.na(density_psqkm)) >= 2
  ) %>%
  ungroup() %>%
  # Add small random noise to Gini to avoid issues with zero-residuals in nls
  mutate(Gini_noise = Gini + rnorm(1, mean = 0, sd = 0.01))

# Subset historical data (up to and including 2024)
data1 <- data %>%
  filter(year <= 2024)

# Create a relative year variable (Year0) for modelling
data1 <- data1 %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()

################################################################################
# CUSTOM PLOTTING THEME FOR PRESENTATION
################################################################################

create_theme1 <- function(rel.size = 1) {
  theme(
    axis.title.y = element_text(size = 18 * rel.size, family = "ShellMedium"),
    axis.title.x = element_text(size = 18 * rel.size, family = "ShellMedium"),
    axis.text.y = element_text(size = 16 * rel.size, family = "ShellMedium"),
    axis.text.x = element_text(size = 16 * rel.size, family = "ShellMedium", angle = 45, hjust = 1),
    plot.margin = margin(5, 5, 5, 5),
    legend.title = element_text(size = 16 * rel.size, family = "ShellMedium"),
    legend.text = element_text(size = 15 * rel.size, family = "ShellMedium"),
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18 * rel.size, family = "ShellMedium", hjust = 0.5),
    strip.text = element_text(size = 16 * rel.size, family = "ShellMedium"),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    panel.border = element_blank()
  )
}

################################################################################
# PLOT: GDP per Capita vs Energy Service per Capita
################################################################################
ggplotly(
  ggplot(data = data1, aes(x = GDP_PPP_pcap, y = ES_pcap, colour = Hex)) +
    geom_line() +
    geom_point(aes(shape = country_name), size = 1.5, alpha = 0.7) +
    geom_text(
      data = data1 %>% filter(year == max(year) - 5),
      aes(label = country_name),
      hjust = -0.1, size = 2
    ) +
    labs(
      title = "GDP per Capita vs Energy Service per Capita",
      x = "GDP per Capita",
      y = "Energy Service per Capita"
    ) +
    theme_minimal() +
    create_theme1(1) +
    scale_color_identity() +
    scale_x_continuous(labels = scales::comma, trans = "log") +
    theme(legend.position = "none")
)

################################################################################
# EXCLUDE COUNTRIES FROM ESTIMATION
################################################################################

# Define groups of countries to exclude
oil_gas_countries <- c("Iran", "Malaysia", "Algeria", "Syria", "Sudan", "Ecuador", "Yemen", "Angola", "Nigeria", "Venezuela")

former_ussr_and_others <- c(
  "Russia", "Uzbekistan", "Belarus", "Poland", "Romania", "Ukraine", "Bulgaria",
  "Azerbaijan", "Kazakhstan", "Turkmenistan", "North Korea"
)

rest_of_countries <- c(
  "International Marine Bunkers", "Rest of Europe West Other", "Rest of Europe East Other",
  "Rest of EU New 12", "Baltic States", "Rest of Central Asia", "Rest of East Asia",
  "Rest of SE Asia", "Rest of South Asia", "Rest of Middle East", "Rest Of Arabian Peninsula",
  "Rest of North Africa", "Rest of East Africa", "Rest of Southern Africa",
  "Rest of West Africa", "Rest of North America", "Rest of Central America & Caribbean",
  "Rest of South America", "Rest of Oceania"
)

# Custom function to title-case while preserving all-uppercase words
smart_title_case <- function(x) {
  sapply(strsplit(x, " "), function(words) {
    sapply(words, function(word) {
      if (grepl("^[A-Z]+$", word)) {
        word  # Keep as is if all uppercase
      } else {
        paste0(toupper(substring(word, 1, 1)), tolower(substring(word, 2)))
      }
    }) %>% paste(collapse = " ")
  }, USE.NAMES = FALSE)
}

# Apply to your vector
rest_of_countries <- smart_title_case(rest_of_countries)


excluded.countries <- c("Luxembourg", "United Arab Emirates", "Qatar", "Kuwait", "Libya", "Saudi Arabia", "Singapore")

# Combine all into a single exclusion list
exclude.countries <- c(
  #oil_gas_countries,
  #former_ussr_and_others,
  rest_of_countries,
  excluded.countries
)

################################################################################
# BACKUP FULL DATASET BEFORE FILTERING
################################################################################

# Save a copy of the full dataset before exclusions
data.orig <- data1
full.dataset <- data1

################################################################################
# FILTER OUT EXCLUDED COUNTRIES
################################################################################

# Remove countries listed in the exclusion list
data1 <- data1 %>%
  filter(!country_name %in% exclude.countries)

################################################################################
# PLOT: GDP per Capita vs Energy Service per Capita (Filtered)
################################################################################

ggplotly(
  ggplot(data = data1, aes(x = GDP_PPP_pcap, y = ES_pcap, colour = Hex)) +
    geom_line() +
    geom_point(aes(shape = country_name), size = 1.5, alpha = 0.7) +
    geom_text(
      data = data1 %>% filter(year == max(year) - 5),
      aes(label = country_name),
      hjust = -0.1, size = 2
    ) +
    labs(
      title = "GDP per Capita vs Energy Service per Capita",
      x = "GDP per Capita",
      y = "Energy Service per Capita"
    ) +
    theme_minimal() +
    create_theme1(1) +
    scale_color_identity() +
    theme(legend.position = "none")
)

################################################################################
# DEFINE WEIBULL MODEL FUNCTION USING DERIV
################################################################################

# Weibull model with interaction between density and Gini
weibull_model_1 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (k * Gini) / lambda * 
    (GDP_PPP_pcap / lambda)^(k * Gini - 1) * 
    exp(-(GDP_PPP_pcap / lambda)^(k * Gini)),
  namevec = c("alpha_0", "alpha_1", "k", "lambda"),
  function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", "alpha_0", "alpha_1", "k", "lambda")
)

################################################################################
# EVALUATE MODEL OVER A GRID OF VALUES
################################################################################

# Set fixed parameter values for visualization
alpha_0 <- 0.5
alpha_1 <- 0.01
k <- 1.5
lambda <- 10000
Gini <- 0.1

# Create sequences for population density and GDP per capita
density_vals <- seq(0, 1000, length.out = 50)
gdp_vals <- seq(1000, 50000, length.out = 50)

# Compute model values over the grid
z_vals <- outer(density_vals, gdp_vals, Vectorize(function(d, gdp) {
  weibull_model_1(d, Gini, gdp, alpha_0, alpha_1, k, lambda)[[1]]
}))

################################################################################
# 3D SURFACE PLOT OF THE WEIBULL FUNCTION
################################################################################

persp(
  x = density_vals,
  y = gdp_vals,
  z = z_vals,
  xlab = "Population Density (per sq km)",
  ylab = "GDP PPP per Capita",
  zlab = "Function Value",
  theta = 30, phi = 30, expand = 0.5,
  col = "lightblue"
)

################################################################################
# DEFINE WEIBULL MODEL FUNCTIONS USING `deriv`
################################################################################

# Weibull Model 2: Includes density and Gini
weibull_model_2 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * 
    k / exp(gamma + lambda * Gini) * 
    (GDP_PPP_pcap / exp(gamma + lambda * Gini))^(k - 1) * 
    exp(-(GDP_PPP_pcap / exp(gamma + lambda * Gini))^k),
  namevec = c("alpha_0", "alpha_1", "k", "lambda", "gamma"),
  function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", 
                   "alpha_0", "alpha_1", "k", "lambda", "gamma")
)

# Weibull Model 3: Adds lagged ES_pcap to the maturity term
weibull_model_3 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * 
    exp(gamma + k * Gini) / lambda * (GDP_PPP_pcap / lambda)^(exp(gamma + k * Gini) - 1) * 
    exp(- (GDP_PPP_pcap / lambda)^(exp(gamma + k * Gini))),
  namevec = c("alpha_0", "alpha_1", "k", "lambda", "gamma"),
  function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", "lag_ES_pcap", 
                   "alpha_0", "alpha_1", "k", "lambda", "gamma")
)

# Weibull Model 4: Adds lagged ES_pcap to the maturity term
weibull_model_4 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm + alpha_2 * lag_ES_pcap) * 
    k / exp(gamma + lambda * Gini) * 
    (GDP_PPP_pcap / exp(gamma + lambda * Gini))^(k - 1) * 
    exp(-(GDP_PPP_pcap / exp(gamma + lambda * Gini))^k),
  namevec = c("alpha_0", "alpha_1", "alpha_2", "k", "lambda", "gamma"),
  function.arg = c("density_psqkm", "Gini", "GDP_PPP_pcap", "lag_ES_pcap", 
                   "alpha_0", "alpha_1", "alpha_2", "k", "lambda", "gamma")
)

################################################################################
# DATA CLEANING AND DIAGNOSTICS
################################################################################

# Inspect data summary
summary(data1)

# Check for non-finite values in each column
sapply(data1, function(x) sum(!is.finite(x)))

# Remove rows with missing lag_ES_pcap (required for Weibull 3)
data1 <- data1 %>%
  filter(!is.na(lag_ES_pcap))

# Re-check for non-finite values after filtering
sapply(data1, function(x) sum(!is.finite(x)))

################################################################################
# MULTICOLLINEARITY CHECK
################################################################################

# Check for linear dependencies among predictors
alias(lm(ES_pcap ~ GDP_PPP_pcap_thousands + d_bar + u_bar + 
           rising_income + falling_income + lag_ES_pcap, data = data1))

################################################################################
# FIT WEIBULL MODEL 2 USING nlsLM
################################################################################
fit_weibull_2 <- nlsLM(
  ES_pcap ~ weibull_model_2(density_psqkm, Gini_01, GDP_PPP_pcap_thousands, 
                            alpha_0, alpha_1, k, lambda, gamma),
  data = data1,
  start = c(
    alpha_0 = 1000,
    alpha_1 = 0,
    k = 1,
    lambda = 1,
    gamma = 1
  ),
  # lower = c(
  #   alpha_0 = -Inf,
  #   alpha_1 = -Inf,
  #   k = 0,
  #   lambda = 0,
  #   gamma = 0
  # ),
  na.action = na.exclude,
  control = nls.lm.control(maxiter = 500)
)

# Display model summary
summary(fit_weibull_2)

################################################################################
# FIT WEIBULL MODEL 4 USING nlsLM (INCLUDES LAGGED ES_pcap)
################################################################################

nlslm_fit_weibull_4 <- nlsLM(
  ES_pcap ~ weibull_model_4(density_psqkm, Gini_01, GDP_PPP_pcap_thousands, lag_ES_pcap,
                            alpha_0, alpha_1, alpha_2, k, lambda, gamma),
  data = data1,
  start = c(
    alpha_0 = 1000,
    alpha_1 = 0,
    alpha_2 = 0,
    k = 1,
    lambda = 1,
    gamma = 1
  ),
  # lower = c(
  #   alpha_0 = -Inf,
  #   alpha_1 = -Inf,
  #   alpha_2 = -Inf,
  #   k = 0,
  #   lambda = 0,
  #   gamma = 0
  # ),
  na.action = na.exclude,
  control = nls.lm.control(maxiter = 500)
)

# Display model summary
summary(nlslm_fit_weibull_4)

# Extract starting values from nlsLM fit
start_vals <- list(fixed = coef(nlslm_fit_weibull_4))

# Fit nonlinear mixed-effects model with random effect on alpha_0
fit_weibull_4_nlme <- nlme(
  model = ES_pcap ~ weibull_model_4(
    density_psqkm, Gini_01, GDP_PPP_pcap_thousands, lag_ES_pcap,
    alpha_0, alpha_1, alpha_2, k, lambda, gamma
  ),
  data = data1,
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

summary(fit_weibull_4_nlme)

fit_weibull_4 <- fit_weibull_4_nlme
################################################################################
# CHECKING FOR NEGATIVE MATURITY TERMS
################################################################################

# Extract estimated coefficients from the fitted model
coefs <- coef(fit_weibull_4)

# Compute the maturity term for each observation
maturity_term <- with(data1, coefs["alpha_0"] + coefs["alpha_1"] * density_psqkm)

# Check if any maturity terms are negative
any_negative <- any(maturity_term < 0)
num_negative <- sum(maturity_term < 0)
which_negative <- which(maturity_term < 0)

# Output results
cat("Any negative maturity terms? ", any_negative, "\n")
cat("Number of negative values: ", num_negative, "\n")

################################################################################
# COPY MODEL PARAMETERS TO CLIPBOARD (WINDOWS ONLY)
################################################################################

# Extract parameters from the fitted model and copy to clipboard
parameters <- coef(fit_weibull_4)
write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

################################################################################
# MODEL DIAGNOSTICS: PREPARE DATA
################################################################################

# Create a clean dataset used in the model (omit rows with missing values)
model_data <- na.omit(data1[, c(
  "ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
  "rising_income", "falling_income", "lag_ES_pcap"
)])

# Optionally, use full dataset instead (commented out)
# full_model_data <- na.omit(full.dataset[, c(
#   "ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
#   "rising_income", "falling_income", "lag_ES_pcap"
# )]) %>%
#   filter(!is.na(lag_ES_pcap))
# model_data <- full_model_data

# Add model predictions and residuals
model_data$predicted <- predict(fit_weibull_4, level = 1)
model_data$residuals <- model_data$ES_pcap - model_data$predicted

################################################################################
# MODEL DIAGNOSTICS: PLOTS
################################################################################

# 1. Actual vs. Predicted Plot
plot(
  model_data$ES_pcap, model_data$predicted,
  xlab = "Actual ES_pcap", ylab = "Predicted ES_pcap",
  main = "Actual vs. Predicted ES_pcap",
  pch = 19, col = rgb(0, 0, 1, 0.5)
)
abline(a = 0, b = 1, col = "red", lty = 2)

# 2. Residuals vs. Fitted Values
plot(
  model_data$predicted, model_data$residuals,
  xlab = "Predicted ES_pcap", ylab = "Residuals",
  main = "Residuals vs. Fitted Values",
  pch = 19, col = rgb(0, 0, 1, 0.5)
)
abline(h = 0, col = "red", lty = 2)

# 3. Histogram of Residuals
hist(
  model_data$residuals,
  breaks = 30,
  main = "Histogram of Residuals",
  xlab = "Residuals",
  col = "lightblue", border = "white"
)

################################################################################
# MODEL SUMMARY EXPORT: FORMATTED TABLES AND FILE OUTPUT
################################################################################

################################################################################
# TIDY MODEL OUTPUT AND DISPLAY AS HTML TABLE
################################################################################

fit1 <- fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit <- broom.mixed::tidy(fit1)

# Create a neat HTML table for viewing in RStudio Viewer or browser
kable(tidy_fit, digits = 3, caption = "Summary of nlme Model") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

################################################################################
# EXPORT MODEL SUMMARY USING STARGAZER
################################################################################

# Export model summary to an HTML file (replace `fit_gately_nlsLM` with your model)
# stargazer(
#   fit_weibull_4,
#   type = "html",
#   title = "Model Summary",
#   out = here::here("results/model_summary_weibull1.html")
# )

################################################################################
# OPTIONAL: CLEAN TERM NAMES FOR EXPORT (IF NEEDED)
################################################################################

# Example of renaming terms in a tidy summary (if applicable)
# tidy_fit <- tidy_fit %>%
#   mutate(
#     term = gsub("random\\(Intercept\\)", "Random Intercept", term),
#     term = gsub("fixed\\(Intercept\\)", "Fixed Intercept", term)
#   )

# Export cleaned summary to CSV
# write.csv(tidy_fit, here::here("results/summary_table_logistic1.csv"), row.names = FALSE)

################################################################################
# CONFIDENCE INTERVALS FOR MODEL PARAMETERS (IF APPLICABLE)
################################################################################

# Extract confidence intervals for model parameters (for nlme models)
# intervals_output <- intervals(fit1)
# print(intervals_output)

################################################################################
# MODEL SELECTION AND PREDICTION
################################################################################

# Assign the chosen model to a generic variable for clarity
fit <- fit_weibull_4

# Prepare dataset for prediction: remove rows with missing values in key variables
data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
  "rising_income", "falling_income", "lag_ES_pcap"
)]), ]

# Generate predictions using the fitted model
data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model, level = 1)

# Calculate residuals
data1_model$residuals <- data1_model$ES_pcap - data1_model$predicted_ES_pcap

# Optional: Inspect a specific country
#View(data1_model %>% filter(country_name == "Libya"))

################################################################################
# DIAGNOSTIC PLOTS
################################################################################

# 1. Actual vs. Predicted ES_pcap
ggplot(data1_model, aes(x = ES_pcap, y = predicted_ES_pcap, color = country_name)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Actual vs Predicted ES_pcap",
    x = "Actual ES_pcap",
    y = "Predicted ES_pcap"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 2. Residuals by Year and Country
ggplot(data1_model, aes(x = year, y = residuals, color = country_name)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ country_name, scales = "free") +
  labs(
    title = "Residuals by Year and Country",
    x = "Year",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################
# MODEL COEFFICIENTS: FIXED EFFECTS SUMMARY
################################################################################

# Extract fixed effects using broom
fixed_df <- broom::tidy(fit) %>%
  mutate(effect_type = "Fixed")

# Optional: Combine with random effects if available
# random_effects <- ranef(fit)
# random_df <- random_effects %>%
#   tibble::rownames_to_column("country_name") %>%
#   pivot_longer(-country_name, names_to = "term", values_to = "estimate") %>%
#   mutate(effect_type = "Random")
# combined_df <- bind_rows(fixed_df, random_df)

# For now, use only fixed effects
combined_df <- fixed_df

# Optional: Display as a GT table
# gt(combined_df) %>%
#   tab_header(title = "Fixed and Random Effects from Nonlinear Mixed-Effects Model") %>%
#   cols_label(
#     country_name = "Country",
#     term = "Parameter",
#     estimate = "Estimate",
#     effect_type = "Effect Type"
#   ) %>%
#   fmt_number(columns = "estimate", decimals = 3)

################################################################################
# PREDICTED VS ACTUAL OVER TIME BY COUNTRY
################################################################################

ggplot(data1_model, aes(x = year, y = ES_pcap)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y = predicted_ES_pcap), color = "red") +
  geom_line(aes(y = predicted_ES_pcap), color = "red") +
  facet_wrap(~ country_name, scales = "free") +
  labs(
    title = "Predicted vs Actual Energy Service per Capita",
    x = "Year",
    y = "Energy Service per Capita"
  ) +
  theme_minimal()

################################################################################
# FORECASTING FUTURE ENERGY SERVICE PER CAPITA (ES_pcap)
# Using iterative prediction from 2024 to 2100 with lagged values
################################################################################

# Step 1: Filter future data (optional filter by country or year)
future_data <- data %>%
  filter(year >= 2024)

# Step 2: Prepare dataset for forecasting
# Set lag_ES_pcap to NA for years >= 2024 to simulate unknown future values
predictions_data <- data %>%
  mutate(lag_ES_pcap = ifelse(year >= 2024, NA, lag_ES_pcap))

# Step 3: Initialize variables
predictions <- list()  # To store predictions if needed
data.orig <- data.frame(predictions_data)  # Working copy of the dataset
model <- fit_weibull_4  # Fitted nlsLM model

################################################################################
# ITERATIVE FORECAST LOOP: YEAR-BY-YEAR PREDICTION
################################################################################

# for (i in 2024:2100) {
#   
#   # Step 3.1: Update lag_ES_pcap for the current year
#   data.orig <- data.orig %>%
#     group_by(country_id) %>%
#     arrange(year) %>%
#     mutate(lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)) %>%
#     ungroup()
#   
#   # Step 3.2: Filter data for the current forecast year
#   data.used <- filter(data.orig, year == i)
#   
#   # Step 3.3: Predict ES_pcap using the model
#   predictions <- predict(model, newdata = data.used, level = 1)
#   
#   # Step 3.4: Create a dataframe of predictions
#   prediction_df <- data.frame(
#     country_id = data.used$country_id,
#     year = data.used$year,
#     predicted_ES_pcap = predictions
#   )
#   
#   # Step 3.5: Update ES_pcap in the original dataset with predicted values
#   data.orig <- data.orig %>%
#     left_join(prediction_df, by = c("country_id", "year")) %>%
#     group_by(country_id) %>%
#     arrange(country_id, year) %>%
#     mutate(
#       ES_pcap = if_else(!is.na(predicted_ES_pcap), predicted_ES_pcap, ES_pcap),
#       lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)
#     ) %>%
#     select(-predicted_ES_pcap)  # Clean up temporary column
# }

# Extract known country_ids from the fitted model
known_ids <- unique(model$groups$country_id)

for (i in 2024:2100) {
  
  # Step 1: Update lag_ES_pcap for the current year
  data.orig <- data.orig %>%
    group_by(country_id) %>%
    arrange(year) %>%
    mutate(lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)) %>%
    ungroup()
  
  # Step 2: Filter data for the current forecast year
  data.used <- filter(data.orig, year == i)
  
  # Step 3: Split into known and new countries
  data.known <- data.used %>% filter(country_id %in% known_ids)
  data.new   <- data.used %>% filter(!country_id %in% known_ids)
  
  # Step 4: Predict using appropriate level
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
  
  # Step 5: Combine predictions
  prediction_df <- bind_rows(
    data.frame(country_id = data.known$country_id, year = data.known$year, predicted_ES_pcap = pred.known),
    data.frame(country_id = data.new$country_id, year = data.new$year, predicted_ES_pcap = pred.new)
  )
  
  # Step 6: Update ES_pcap in the original dataset
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


################################################################################
# FINALIZE FORECASTED DATASET
################################################################################

# Arrange the final dataset by country and year
predictions_data <- data.orig %>%
  arrange(country_id, year)

################################################################################
# PREPARE FUTURE DATA FOR VISUALIZATION
################################################################################

# Step 1: Extract future data (from 2024 onward) and assign predicted ES_pcap
future_data <- predictions_data %>%
  filter(year >= 2024) %>%
  mutate(predicted_ES_pcap = ES_pcap)

################################################################################
# EXPORT SELECTED COUNTRY DATA (e.g., Libya) TO CLIPBOARD
################################################################################
# Step 2: Extract Libya's forecast data for review or export
selected.libya <- future_data %>%
  filter(country_name == "Libya") %>%
  select(country_name, year, d_bar, u_bar, rising_income, falling_income, 
         lag_ES_pcap, GDP_PPP_pcap_thousands, ES_pcap)

# Copy to clipboard (Windows only)
write.table(selected.libya, file = "clipboard", sep = "\t", row.names = FALSE, col.names = TRUE)

################################################################################
# COMBINE HISTORICAL AND PREDICTED DATA FOR VISUALIZATION
################################################################################

# Step 3: Select relevant columns from historical and future datasets
selected1 <- data.orig %>%
  filter(year < 2024) %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

selected2 <- future_data %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

# Step 4: Combine and label data
aggregated.data <- rbind(selected1, selected2) %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= 2023, "Historical", "Predicted"))

# Step 5: Split into historical and predicted subsets
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")

################################################################################
# DEFINE COUNTRY COLORS FOR CONSISTENT PLOTTING
################################################################################

# Create a named vector of colors for each country
country_colors <- historical_data %>%
  select(country_name, Hex) %>%
  distinct() 

country_colors <- setNames(country_colors$Hex, country_colors$country_name)

################################################################################
# CREATE PLOT: GDP vs. ES_pcap (Historical vs. Predicted)
################################################################################

p1 <- ggplot() +
  # Historical lines
  geom_line(
    data = historical_data,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = country_name,
      group = country_name,
      text = paste(
        "Country:", country_name,
        "<br>Year:", year,
        "<br>GDP per capita:", scales::comma(GDP_PPP_pcap),
        "<br>ES per capita:", scales::comma(ES_pcap)
      )
    ),
    linetype = "solid"
  ) +
  # Predicted lines
  geom_line(
    data = predicted_data,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = country_name,
      group = country_name,
      text = paste(
        "Country:", country_name,
        "<br>Year:", year,
        "<br>GDP per capita:", scales::comma(GDP_PPP_pcap),
        "<br>ES per capita:", scales::comma(ES_pcap)
      )
    ),
    linetype = "dashed"
  ) +
  scale_color_manual(values = country_colors) +
  labs(
    title = "Predicted Energy Service per Capita using Weibull 3 Model",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

# Add the Weibull equation to your existing plot
p1 <- p1 +
  annotate(
    "text",
    x = Inf, y = Inf,  # Position in top-right corner (adjust as needed)
    label = "italic((alpha[0] + alpha[1]*density[psqkm] + alpha[2]*lag_ES[pcap]) * 
                    k / exp(gamma + lambda*Gini) * 
                    (GDP[PPP][pcap] / exp(gamma + lambda*Gini))^(k - 1) * 
                    exp(-(GDP[PPP][pcap] / exp(gamma + lambda*Gini))^k))",
    hjust = 1.1, vjust = 1.5,
    parse = TRUE,
    size = 4
  )


################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_3_re.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibull_3_re.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)



#############################################################################
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(here)

# Base plot with historical and predicted lines
p1 <- ggplot() +
  # Historical lines
  geom_line(
    data = historical_data,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = country_name,
      group = country_name,
      text = paste(
        "Country:", country_name,
        "<br>Year:", year,
        "<br>GDP per capita:", scales::comma(GDP_PPP_pcap),
        "<br>ES per capita:", scales::comma(ES_pcap)
      )
    ),
    linetype = "solid"
  ) +
  # Predicted lines
  geom_line(
    data = predicted_data,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = country_name,
      group = country_name,
      text = paste(
        "Country:", country_name,
        "<br>Year:", year,
        "<br>GDP per capita:", scales::comma(GDP_PPP_pcap),
        "<br>ES per capita:", scales::comma(ES_pcap)
      )
    ),
    linetype = "dashed"
  ) +
  scale_color_manual(values = country_colors) +
  labs(
    title = "Predicted Energy Service per Capita using Weibull 3 Model",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country"
  ) +
  theme_minimal() +
  create_theme(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

# Add plain-text Weibull equation annotation (compatible with ggplotly)
p1 <- p1 +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "ES = (α₀ + α₁·density + α₂·price) × Weibull(GDP, Gini)",
    hjust = 1.1, vjust = 1.5,
    size = 4
  )

# Convert to interactive plotly object and export
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_3_re.html"),
    selfcontained = TRUE
  )

#############################################################################################################################################
##### Model function
library(R6)
library(ggplot2)

ModelClass <- R6Class("ModelClass",
                      public = list(
                        name = NULL,
                        formula = NULL,
                        model = NULL,
                        data = NULL,
                        
                        initialize = function(name, formula) {
                          self$name <- name
                          self$formula <- formula
                        },
                        
                        fit = function(data) {
                          self$data <- data
                          self$model <- nls(self$formula, data = data, start = list(
                            alpha_0 = 1, alpha_1 = 1, alpha_2 = 1, k = 1, lambda = 1, gamma = 1
                          ))
                        },
                        
                        predict = function(newdata) {
                          predict(self$model, newdata)
                        },
                        
                        plot = function() {
                          ggplot(self$data, aes(x = GDP_PPP_pcap, y = ES_pcap)) +
                            geom_point() +
                            geom_line(aes(y = predict(self$model)), color = "blue") +
                            labs(title = paste("Model:", self$name))
                        }
                      )
)

# Define a Weibull model
weibull_formula <- ES_pcap ~ (alpha_0 + alpha_1 * density_psqkm + alpha_2 * lag_ES_pcap) *
  exp(gamma + k * Gini) / lambda *
  (GDP_PPP_pcap / lambda)^(exp(gamma + k * Gini) - 1) *
  exp(-(GDP_PPP_pcap / lambda)^exp(gamma + k * Gini))


my_data <- data1  # Assuming data1 is your dataset with the required columns
  
# Define a Weibull model
weibull_formula <- ES_pcap ~ (alpha_0 + alpha_1 * density_psqkm + alpha_2 * lag_ES_pcap) *
  exp(gamma + k * Gini) / lambda *
  (GDP_PPP_pcap / lambda)^(exp(gamma + k * Gini) - 1) *
  exp(-(GDP_PPP_pcap / lambda)^exp(gamma + k * Gini))

# Create model object
weibull_model <- ModelClass$new("Weibull", weibull_formula)

# Fit model
weibull_model$fit(my_data)

# Predict
predictions <- weibull_model$predict(new_data)

# Plot
weibull_model$plot()

#########################################################################################################################################
# Load required libraries
library(R6)
library(ggplot2)
library(dplyr)

# Sample synthetic dataset
set.seed(123)
n <- 100
my_data <- data.frame(
  GDP_PPP_pcap = runif(n, 1000, 50000),
  density_psqkm = runif(n, 50, 500),
  lag_ES_pcap = runif(n, 100, 1000),
  Gini = runif(n, 0.25, 0.5)
)

# True parameters for simulation
true_params <- list(alpha_0 = 0.5, alpha_1 = 0.01, alpha_2 = 0.005, k = 1.5, lambda = 20000, gamma = 0.3)

# Simulate ES_pcap using a Weibull-like function
my_data$ES_pcap <- with(my_data, 
                        (true_params$alpha_0 + true_params$alpha_1 * density_psqkm + true_params$alpha_2 * lag_ES_pcap) *
                          exp(true_params$gamma + true_params$k * Gini) / true_params$lambda *
                          (GDP_PPP_pcap / true_params$lambda)^(exp(true_params$gamma + true_params$k * Gini) - 1) *
                          exp(-(GDP_PPP_pcap / true_params$lambda)^exp(true_params$gamma + true_params$k * Gini))
)

# Define the R6 class
ModelClass <- R6Class("ModelClass",
                      public = list(
                        name = NULL,
                        formula = NULL,
                        model = NULL,
                        data = NULL,
                        
                        initialize = function(name, formula) {
                          self$name <- name
                          self$formula <- formula
                        },
                        
                        fit = function(data) {
                          self$data <- data
                          self$model <- nls(
                            self$formula,
                            data = data,
                            start = list(
                              alpha_0 = 0.5, alpha_1 = 0.01, alpha_2 = 0.005,
                              k = 1.5, lambda = 20000, gamma = 0.3
                            ),
                            control = nls.control(maxiter = 500, warnOnly = TRUE)
                          )
                        }
                        ,
                        
                        plot = function() {
                          self$data$predicted <- predict(self$model)
                          ggplot(self$data, aes(x = GDP_PPP_pcap)) +
                            geom_point(aes(y = ES_pcap), color = "black", alpha = 0.6) +
                            geom_line(aes(y = predicted), color = "blue", size = 1) +
                            labs(
                              title = paste("Model Fit:", self$name),
                              x = "GDP per capita (USD)",
                              y = "Energy Service per Capita"
                            ) +
                            theme_minimal()
                        }
                      )
)

# Define the Weibull-like formula
weibull_formula <- ES_pcap ~ (alpha_0 + alpha_1 * density_psqkm + alpha_2 * lag_ES_pcap) *
  exp(gamma + k * Gini) / lambda *
  (GDP_PPP_pcap / lambda)^(exp(gamma + k * Gini) - 1) *
  exp(-(GDP_PPP_pcap / lambda)^exp(gamma + k * Gini))

# Create and use the model
weibull_model <- ModelClass$new("Weibull Model", weibull_formula)
weibull_model$fit(my_data)

# Plot the results
plot <- weibull_model$plot()
print(plot)

