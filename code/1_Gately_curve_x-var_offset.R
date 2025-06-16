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
  "sysfonts", "extrafont", "showtext", "stringr", "knitr", "kableExtra", "stargazer",
  "nls2"
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

# Subset historical data (up to and including 2023)
last.historical.year <- 2023
data1 <- data %>%
  filter(year <= 2023)

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
  #rest_of_countries,
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
# DEFINE LOGISTIC MODEL FUNCTION USING DERIV
################################################################################

multiple_logistic_model4 <- deriv(
  ~ (a0 + a1 * density_psqkm) * (1 / (1 + exp((GDP_PPP_pcap - xmid) / scal))) ^ (g0 + g1 * Gini),
  namevec = c("a0", "a1", "xmid", "scal", "g0", "g1"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "a0", "a1", "xmid", "scal", "g0", "g1", "Gini")
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
# FIT LOGISTIC MODEL 4 USING nlsLM (INCLUDES LAGGED ES_pcap)
################################################################################

# multiple_logistic_model4 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((xmid - GDP_PPP_pcap) / scal))) ^ (gamma_0 + gamma_1 * Gini),
#   namevec = c("alpha_0", "alpha_1", "xmid", "scal", "gamma_0", "gamma_1"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "alpha_0", "alpha_1", "xmid", "scal", "gamma_0", "gamma_1", "Gini")
# )
# 
# 
# 
# nlslm_fit_logistic_4 <- nlsLM(
#   ES_pcap ~ multiple_logistic_model4(GDP_PPP_pcap_thousands, density_psqkm, alpha_0, alpha_1, xmid, scal, gamma_0, gamma_1, Gini_01),
#   data = data1,
#   start = c(
#     alpha_0 = 1000,
#     alpha_1 = 0,
#     xmid = 10000,
#     scal = 2000,
#     gamma_0 = 1,
#     gamma_1 = 0.001
#   ),
#   # lower = c(
#   #   alpha_0 = -Inf,
#   #   alpha_1 = -Inf,
#   #   alpha_2 = -Inf,
#   #   k = 0,
#   #   lambda = 0,
#   #   gamma = 0
#   # ),
#   na.action = na.exclude,
#   control = nls.lm.control(maxiter = 500)
# )
# 
# # Display model summary
# summary(nlslm_fit_logistic_4)
# 
# # Extract starting values from nlsLM fit
# start_vals <- list(fixed = coef(nlslm_fit_logistic_4))
# 
# # # Fit nonlinear mixed-effects model with random effect on alpha_0
# # fit_logistic_4_nlme <- nlme(
# #   model = ES_pcap ~ multiple_logistic_model4(GDP_PPP_pcap_thousands, 
# #           density_psqkm, alpha_0, alpha_1, xmid, scal, gamma_0, gamma_1, Gini_01),
# #   data = data1,
# #   fixed = alpha_0 + alpha_1 + xmid + scal + gamma_0 + gamma_1 ~ 1,
# #   random = alpha_1 ~ 1 | country_id,  # Random effect on alpha_0
# #   start = start_vals,
# #   na.action = na.exclude,
# #   control = nlmeControl(
# #     pnlsTol = 0.5,
# #     maxIter = 500,
# #     minFactor = 1e-10,
# #     msMaxIter = 500,
# #     warnOnly = TRUE
# #   )
# # )
# # 
# # summary(fit_logistic_4_nlme)
# # 
# # fit_weibull_4 <- fit_logistic_4_nlme
# 
# ####################################################
# 
# multiple_logistic_model_4 <- deriv(
#   ~ ((a0 + a1 * density_psqkm) / (1 + exp((xmid - GDP_PPP_pcap) / scal))) * exp(b1 * Gini),
#   namevec = c("a0", "a1", "xmid", "scal", "b1"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "a0", "a1", "xmid", "scal", "b1")
# )
# 
# nlslm_fit_logistic_4 <- nlsLM(
#   ES_pcap ~ multiple_logistic_model_4(GDP_PPP_pcap_thousands, density_psqkm, Gini_01, a0, a1, xmid, scal, b1),
#   data = data1,
#   start = c(a0 = 5000, a1 = 10, xmid = 10000, scal = 1000, b1 = 0.001),
#   # lower = c(
#   #   alpha_0 = -Inf,
#   #   alpha_1 = -Inf,
#   #   alpha_2 = -Inf,
#   #   k = 0,
#   #   lambda = 0,
#   #   gamma = 0
#   # ),
#   na.action = na.exclude,
#   control = nls.lm.control(maxiter = 500)
# )
# 
# # Display model summary
# summary(nlslm_fit_logistic_4)
# 
# # Extract starting values from nlsLM fit
# start_vals <- list(fixed = coef(nlslm_fit_logistic_4))
# 
# # Fit nonlinear mixed-effects model with random effect on alpha_0
# fit_logistic_4_nlme <- nlme(
#   model = ES_pcap ~ multiple_logistic_model_4(GDP_PPP_pcap_thousands, density_psqkm, Gini_01, a0, a1, xmid, scal, b1),
#   data = data1,
#   fixed = a0 + a1 + xmid + scal + b1 ~ 1,
#   random = a0 ~ 1 | country_id,  # Random effect on alpha_0
#   start = start_vals,
#   na.action = na.exclude,
#   control = nlmeControl(
#     pnlsTol = 0.5,
#     maxIter = 500,
#     minFactor = 1e-10,
#     msMaxIter = 500,
#     warnOnly = TRUE
#   )
# )
# 
# summary(fit_logistic_4_nlme)
# 
# fit_weibull_4 <- fit_logistic_4_nlme #nlslm_fit_logistic_4 #fit_logistic_4_nlme

################################################################################
# FIT LOGISTIC MODELS
################################################################################

# logistic_model_1 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((GDP_PPP_pcap - xmid)/ scal))),
#   namevec = c("alpha_0", "alpha_1", "xmid", "scal"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "alpha_0", "alpha_1", "xmid", "scal")
# )

logistic_model_1 <- deriv(
  ~ (gamma_max + lambda * d_bar + phi * u_bar) * (theta_r * rising_income + theta_f * falling_income) * exp(alpha * exp(beta_i * GDP_PPP_pcap)) +
    (1 - theta_r * rising_income - theta_f * falling_income) * lag_ES_pcap,
  namevec = c("gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i"),
  function.arg = c("GDP_PPP_pcap", "d_bar", "u_bar", "rising_income", "falling_income", "lag_ES_pcap", "gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i")
)

# Rebalancing because of perfect multicollinearity

# logistic_model_1 <- deriv(
#   ~ (gamma_max + lambda * d_bar + phi * u_bar) * (theta_f + theta_r * rising_income) * exp(alpha * exp(beta_i * GDP_PPP_pcap)) +
#     (1 - (theta_f + (theta_r) * rising_income)) * lag_ES_pcap,
#   namevec = c("gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i"),
#   function.arg = c("GDP_PPP_pcap", "d_bar", "u_bar", "rising_income", "lag_ES_pcap", "gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i")
# )

logistic_model_1 <- deriv(
  ~ (gamma_max + lambda * d_bar + phi * u_bar) * (theta_r  + theta_f * falling_income) * exp(alpha * exp(beta_i * GDP_PPP_pcap)) +
    (1 - theta_r - theta_f * falling_income) * lag_ES_pcap,
  namevec = c("gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i"),
  function.arg = c("GDP_PPP_pcap", "d_bar", "u_bar", "falling_income", "lag_ES_pcap", "gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i")
)

# Check if rising_income and falling_income are collinear
alias(lm(rising_income ~ falling_income, data = data1))

# logistic_model_1 <- function(GDP_PPP_pcap, d_bar, u_bar, rising_income, falling_income,
#                                     lag_ES_pcap, gamma_max, lambda, phi, theta_r, theta_f, alpha, beta_i) {
#   term1 <- (gamma_max + lambda * d_bar + phi * u_bar) * 
#     (theta_r * rising_income + theta_f * falling_income) * 
#     exp(alpha * exp(beta_i * GDP_PPP_pcap))
#   
#   term2 <- (1 - theta_r * rising_income - theta_f * falling_income) * lag_ES_pcap
#   
#   result <- term1 + term2
#   return(result)
# }

# logistic_model_1 <- function(GDP_PPP_pcap, Gini, d_bar, u_bar, rising_income, falling_income,
#                              lag_ES_pcap, gamma_max, lambda, phi, theta_r, theta_f, beta_i, alpha_1) {
#   term1 <- (gamma_max + lambda * d_bar + phi * u_bar) * 
#     (theta_r * rising_income + theta_f * falling_income) * 
#     exp((alpha_1 * Gini) * exp(beta_i * GDP_PPP_pcap))
#   
#   term2 <- (1 - theta_r * rising_income - theta_f * falling_income) * lag_ES_pcap
#   
#   result <- term1 + term2
#   return(result)
# }

################################################################################
# # Set parameter values
# alpha_0 <- 20000
# alpha_1 <- -0.15
# xmid <- 25000
# scal <- 45500
# 
# # Create a grid of values
# GDP_PPP_pcap_vals <- seq(10000, 100000, length.out = 100)
# density_vals <- seq(10, 1000, length.out = 100)
# grid <- expand.grid(GDP_PPP_pcap = GDP_PPP_pcap_vals, density_psqkm = density_vals)
# 
# # Compute model values
# grid$logistic_value <- with(grid, logistic_model_1(GDP_PPP_pcap, density_psqkm, alpha_0, alpha_1, xmid, scal))
# 
# # Plot using ggplot2
# ggplot(grid, aes(x = GDP_PPP_pcap, y = density_psqkm, z = logistic_value)) +
#   geom_contour_filled() +
#   scale_fill_viridis_d() +
#   labs(
#     title = "Logistic Model Visualization",
#     x = "GDP_PPP_pcap",
#     y = "Population Density (per sq km)",
#     fill = "Model Output"
#   ) +
#   theme_minimal()
# 
# ################################################################################
# # EVALUATE MODEL OVER A GRID OF VALUES
# ################################################################################
# 
# # Define the logistic model function
# logistic_model <- function(GDP_PPP_pcap, density_psqkm, alpha_0, alpha_1, xmid, scal) {
#   (alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((GDP_PPP_pcap - xmid) / scal)))
# }
# 
# logistic_model <- function(GDP_PPP_pcap, density_psqkm, alpha_0, alpha_1, xmid, scal) {
#   (alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp(-(xmid - GDP_PPP_pcap) / scal)))
# }
# 
# # Set parameter values
# alpha_0 <- 15000
# alpha_1 <- -0.15
# xmid <- 50000
# scal <- 45000
# 
# # Create sequences
# GDP_PPP_pcap_vals <- seq(10000, 100000, length.out = 100)
# density_vals <- seq(10, 1000, length.out = 100)
# 
# # Create grid and compute z values
# z_vals <- outer(density_vals, GDP_PPP_pcap_vals, Vectorize(function(d, gdp) {
#   logistic_model(gdp, d, alpha_0, alpha_1, xmid, scal)
# }))
# 
# # Create interactive 3D surface plot
# (plot1 <- plot_ly(
#   x = ~density_vals,
#   y = ~GDP_PPP_pcap_vals,
#   z = ~z_vals
# ) %>%
#   add_surface() %>%
#   layout(
#     title = "Interactive 3D Surface Plot of Logistic Model",
#     scene = list(
#       xaxis = list(title = "Population Density (per sq km)"),
#       yaxis = list(title = "GDP PPP per Capita"),
#       zaxis = list(title = "ES per capita")
#     )
#   ))
# 
# htmlwidgets::saveWidget(plot1, here::here("plots/logistic_model_1_plot.html"))

################################################################################
summary(data1[, c("ES_pcap", "GDP_PPP_pcap", "density_psqkm")])

# Display rows with NA values in ES_pcap
data1 %>%
  filter(is.na(ES_pcap)) %>%
  select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm)


################################################################################
# Fit the model using nls2 with brute-force algorithm
# nls2_fit <- nls2(
#   ES_pcap ~ logistic_model_1(GDP_PPP_pcap_thousands, d_bar, u_bar, rising_income, falling_income, lag_ES_pcap, gamma_max, lambda, phi, theta_r, theta_f, alpha, beta_i),#(alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((GDP_PPP_pcap - xmid)/scal))),
#   data = data1,
#   start = data.frame(
#     gamma_max = seq(0, 50000, length.out = 100),
#     lambda = seq(-10, 0, length.out = 100),
#     phi = seq(-10, 0, length.out = 100),
#     theta_r = seq(-10, 10, length.out = 100),
#     theta_f = seq(-10, 10, length.out = 100),
#     alpha = seq(-50, 0, length.out = 100),
#     beta_i = seq(-50, 0, length.out = 100)
#   ),
#   #algorithm = "random-search"
#   algorithm = "brute-force"
# )

logistic_model_1 <- deriv(
  ~ (gamma_max + lambda * d_bar + phi * u_bar) * (theta_r  + theta_f * falling_income) * exp(alpha * exp(beta_i * GDP_PPP_pcap)) +
    (1 - theta_r - theta_f * falling_income) * lag_ES_pcap,
  namevec = c("gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i"),
  function.arg = c("GDP_PPP_pcap", "d_bar", "u_bar", "falling_income", "lag_ES_pcap", "gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i")
)

logistic_model_1 <- deriv(
  ~ (gamma_max + lambda * d_bar + phi * u_bar) * ( 1 - exp( alpha * (exp(beta_i * GDP_PPP_pcap) -1))) +
    rho * lag_ES_pcap,
  namevec = c("gamma_max", "lambda", "phi", "alpha", "beta_i", "rho"),
  function.arg = c("GDP_PPP_pcap", "d_bar", "u_bar", "lag_ES_pcap", "gamma_max", "lambda", "phi", "alpha", "beta_i", "rho")
)

nls2_fit <- nls2(
  ES_pcap ~ logistic_model_1(GDP_PPP_pcap_thousands, d_bar, u_bar, lag_ES_pcap, 
                             gamma_max, lambda, phi, alpha, beta_i, rho),#(alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((GDP_PPP_pcap - xmid)/scal))),
  data = data1,
  start = data.frame(
    gamma_max = seq(0, 50000, length.out = 100),
    lambda = seq(-10, 0, length.out = 100),
    phi = seq(-10, 0, length.out = 100),
    #theta_r = seq(-10, 10, length.out = 100),
    #theta_f = seq(-10, 10, length.out = 100),
    alpha = seq(-50, 0, length.out = 100),
    beta_i = seq(-50, 0, length.out = 100),
    rho = seq(-1, 1, length.out = 100)
  ),
  #algorithm = "random-search"
  algorithm = "brute-force"
)

summary(nls2_fit)

logistic_model_simple <- deriv(
  ~ gamma_max * (1 - exp(-alpha * (exp(beta_i * GDP_PPP_pcap) - 1))),
  namevec = c("gamma_max", "alpha", "beta_i"),
  function.arg = c("GDP_PPP_pcap", "gamma_max", "alpha", "beta_i")
)

nls2_fit_simple <- nls2(
  ES_pcap ~ logistic_model_simple(GDP_PPP_pcap, gamma_max, alpha, beta_i),
  data = data1,
  start = data.frame(
    gamma_max = seq(0, 50000, length.out = 100),
    alpha = seq(-50, 0, length.out = 100),
    beta_i = seq(-50, 0, length.out = 100)
  ),
  algorithm = "brute-force"
)

summary(nls2_fit_simple)

###################################################################################################################################################
summary(nls2_fit)

best_start_vals <- as.list(coef(nls2_fit))

# Fit the model using nlsLM with starting values
nlsLM_fit <- nlsLM(
  ES_pcap ~ logistic_model_1(GDP_PPP_pcap_thousands, d_bar, u_bar, rising_income, falling_income, lag_ES_pcap, gamma_max, lambda, phi, theta_r, theta_f, alpha, beta_i),#(alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((GDP_PPP_pcap - xmid)/scal))),
  data = data1,
  start = best_start_vals,
    control = nls.lm.control(
      maxiter = 1000, # Increase max iterations
      maxfev = 5000, # Increase max function evaluations
      ftol = 1e-10, # Tighter tolerance for function value
      ptol = 1e-10, # Tighter tolerance for parameter changes
      nprint = 1 # Optional: prints progress every iteration
    ),
    # lower = c(
    #   gamma_max = 0,
    #   lambda = -Inf,
    #   phi = -Inf,
    #   theta_r = -Inf,
    #   theta_f = -Inf,
    #   alpha = -Inf,
    #   beta_i = -Inf
    # ),
    # upper = c(
    #   gamma_max = Inf,
    #   lambda = 0,
    #   phi = 0,
    #   theta_r = Inf,
    #   theta_f = Inf,
    #   alpha = 0,
    #   beta_i = 0
    # ),
  na.action = na.exclude
)

summary(nlsLM_fit)

# fit_gately_nlsLM <- nlsLM(
#   ES_pcap ~ logistic_model_1(GDP_PPP_pcap, d_bar, u_bar, rising_income, 
#                              falling_income, lag_ES_pcap, gamma_max, lambda, 
#                              phi, theta_r, theta_f, alpha, beta_i),
#   data = data1,
#   start = c(
#     gamma_max = 5000, # If ES in original, set to 1000
#     lambda = -1e-6,
#     phi = -1e-6,
#     theta_r = 1,
#     theta_f = 1,
#     alpha = -1e-6,
#     beta_i = -1e-6
#   ),
#   # # Setting constraints
#   lower = c(
#     gamma_max = 100,
#     lambda = -Inf,
#     phi = -Inf,
#     theta_r = -Inf,
#     theta_f = -Inf,
#     alpha = -Inf,
#     beta_i = -Inf
#   ),
#   upper = c(
#     gamma_max = Inf,
#     lambda = 0,
#     phi = 0,
#     theta_r = Inf,
#     theta_f = Inf,
#     alpha = 0,
#     beta_i = 0
#   ),
#   na.action =  na.exclude,
#   control = nls.lm.control(maxiter = 500)
# )


nlsLM_fit <- nls2_fit
summary(nlsLM_fit)
plot(residuals(nlsLM_fit))

fit_weibull_4 <- nlsLM_fit


bicmodel1 <- BIC(fit_weibull_4)

fit_stats1 <- data.frame(
  Model = "Gately 1",
  AIC = AIC(fit_weibull_4),
  BIC = BIC(fit_weibull_4)#,
  #R_squared = summary(fit_weibull_4)$r.squared
)
# Create a kable table for model fit statistics
kable(fit_stats1, digits = 3, caption = "Model Fit Statistics for Gately 1 Model") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable(here::here("results/model_fit_statistics_gately_1.html"))


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
# model_data <- na.omit(data1[, c(
#   "ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
#   "rising_income", "falling_income", "lag_ES_pcap"
# )])


model_data <- 
  data1[complete.cases(data1[, c(
    "ES_pcap", "GDP_PPP_pcap_thousands", "density_psqkm", "Gini_01"
  )]), ]



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

fit1 <- fit_weibull_4 #fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit <- broom.mixed::tidy(fit1)


# Save to HTML file
kable(tidy_fit, digits = 3, caption = "Summary of Non-linear Model") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable(here::here("results/tidy_model_summary_gately1.html"))

################################################################################
# EXPORT MODEL SUMMARY USING STARGAZER
################################################################################

# # Export model summary to an HTML file (replace `fit_gately_nlsLM` with your model)
# stargazer(
#   fit1,
#   type = "html",
#   title = "Model Summary",
#   out = here::here("results/model_summary_logistic1.html")
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
fit <- fit_weibull_4#fit_weibull_4

# Prepare dataset for prediction: remove rows with missing values in key variables
data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
  "rising_income", "falling_income", "lag_ES_pcap"
)]), ]

data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap_thousands", "density_psqkm", "Gini_01"
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
model <- fit_weibull_4 #fit_weibull_4  # Fitted nlsLM model

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

# Get base year value of ES_pcap per country_id
base_year_values <- data.orig %>%
  filter(year == 2023) %>%
  select(country_id, ES_pcap)

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

# Calculate offset value by letting model predict 2023 values (base year) and calculate difference with actual
base_year_values <- predict(model, newdata = data.orig %>% filter(year == 2023), level = 1) %>%
  data.frame(country_id = data.orig$country_id[data.orig$year == 2023], ES_pcap = .)

# Calculate offset value given difference between base_year value and model prediction
offset_values <- base_year_values %>%
  left_join(predictions_data %>% filter(year == 2023) %>% select(country_id, ES_pcap), by = "country_id") %>%
  mutate(offset = ES_pcap.y - ES_pcap.x) %>%
  select(country_id, offset)

# Add offset value to predictions_data for year >= 2024
predictions_data <- predictions_data %>%
  left_join(offset_values, by = "country_id") %>%
  mutate(
    ES_pcap = if_else(year >= 2024, ES_pcap + offset, ES_pcap),
    lag_ES_pcap = if_else(year >= 2024, lag(ES_pcap), lag_ES_pcap)
  ) %>%
  select(-offset)

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
  geom_path(
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
  geom_path(
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
    title = "Predicted Energy Service per Capita using Gately 1 Model",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

# Add the Logistic equation to your existing plot
# p1 <- p1 +
#   annotate(
#     "text",
#     x = Inf, y = Inf,  # Position in top-right corner (adjust as needed)
#     label = "italic((alpha[0] + alpha[1]*density[psqkm]) * 
#                     (1/(1 + exp(- (GDP[PPP][pcap] - xmid)/ scale))))",
#     hjust = 1.1, vjust = 1.5,
#     parse = TRUE,
#     size = 4
#   )

# # Add the Weibull equation to your existing plot
# p1 <- p1 +
#   annotate(
#     "text",
#     x = Inf, y = Inf,  # Position in top-right corner (adjust as needed)
#     label = "italic((alpha[0] + alpha[1]*density[psqkm] + alpha[2]*lag_ES[pcap]) * 
#                     k / exp(gamma + lambda*Gini) * 
#                     (GDP[PPP][pcap] / exp(gamma + lambda*Gini))^(k - 1) * 
#                     exp(-(GDP[PPP][pcap] / exp(gamma + lambda*Gini))^k))",
#     hjust = 1.1, vjust = 1.5,
#     parse = TRUE,
#     size = 4
#   )
# 

################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_gately_1_fe.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_gately_1_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)

