# Random Effects: https://stats.stackexchange.com/questions/242759/calculate-random-effect-predictions-manually-for-a-linear-mixed-model

################################################################################
# SETUP: Clean Environment and Load Required Packages
################################################################################

# Remove all objects from the workspace (use with caution)
rm(list = ls())

# Define number of CPU cores for parallel installation
ncpus <- 12

# List of required packages
packages <- c(
  "dplyr", "minpack.lm", "tidyr", "purrr", "ggplot2", "nlme", "broom.mixed",
  "gt", "prophet", "readr", "here", "stargazer", "plotly", "lme4", "car",
  "sysfonts", "extrafont", "showtext", "stringr", "knitr", "kableExtra", "stargazer",
  "nls2", "car", "future.apply", "texreg", "openxlsx"
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
# Diving Gini by 100 to ensure stability of estimates    
    Gini_00 = Gini / 100,
    Gini_01 = gini_case1 / 100,
    Gini_02 = gini_case2 / 100,
    Gini_03 = gini_case3 / 100,
    GDP_PPP_pcap_thousands = GDP_PPP_pcap / 1000
  ) %>%
  ungroup()

# Z-score standardization of Population Density
data <- data %>%
  group_by(country_name) %>%
  mutate(
    density_psqkm_std = (density_psqkm - mean(density_psqkm, na.rm = TRUE)) / sd(density_psqkm, na.rm = TRUE)
  ) %>%
  ungroup()

# Inspect data
summary(data)

# Display rows of data with NA values in density_psqkm
# data %>%
#   filter(is.na(density_psqkm)) %>%
#   select(country_name, year, density_psqkm, GDP_PPP_pcap, ES_pcap)

################################################################################
# BRAND PALETTE MERGE
################################################################################

# # Step 1: Read the hex codes from Excel (assuming header is in E10)
# hex_codes <- read_excel(
#   path = here::here("data", "Shell Scenarios v14.6 2023_06_23.xlsm"),
#   sheet = "Settings",
#   range = "D10:E50",  # Skip the header row
#   col_names = TRUE
# ) %>%
#   dplyr::select(-Colour) %>%
#   distinct()
# 
# # Step 2: Expand to 100 rows by sampling with replacement
# set.seed(1234)  # For reproducibility
# n_needed <- 100 - nrow(hex_codes)
# 
# expanded_palette <- bind_rows(
#   hex_codes,
#   hex_codes %>% slice_sample(n = n_needed, replace = TRUE)
# )
# 
# # Step 3: Confirm it's exactly 100 rows
# expanded_palette <- expanded_palette %>% slice_head(n = 100)
# saveRDS(expanded_palette, file = here::here("data", "shell_brand_palette_extended.rds"))

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
# PLOT: Standardized theme for Scenarios
################################################################################
# Create a named vector of colors for each country
country_colors <- data %>%
  select(country_name, Hex) %>%
  distinct() 

country_colors <- setNames(country_colors$Hex, country_colors$country_name)

create_theme1 <- function(text_size = 14) {
  theme(
    axis.title.y = element_text(size = text_size, family = "ShellMedium, sans"),
    axis.title.x = element_text(size = text_size, family = "ShellMedium, sans"),
    axis.text.y = element_text(size = text_size - 2, family = "ShellMedium, sans"),
    axis.text.x = element_text(size = text_size - 2, family = "ShellMedium, sans", angle = 45, hjust = 1),
    legend.position = c(1.05, 0.9),
    legend.text = element_text(size = text_size - 2, family = "ShellMedium, sans"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
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
# create_theme1 <- function(rel.size = 1) {
#   theme(
#     axis.title.y = element_text(size = 18 * rel.size, family = "ShellMedium"),
#     axis.title.x = element_text(size = 18 * rel.size, family = "ShellMedium"),
#     axis.text.y = element_text(size = 16 * rel.size, family = "ShellMedium"),
#     axis.text.x = element_text(size = 16 * rel.size, family = "ShellMedium", angle = 45, hjust = 1),
#     plot.margin = margin(5, 5, 5, 5),
#     legend.title = element_text(size = 16 * rel.size, family = "ShellMedium"),
#     legend.text = element_text(size = 15 * rel.size, family = "ShellMedium"),
#     legend.position = "top",
#     legend.background = element_rect(fill = "white", color = "black"),
#     plot.background = element_rect(fill = "white"),
#     panel.background = element_rect(fill = "white"),
#     panel.grid.major = element_line(color = "gray", linetype = "dashed"),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 18 * rel.size, family = "ShellMedium", hjust = 0.5),
#     strip.text = element_text(size = 16 * rel.size, family = "ShellMedium"),
#     strip.background = element_rect(fill = "lightgray", color = "black"),
#     panel.border = element_blank()
#   )
# }

################################################################################
# DATA FILTERING AND PREPARATION
################################################################################
# Set seed for reproducibility
set.seed(1234)

# Global variables
last.historical.year <- 2023 # Define the last year of historical data


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
data1 <- data %>%
  filter(year <= last.historical.year)

# Create a relative year variable (Year0) for modelling
data1 <- data1 %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()

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
    create_theme1(14) +
    scale_color_identity() +
    scale_x_continuous(labels = scales::comma, trans = "log") +
    theme(legend.position = "none")
)

################################################################################
# EXCLUDE COUNTRIES FROM ESTIMATION: FIND INFLUENTIAL VALUES AND/OR OUTLIERS AND
# EXCLUDE FROM MODEL FITTING
################################################################################

# Fit the model
model <- nls(ES_pcap ~ SSlogis(GDP_PPP_pcap, Asym, xmid, scal), data = data1)

# Raw residuals
res <- residuals(model)

# Estimate standard deviation of residuals
sigma <- summary(model)$sigma

# Standardized residuals
data1$std_residuals <- res / sigma

# Plot standardized residuals
ggplotly(ggplot(data1, aes(x = GDP_PPP_pcap, y = std_residuals)) +
           geom_point(aes(colour = country_name), size = 1.5, alpha = 0.7) +
           geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red") + # +/- 3 standard deviations rule of thumb
           labs(
             title = "Standardized Residuals vs GDP per Capita",
             x = "GDP per Capita",
             y = "Standardized Residuals"
           ) +
           create_theme1(14) +
           scale_color_manual(values = country_colors) +
           theme(legend.position = "none"))


# Identify high residuals
high_residual_threshold <- 2  # or whatever threshold you choose
data1$high_residual_flag <- abs(data1$std_residuals) > high_residual_threshold

# Get country names with high residuals
high_residual_flag <- unique(data1$country_name[data1$high_residual_flag])

#########################################################################################
# Calculate Cook's Distance for influential values
#########################################################################################

# # Define logistic function
# logistic <- function(x, Asym, xmid, scal) {
#   Asym / (1 + exp((xmid - x) / scal))
# }
# 
# # Fit full model
# full_model <- nls(ES_pcap ~ SSlogis(GDP_PPP_pcap, Asym, xmid, scal), data = data1)
# full_pred <- predict(full_model)
# n <- nrow(data1)
# p <- length(coef(full_model))
# mse <- mean(residuals(full_model)^2)
# # 
# # # # Initialize Cook's D vector - alternative : need not run, rather run parallel below
# # # cooks_d <- numeric(n)
# # # 
# # # # Loop through each observation
# # # for (i in 1:n) {
# # #   data_loo <- data1[-i, ]
# # #   tryCatch({
# # #     loo_model <- nls(ES_pcap ~ SSlogis(GDP_PPP_pcap, Asym, xmid, scal), data = data_loo)
# # #     loo_pred <- predict(loo_model, newdata = data1)
# # #     diff <- full_pred - loo_pred
# # #     cooks_d[i] <- sum(diff^2) / (p * mse)
# # #   }, error = function(e) {
# # #     cooks_d[i] <- NA
# # #   })
# # # }
# # 
# # Set up parallel plan
# plan(multisession)  # works on Windows
# 
# # Define the function for one iteration
# compute_cook <- function(i) {
#   data_loo <- data1[-i, ]
#   tryCatch({
#     loo_model <- nls(ES_pcap ~ SSlogis(GDP_PPP_pcap, Asym, xmid, scal), data = data_loo)
#     loo_pred <- predict(loo_model, newdata = data1)
#     diff <- full_pred - loo_pred
#     sum(diff^2) / (p * mse)
#   }, error = function(e) NA)
# }
# 
# # Run in parallel
# cooks_d <- future_sapply(1:n, compute_cook)
# 
# # Add to data frame and inspect
# data1$Cooks_D <- cooks_d
# print(head(data1[order(-data1$Cooks_D), ]))
# 
# plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's D")
# abline(h = 4 / n, col = "red", lty = 2)
# 
# # Save data1 to rds
# saveRDS(data1, file = here::here("data", "data1_with_cooks_d.rds"))
################################################################################

data1 <- readRDS(here::here("data", "data1_with_cooks_d.rds"))
# Cook's D threshold > 4/n

threshold <- 4 / nrow(data1)

# Identify high Cook's D
high_residual_threshold <- threshold  # or whatever threshold you choose
data1$high_cooksd_flag <- data1$Cooks_D > threshold

high_cooksd_countries <- unique(data1$country_name[data1$high_cooksd_flag])

# Plot Cook's D using ggplot2
ggplotly(
  ggplot(data1, aes(x = GDP_PPP_pcap, y = Cooks_D)) +
    geom_point(aes(colour = country_name), size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "red") + # Threshold line
    labs(
      title = "Cook's Distance vs GDP per Capita",
      x = "GDP per Capita",
      y = "Cook's Distance"
    ) +
    create_theme1(14) +
    scale_color_manual(values = country_colors) +
    theme(legend.position = "none")
)

#################################################################################
# Alternatives: Exclude countries based on historical WEM methodology
################################################################################

# Define groups of countries to exclude
oil_gas_countries <- c("Iran", "Malaysia", "Algeria", "Syria", "Sudan", 
                       "Ecuador", "Yemen", "Angola", "Nigeria", "Venezuela")

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

# Ensure that the rest_of_countries vector is in title case to match with data set
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

# remove USA from high_cooksd_countries
high_residual_flag_countries <- setdiff(high_residual_flag, "USA") # Want to include USA for saturation level
high_cooksd_flag_countries <- setdiff(high_cooksd_countries, c("USA", "Canada", "Norway", "Switzerland")) # Want to include USA for saturation level

# Combine all into a single exclusion list for which countries you wish to exclude from model fitting
exclude.countries <- c(
  #oil_gas_countries,
  #former_ussr_and_others,
  #rest_of_countries,
  #excluded.countries,
  high_cooksd_flag_countries
  #high_residual_flag_countries,
  
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
# DATA CLEANING AND DIAGNOSTICS
################################################################################

# Inspect data summary
summary(data1)

# Check for non-finite values in each column
sapply(data1, function(x) sum(!is.finite(x)))

# Remove rows with missing lag_ES_pcap (required for models with lagged variables)
data1 <- data1 %>%
  filter(!is.na(lag_ES_pcap))

# Re-check for non-finite values after filtering
sapply(data1, function(x) sum(!is.finite(x)))

################################################################################
# MULTICOLLINEARITY CHECK
################################################################################

# Check for linear dependencies among predictors
alias(lm(ES_pcap ~ GDP_PPP_pcap + d_bar + u_bar + 
           rising_income + falling_income + lag_ES_pcap, data = data1))

################################################################################
# DEFINE WEIBULL MODELS
################################################################################

# Weibull with Gini, variant 2, simple
weibull_model_1 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/scal)^( shape * Gini))),
  namevec = c("alpha_0", "alpha_1", "scal", "shape"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape")
)

# # Weibull with Gini: variant 1, simple
weibull_model_2 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(gamma + scal * Gini))^shape)),
  namevec = c("alpha_0", "alpha_1", "scal", "shape", "gamma"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape", "gamma")
)

# # Weibull with Gini: variant 1, simple - this may be most aligned with Thomas' hypothesis
weibull_model_3 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(scal))^(gamma + shape * Gini))),
  namevec = c("alpha_0", "alpha_1", "scal", "shape", "gamma"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape", "gamma")
)

# # Weibull with Gini: variant 1, simple
weibull_model_4 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(scal * Gini))^(shape))),
  namevec = c("alpha_0", "alpha_1", "scal", "shape"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape")
)

# # Weibull without Gini: variant 1, simple
weibull_model_5 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(scal))^(shape))),
  namevec = c("alpha_0", "alpha_1", "scal", "shape"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "alpha_0", "alpha_1", "scal", "shape")
)

weibull_model_6 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) *
    (1 - exp(-(GDP_PPP_pcap / (scal_base + scal_slope * Gini)) ^ (shape_base + shape_slope * Gini))),
  namevec = c("alpha_0", "alpha_1", "scal_base", "scal_slope", "shape_base", "shape_slope"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini",
                   "alpha_0", "alpha_1", "scal_base", "scal_slope", "shape_base", "shape_slope")
)

# weibull_model_6 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(scal * Gini))^ (shape * Gini))),
#   namevec = c("alpha_0", "alpha_1", "scal", "shape"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape")
# )

# weibull_model_6 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) *
#     (1 - exp(-(GDP_PPP_pcap / scal) ^ (shape_base + shape_slope * (Gini - gini_ref)))),
#   namevec = c("alpha_0", "alpha_1", "scal", "shape_base", "shape_slope", "gini_ref"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini",
#                    "alpha_0", "alpha_1", "scal", "shape_base", "shape_slope", "gini_ref")
# )


# gamma_0 captures the intercept, gamma_1 captures the linear effect of Gini,
# gamma_2 captures the quadratic effect of Gini, gamma_3 captures the linear effect of GDP_PPP_pcap,
# and gamma_4 captures the interaction effect between Gini and GDP_PPP_pcap (allowed to vary by income level).

# weibull_model_6 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) *
#     (1 - exp(-(GDP_PPP_pcap / (scal_base + scal_slope * Gini)) ^ (shape_base + shape_slope * (Gini - gini_ref)))),
#   namevec = c("alpha_0", "alpha_1", "scal_base", "scal_slope", "shape_base", "shape_slope", "gini_ref"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini",
#                    "alpha_0", "alpha_1", "scal_base", "scal_slope", "shape_base", "shape_slope", "gini_ref")
# )
# 
# logistic_model_5 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((xmid - GDP_PPP_pcap)/scal)))^(beta_1 * Gini) +
#     phi_0 * lag_ES_pcap,
#   namevec = c("alpha_0", "alpha_1", "xmid", "scal", "beta_1", "phi_0"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "lag_ES_pcap", "alpha_0", "alpha_1", "xmid", "scal", "beta_1", "phi_0")
# )

################################################################################
# FIT WEIBULL MODELS
################################################################################

# 1. Weibull with Gini, variant 2, simple
summary(data1[, c("ES_pcap", "GDP_PPP_pcap", "density_psqkm")])

# Display rows with NA values in ES_pcap
data1 %>%
  filter(is.na(ES_pcap)) %>%
  select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm)

summary(data1[, c("GDP_PPP_pcap", "density_psqkm", "Gini_01")])
any(!is.finite(data1$GDP_PPP_pcap))
any(!is.finite(data1$density_psqkm))
any(!is.finite(data1$Gini_01))


# Use nls2() to explore a wide range of starting values, then refine the best fit 
# with nlsLM() for robust and bounded optimization.
nls2_fit <- nls2(
  ES_pcap ~ weibull_model_1(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape), data = data1,
  start = data.frame(
    alpha_0 = seq(1000, 50000, length.out = 10),
    alpha_1 = seq(-10, 10, length.out = 10),
    scal = seq(1000, 70000, length.out = 10),  # avoid 0
    shape = seq(0.1, 10, length.out = 10)      # avoid 0 and large values
  ),
  na.action = na.exclude,
  ,
  algorithm = "brute-force"
)

summary(nls2_fit)

# fit2a <- nls(
#   ES_pcap ~ logistic_model_1(GDP_PPP_pcap, density_psqkm, alpha_0, alpha_1, xmid, scal),
#   data = data1,
#   #fixed = a0 + a1 + xmid + scal + b0 + b1 ~ 1,
#   #random = pdDiag(~ 1 | country_name),
#   #random =  xmid ~ 1 | country_name,
#   start = c(alpha_0 = 23000, alpha_1 = -55, xmid = 33000, scal = 32000),
#   na.action = na.exclude, #na.exclude to retain original number of rows
#   control = nlmeControl(pnlsTol = 0.5, maxIter = 200, minFactor = 1e-10, msMaxIter = 200, warnOnly = TRUE)
# )

# Extract starting values from nlsLM fit
# start_vals <- list(fixed = coef(nls2_fit))

best_start_vals <- as.list(coef(nls2_fit))


# Fit the model using nlsLM with starting values from nls2 
nlsLM_fit <- nlsLM(
  ES_pcap ~ weibull_model_1(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape),
  data = data1,
  start = best_start_vals,
  control = nls.lm.control(maxiter = 500, ftol = 1e-8),
  na.action = na.exclude
)

summary(nlsLM_fit)

# Residual analysis
plot(residuals(nls2_fit))
plot(residuals(nlsLM_fit))
# Appears that nlsM_fit is better

# Check if model generalizes well across subsets of data
set.seed(1234)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(data1)))

cv_results <- data.frame(fold = integer(), RMSE = numeric())

for (i in 1:k) {
  train_data <- data1[folds != i, ]
  test_data  <- data1[folds == i, ]
  
  try({
    fit <- nlsLM(
      ES_pcap ~ weibull_model_1(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape),
      data = train_data,
      start = best_start_vals,
      control = nls.lm.control(maxiter = 500, ftol = 1e-8)
    )
    
    preds <- predict(fit, newdata = test_data)
    rmse <- sqrt(mean((test_data$ES_pcap - preds)^2))
    
    cv_results <- rbind(cv_results, data.frame(fold = i, RMSE = rmse))
  }, silent = TRUE)
}

print(cv_results)
cat("Mean RMSE across folds:", mean(cv_results$RMSE), "\n")

# Fit stats of best fitting model
fit_stats1 <- data.frame(
  Model = "Weibull 1: With Gini Simple Variant 2",
  AIC = AIC(nlsLM_fit),
  BIC = BIC(nlsLM_fit)#,
  #R_squared = summary(fit_weibull_4)$r.squared
)

# combine all fit stats into a table
# Create a kable table for model fit statistics
# kable(fit_stats1, digits = 3, caption = "Model Fit Statistics: Logistic 1 Model wo Gini") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/model_fit_statistics_logistic_1.html"))

################################################################################
# CHECKING FOR NEGATIVE MATURITY TERMS
################################################################################

model_chosen_1 <- nlsLM_fit

# Extract estimated coefficients from the fitted model
coefs <- coef(model_chosen_1)

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
# COPY MODEL PARAMETERS TO CLIPBOARD 
################################################################################

# # Extract parameters from the fitted model and copy to clipboard
# parameters <- coef(model_chosen_1)
# write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

################################################################################
# MODEL DIAGNOSTICS: PREPARE DATA
################################################################################

# Create a clean dataset used in the model (omit rows with missing values)
# model_data <- na.omit(data1[, c(
#   "ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
#   "rising_income", "falling_income", "lag_ES_pcap"
# )])

# Prepare for in sample model diagnostics
model_data <- 
  data1[complete.cases(data1[, c(
    "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
  )]), ]

# Add model predictions and residuals
model_data$predicted <- predict(model_chosen_1, level = 0) # Level =1 if random effects
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

# Plot residuals versus fitted values using ggplot2 and colour by country_name
ggplotly(ggplot(model_data, aes(x = predicted, y = residuals, color = country_name)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted ES_pcap",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(legend.position = "none"))

# Plot qq plots of residuals facetted by country_name
p1 <- ggplotly(ggplot(model_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ country_name, scales = "free") +
  labs(
    title = "Q-Q Plot of Residuals by Country",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal() +
  theme(legend.position = "none"))

# Save p1 to interactive html
htmlwidgets::saveWidget(p1, here::here("plots/weibull1_qq_plot_residuals_by_country.html"))

# Plot influential values
ggplotly(ggplot(model_data, aes(x = predicted, y = residuals, color = country_name)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted ES_pcap",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(legend.position = "none"))

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

fit1 <- model_chosen_1 #fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit1 <- broom.mixed::tidy(fit1)

################################################################################
# MODEL SELECTION AND PREDICTION
################################################################################

# Assign the chosen model to a generic variable for clarity
fit <- model_chosen_1#fit_weibull_4

# Prepare dataset for prediction including out of sample: 
# remove rows with missing values in key variables included in equation
data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap_thousands", "density_psqkm", "Gini_01"
)]), ]

# Generate predictions using the fitted model
data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model, level = 0)

# Calculate residuals
data1_model$residuals <- data1_model$ES_pcap - data1_model$predicted_ES_pcap

# Optional: Inspect a specific country
#View(data1_model %>% filter(country_name == "Libya"))

################################################################################
# DIAGNOSTIC PLOTS
################################################################################

# 1. Actual vs. Predicted ES_pcap
ggplotly(ggplot(data1_model, aes(x = ES_pcap, y = predicted_ES_pcap, color = country_name)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Actual vs Predicted ES_pcap",
    x = "Actual ES_pcap",
    y = "Predicted ES_pcap"
  ) +
  theme_minimal() +
  theme(legend.position = "none"))

# 1.A Plot standardized residuals vs. fitted values
# standardize residuals

ggplotly(ggplot(data1_model, aes(x = predicted_ES_pcap, y = residuals, color = country_name)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted ES_pcap",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(legend.position = "none"))

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
  filter(year >= (last.historical.year + 1))

# Step 2: Prepare dataset for forecasting
# Set lag_ES_pcap to NA for years >= 2024 to simulate unknown future values
predictions_data <- data %>%
  mutate(lag_ES_pcap = ifelse(year >= 2024, NA, lag_ES_pcap))

# Step 3: Initialize variables
predictions <- list()  # To store predictions if needed
data.orig <- data.frame(predictions_data)  # Working copy of the dataset
model <- model_chosen_1 #fit_weibull_4  # Fitted nlsLM model

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

# Get base year value of ES_pcap per country_id for calculation of offset
base_year_values <- data.orig %>%
  filter(year == last.historical.year) %>%
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
base_year_values <- predict(model, newdata = data.orig %>% filter(year == last.historical.year), level = 0) %>%
  data.frame(country_id = data.orig$country_id[data.orig$year == last.historical.year], ES_pcap = .)

# Calculate offset value given difference between base_year value and model prediction
offset_values <- base_year_values %>%
  left_join(predictions_data %>% filter(year == last.historical.year) %>% select(country_id, ES_pcap), by = "country_id") %>%
  mutate(offset = ES_pcap.y - ES_pcap.x) %>%
  select(country_id, offset)

# Add offset value to predictions_data for year >= 2024
predictions_data <- predictions_data %>%
  left_join(offset_values, by = "country_id") %>%
  mutate(
    ES_pcap = if_else(year > last.historical.year, ES_pcap + offset, ES_pcap),
    lag_ES_pcap = if_else(year > last.historical.year, lag(ES_pcap), lag_ES_pcap)
  ) %>%
  select(-offset)

################################################################################
# PREPARE FUTURE DATA FOR VISUALIZATION
################################################################################

# Step 1: Extract future data (from 2024 onward) and assign predicted ES_pcap
future_data <- predictions_data %>%
  filter(year > last.historical.year) %>%
  mutate(predicted_ES_pcap = ES_pcap)

################################################################################
# COMBINE HISTORICAL AND PREDICTED DATA FOR VISUALIZATION
################################################################################

# Step 3: Select relevant columns from historical and future datasets
selected1 <- data.orig %>%
  filter(year <= last.historical.year) %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

selected2 <- future_data %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

# Step 4: Combine and label data
aggregated.data <- rbind(selected1, selected2) %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= last.historical.year, "Historical", "Predicted"))

# Step 5: Split into historical and predicted subsets
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")

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
    title = "Predicted Energy Service per Capita using Weibull 1 Model with Gini",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme1(16) +
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
    here::here("plots/predicted_ES_pcap_weibull_1_fe.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibull_1_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)

################################################################################
# FIT WEIBULL 2 MODEL
################################################################################

# # Check for invalid combinations
# # Create parameter grid
# param_grid <- expand.grid(
#   alpha_0 = seq(100, 75000, length.out = 10),
#   alpha_1 = seq(-10, 10, length.out = 10),
#   xmid = seq(1000, 70000, length.out = 10),
#   scal = seq(0, 100000, length.out = 10),
#   beta_1 = seq(0, 5, length.out = 10)
# )
# 
# # Sample one row of your data to test the model
# test_row <- data1[1, ]
# 
# # Function to test a parameter combination
# test_combination <- function(params) {
#   tryCatch({
#     result <- logistic_model_2(
#       GDP_PPP_pcap = test_row$GDP_PPP_pcap,
#       density_psqkm = test_row$density_psqkm,
#       Gini = test_row$Gini_01,
#       alpha_0 = params["alpha_0"],
#       alpha_1 = params["alpha_1"],
#       xmid = params["xmid"],
#       scal = params["scal"],
#       beta_1 = params["beta_1"]
#     )
#     if (is.na(result) || is.nan(result) || is.infinite(result)) return(FALSE)
#     return(TRUE)
#   }, error = function(e) FALSE)
# }
# 
# # Apply the test
# valid_combinations <- apply(param_grid, 1, test_combination)
# 
# # View problematic combinations
# bad_params <- param_grid[!valid_combinations, ]
# head(bad_params)

####################################################################################
# Fit the Weibull 2 model using nls2 with a wide range of starting values
####################################################################################

nls2_fit_2 <- nls2(
  ES_pcap ~ weibull_model_2(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape, gamma),
  data = data1,
  start = data.frame(
    alpha_0 = seq(1000, 50000, length.out = 10),
    alpha_1 = seq(-10, 10, length.out = 10),
    scal = seq(1000, 70000, length.out = 10),  # avoid 0
    shape = seq(0.1, 10, length.out = 10) ,     # avoid 0 and large values
    gamma = seq(-5000, 5000, length.out = 10)        # avoid 0 and large values
    ),
  algorithm = "brute-force"
)

summary(nls2_fit_2)

best_start_vals <- as.list(coef(nls2_fit_2))

# Fit the model using nlsLM with starting values
nlsLM_fit_2 <- nlsLM(
  ES_pcap ~ weibull_model_2(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape, gamma),
  data = data1,
  start = best_start_vals,
  control = nls.lm.control(maxiter = 500, ftol = 1e-8)
)

summary(nlsLM_fit_2)
# Residual analysis
plot(residuals(nls2_fit_2))
plot(residuals(nlsLM_fit_2))

model_chosen_2 <- nlsLM_fit_2

fit_stats2 <- data.frame(
  Model = "Weibull 2: With Gini Simple Variant 2",
  AIC = AIC(model_chosen_2),
  BIC = BIC(model_chosen_2)#,
  #R_squared = summary(fit_weibull_4)$r.squared
)

fit_stats_logistic <- rbind(fit_stats1, fit_stats2)

# Create a kable table for model fit statistics
# kable(fit_stats2, digits = 3, caption = "Model Fit Statistics for Logistic 2 Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/model_fit_statistics_logistic_2.html"))


################################################################################
# CHECKING FOR NEGATIVE MATURITY TERMS
################################################################################

# Extract estimated coefficients from the fitted model
coefs <- coef(model_chosen_2)

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
# parameters <- coef(fit_weibull_4)
# write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

################################################################################
# MODEL DIAGNOSTICS: PREPARE DATA
################################################################################
model_data <- 
  data1[complete.cases(data1[, c(
    "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
  )]), ]


# Add model predictions and residuals
model_data$predicted <- predict(model_chosen_2, level = 0)
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

fit1 <- model_chosen_2 #fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit2 <- broom.mixed::tidy(fit1)

# Create a neat HTML table for viewing in RStudio Viewer or browser
# kable(tidy_fit, digits = 3, caption = "Summary of Non-linear Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/tidy_model_summary_logistic2.html"))

################################################################################
# MODEL SELECTION AND PREDICTION
################################################################################

# Assign the chosen model to a generic variable for clarity
fit <- model_chosen_2#fit_weibull_4

# Prepare dataset for prediction: remove rows with missing values in key variables
data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap_thousands", "density_psqkm", "Gini_01"
)]), ]

# Generate predictions using the fitted model
data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model, level = 0)

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
  filter(year > last.historical.year)

# Step 2: Prepare dataset for forecasting
# Set lag_ES_pcap to NA for years >= 2024 to simulate unknown future values
predictions_data <- data %>%
  mutate(lag_ES_pcap = ifelse(year > last.historical.year, NA, lag_ES_pcap))

# Step 3: Initialize variables
predictions <- list()  # To store predictions if needed
data.orig <- data.frame(predictions_data)  # Working copy of the dataset
model <- model_chosen_2 #fit_weibull_4  # Fitted nlsLM model

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
  filter(year == last.historical.year) %>%
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
base_year_values <- predict(model, newdata = data.orig %>% filter(year == last.historical.year), level = 1) %>%
  data.frame(country_id = data.orig$country_id[data.orig$year == last.historical.year], ES_pcap = .)

# Calculate offset value given difference between base_year value and model prediction
offset_values <- base_year_values %>%
  left_join(predictions_data %>% filter(year == last.historical.year) %>% select(country_id, ES_pcap), by = "country_id") %>%
  mutate(offset = ES_pcap.y - ES_pcap.x) %>%
  select(country_id, offset)

# Add offset value to predictions_data for year >= 2024
predictions_data <- predictions_data %>%
  left_join(offset_values, by = "country_id") %>%
  mutate(
    ES_pcap = if_else(year > last.historical.year, ES_pcap + offset, ES_pcap),
    lag_ES_pcap = if_else(year > last.historical.year, lag(ES_pcap), lag_ES_pcap)
  ) %>%
  select(-offset)

################################################################################
# PREPARE FUTURE DATA FOR VISUALIZATION
################################################################################

# Step 1: Extract future data (from 2024 onward) and assign predicted ES_pcap
future_data <- predictions_data %>%
  filter(year > last.historical.year) %>%
  mutate(predicted_ES_pcap = ES_pcap)

################################################################################
# COMBINE HISTORICAL AND PREDICTED DATA FOR VISUALIZATION
################################################################################

# Step 3: Select relevant columns from historical and future datasets
selected1 <- data.orig %>%
  filter(year <= last.historical.year) %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

selected2 <- future_data %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

# Step 4: Combine and label data
aggregated.data <- rbind(selected1, selected2) %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= last.historical.year, "Historical", "Predicted"))

# Step 5: Split into historical and predicted subsets
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")

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
    title = "Predicted Energy Service per Capita using Weibull 2 Model: With Gini Simple Variant 1",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme1(16) +
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

################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_2_fe.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibull_2_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)

################################################################################
# FIT WEIBULL 3 MODEL
################################################################################
# Check for invalid combinations
# Create parameter grid
# param_grid <- expand.grid(
#   alpha_0 = seq(100, 75000, length.out = 10),
#   alpha_1 = seq(-10, 10, length.out = 10),
#   xmid = seq(10000, 70000, length.out = 10),
#   scal = seq(1000, 100000, length.out = 10),
#   beta_1 = seq(0.001, 5, length.out = 10),
#   beta_0 = seq(0.001, 5, length.out = 10)
# )
# 
# # Sample one row of your data to test the model
# test_row <- data1[1, ]
# 
# # Function to test a parameter combination
# test_combination <- function(params) {
#   tryCatch({
#     result <- logistic_model_3(
#       GDP_PPP_pcap = test_row$GDP_PPP_pcap,
#       density_psqkm = test_row$density_psqkm,
#       Gini = test_row$Gini_01,
#       alpha_0 = params["alpha_0"],
#       alpha_1 = params["alpha_1"],
#       xmid = params["xmid"],
#       scal = params["scal"],
#       beta_1 = params["beta_1"],
#       beta_0 = params["beta_0"]
#     )
#     if (is.na(result) || is.nan(result) || is.infinite(result)) return(FALSE)
#     return(TRUE)
#   }, error = function(e) FALSE)
# }
# 
# # Apply the test
# valid_combinations <- apply(param_grid, 1, test_combination)
# 
# # View problematic combinations
# bad_params <- param_grid[!valid_combinations, ]
# head(bad_params)

################################################################################
summary(data1[, c("ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01")])

nls2_fit_3 <- nls2(
  ES_pcap ~ weibull_model_3(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape, gamma),
  data = data1,
  start = data.frame(
    alpha_0 = seq(1000, 50000, length.out = 10),
    alpha_1 = seq(-10, 10, length.out = 10),
    scal = seq(1000, 70000, length.out = 10),  # avoid 0
    shape = seq(0.1, 10, length.out = 10) ,     # avoid 0 and large values
    gamma = seq(-5000, 5000, length.out = 10)        # avoid 0 and large values
  ),
  na.action = na.exclude,
  algorithm = "brute-force"
)

summary(nls2_fit_3)

best_start_vals <- as.list(coef(nls2_fit_3))

# Add beta_0 to best_start_vals
#best_start_vals$beta_0 <- 1  # Set a reasonable starting value for beta_0


# Fit the model using nlsLM with starting values
nlsLM_fit_3 <- nlsLM(
  ES_pcap ~ weibull_model_3(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape, gamma),
  data = data1,
  start = best_start_vals,
  #lower = c(alpha_0 = 0, alpha_1 = -Inf, xmid = 2000, scal = 0, beta_1 = -Inf, beta_0 = -Inf),
  control = nls.lm.control(maxiter = 500, ftol = 1e-8)
)

summary(nlsLM_fit_3)
plot(residuals(nls2_fit_3))
plot(residuals(nlsLM_fit_3))

model_chosen_3 <- nlsLM_fit_3

fit_stats3 <- data.frame(
  Model = "Weibull 3: With Gini Simple Variant 3",
  AIC = AIC(model_chosen_3),
  BIC = BIC(model_chosen_3)#,
  #R_squared = summary(fit_weibull_4)$r.squared
)


# Create a kable table for model fit statistics
# kable(fit_stats2, digits = 3, caption = "Model Fit Statistics for Logistic 2 Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/model_fit_statistics_logistic_2.html"))

# Save model fit statistics to kable html


################################################################################
# CHECKING FOR NEGATIVE MATURITY TERMS
################################################################################

# Extract estimated coefficients from the fitted model
coefs <- coef(model_chosen_3)

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
# parameters <- coef(fit_weibull_4)
# write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

################################################################################
# MODEL DIAGNOSTICS: PREPARE DATA
################################################################################

model_data <- 
  data1[complete.cases(data1[, c(
    "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
  )]), ]

# Add model predictions and residuals
model_data$predicted <- predict(model_chosen_3, level = 0)
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

fit1 <- model_chosen_3 #fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit3 <- broom.mixed::tidy(fit1)

# Create a neat HTML table for viewing in RStudio Viewer or browser
# kable(tidy_fit, digits = 3, caption = "Summary of Non-linear Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/tidy_model_summary_logistic3.html"))

################################################################################
# MODEL SELECTION AND PREDICTION
################################################################################

# Assign the chosen model to a generic variable for clarity
fit <- model_chosen_3#fit_weibull_4

# Prepare dataset for prediction: remove rows with missing values in key variables

data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
)]), ]


# Generate predictions using the fitted model
data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model, level = 0)

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
  filter(year > last.historical.year)

# Step 2: Prepare dataset for forecasting
# Set lag_ES_pcap to NA for years >= 2024 to simulate unknown future values
predictions_data <- data %>%
  mutate(lag_ES_pcap = ifelse(year > last.historical.year, NA, lag_ES_pcap))

# Step 3: Initialize variables
predictions <- list()  # To store predictions if needed
data.orig <- data.frame(predictions_data)  # Working copy of the dataset
model <- model_chosen_3 #fit_weibull_4  # Fitted nlsLM model

################################################################################
# ITERATIVE FORECAST LOOP: YEAR-BY-YEAR PREDICTION
################################################################################

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

# Calculate offset value by letting model predict 2023 values (base year) and calculate difference with actual
base_year_values <- predict(model, newdata = data.orig %>% filter(year == last.historical.year), level = 0) %>%
  data.frame(country_id = data.orig$country_id[data.orig$year == last.historical.year], ES_pcap = .)

# Calculate offset value given difference between base_year value and model prediction
offset_values <- base_year_values %>%
  left_join(predictions_data %>% filter(year == last.historical.year) %>% select(country_id, ES_pcap), by = "country_id") %>%
  mutate(offset = ES_pcap.y - ES_pcap.x) %>%
  select(country_id, offset)

# Add offset value to predictions_data for year >= 2024
predictions_data <- predictions_data %>%
  left_join(offset_values, by = "country_id") %>%
  mutate(
    ES_pcap = if_else(year > last.historical.year, ES_pcap + offset, ES_pcap),
    lag_ES_pcap = if_else(year > last.historical.year, lag(ES_pcap), lag_ES_pcap)
  ) %>%
  select(-offset)

################################################################################
# PREPARE FUTURE DATA FOR VISUALIZATION
################################################################################

# Step 1: Extract future data (from 2024 onward) and assign predicted ES_pcap
future_data <- predictions_data %>%
  filter(year > last.historical.year) %>%
  mutate(predicted_ES_pcap = ES_pcap)

################################################################################
# COMBINE HISTORICAL AND PREDICTED DATA FOR VISUALIZATION
################################################################################

# Step 3: Select relevant columns from historical and future datasets
selected1 <- data.orig %>%
  filter(year <= last.historical.year) %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

selected2 <- future_data %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

# Step 4: Combine and label data
aggregated.data <- rbind(selected1, selected2) %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= last.historical.year, "Historical", "Predicted"))

# Step 5: Split into historical and predicted subsets
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")

weibull_3_predicted <- predicted_data

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
    title = "Predicted Energy Service per Capita using Weibull 3 Model: With Gini Simple Variant 3",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme1(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_3_fe.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibull_3_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)

################################################################################
# FIT WEIBULL 4 MODEL
################################################################################
# Check for invalid combinations
# Create parameter grid
# param_grid <- expand.grid(
#   alpha_0 = seq(100, 75000, length.out = 10),
#   alpha_1 = seq(-10, 10, length.out = 10),
#   xmid = seq(10000, 70000, length.out = 10),
#   scal = seq(1000, 100000, length.out = 10),
#   beta_1 = seq(-10000, 10000, length.out = 10)
# )
# 
# # Sample one row of your data to test the model
# test_row <- data1[1, ]
# 
# # Function to test a parameter combination
# test_combination <- function(params) {
#   tryCatch({
#     result <- logistic_model_4(
#       GDP_PPP_pcap = test_row$GDP_PPP_pcap,
#       density_psqkm = test_row$density_psqkm,
#       Gini = test_row$Gini_01,
#       alpha_0 = params["alpha_0"],
#       alpha_1 = params["alpha_1"],
#       xmid = params["xmid"],
#       scal = params["scal"],
#       beta_1 = params["beta_1"]
#     )
#     if (is.na(result) || is.nan(result) || is.infinite(result)) return(FALSE)
#     return(TRUE)
#   }, error = function(e) FALSE)
# }
# 
# # Apply the test
# valid_combinations <- apply(param_grid, 1, test_combination)
# 
# # View problematic combinations
# bad_params <- param_grid[!valid_combinations, ]
# head(bad_params)

################################################################################

nls2_fit_4 <- nls2(
  ES_pcap ~ weibull_model_4(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape),
  data = data1,
  start = data.frame(
    alpha_0 = seq(1000, 50000, length.out = 10),
    alpha_1 = seq(-10, 10, length.out = 10),
    scal = seq(1000, 70000, length.out = 10),  # avoid 0
    shape = seq(0.1, 10, length.out = 10)      # avoid 0 and large values
  ),
  na.action = na.exclude,,
  algorithm = "brute-force"
)

summary(nls2_fit_4)

best_start_vals <- as.list(coef(nls2_fit_4))

# Add beta_0 to best_start_vals
#best_start_vals$beta_0 <- 1  # Set a reasonable starting value for beta_0


# Fit the model using nlsLM with starting values
nlsLM_fit_4 <- nlsLM(
  ES_pcap ~ weibull_model_4(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape),
  data = data1,
  start = best_start_vals,
  #lower = c(alpha_0 = 0, alpha_1 = -Inf, xmid = 2000, scal = 0, beta_1 = -Inf),
  control = nls.lm.control(maxiter = 500, ftol = 1e-8)
)

summary(nlsLM_fit_4)
plot(residuals(nls2_fit_4))
plot(residuals(nlsLM_fit_4))


# Plot residuals using ggplot2 against index and colour and group by country_name
ggplotly(data1 %>%
  mutate(residuals = residuals(nlsLM_fit_4)) %>%
  ggplot(aes(x = seq_along(residuals), y = residuals, color = country_name, group = country_name)) +
  geom_point() +
  geom_line() +
 facet_wrap(~ country_name, scales = "free") +
  labs(title = "Residuals of Weibull 4 Model by Country", x = "Index", y = "Residuals") +
  theme_minimal() +
  theme(legend.position = "none"))

# Residual diagnostics: Plot residuals against fitted values
# First get fitted values
res.data <- data1 %>%
  mutate(fitted_values = predict(nlsLM_fit_4, newdata = data1)) %>%
  mutate(residuals = residuals(nlsLM_fit_4))

ggplotly(res.data %>%
           ggplot(aes(x = fitted_values, y = residuals, color = country_name, group = country_name)) +
           geom_point() +
           #geom_line() +
           #facet_wrap(~ country_name, scales = "free") +
           labs(title = "Residuals of Weibull 4 Model by Country", x = "Fitted values", y = "Residuals") +
           theme_minimal() +
           theme(legend.position = "none"))

model_chosen_4 <- nlsLM_fit_4

fit_stats4 <- data.frame(
  Model = "Weibull 4: With Gini Simple Variant 1",
  AIC = AIC(model_chosen_4),
  BIC = BIC(model_chosen_4)#,
  #R_squared = summary(fit_weibull_4)$r.squared
)


# Create a kable table for model fit statistics
# kable(fit_stats2, digits = 3, caption = "Model Fit Statistics for Logistic 2 Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/model_fit_statistics_logistic_2.html"))

# Save model fit statistics to kable html


################################################################################
# CHECKING FOR NEGATIVE MATURITY TERMS
################################################################################

# Extract estimated coefficients from the fitted model
coefs <- coef(model_chosen_4)

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
# parameters <- coef(fit_weibull_4)
# write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

################################################################################
# MODEL DIAGNOSTICS: PREPARE DATA
################################################################################

model_data <- 
  data1[complete.cases(data1[, c(
    "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
  )]), ]

# Add model predictions and residuals
model_data$predicted <- predict(model_chosen_4, level = 0)
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

fit1 <- model_chosen_4 #fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit4 <- broom.mixed::tidy(fit1)

# Create a neat HTML table for viewing in RStudio Viewer or browser
# kable(tidy_fit, digits = 3, caption = "Summary of Non-linear Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/tidy_model_summary_logistic3.html"))

################################################################################
# MODEL SELECTION AND PREDICTION
################################################################################

# Assign the chosen model to a generic variable for clarity
fit <- model_chosen_4#fit_weibull_4

# Prepare dataset for prediction: remove rows with missing values in key variables

data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
)]), ]


# Generate predictions using the fitted model
data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model, level = 0)

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
  filter(year > last.historical.year)

# Step 2: Prepare dataset for forecasting
# Set lag_ES_pcap to NA for years >= 2024 to simulate unknown future values
predictions_data <- data %>%
  mutate(lag_ES_pcap = ifelse(year > last.historical.year, NA, lag_ES_pcap))

# Step 3: Initialize variables
predictions <- list()  # To store predictions if needed
data.orig <- data.frame(predictions_data)  # Working copy of the dataset
model <- model_chosen_4 #fit_weibull_4  # Fitted nlsLM model

################################################################################
# ITERATIVE FORECAST LOOP: YEAR-BY-YEAR PREDICTION
################################################################################

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

# Calculate offset value by letting model predict 2023 values (base year) and calculate difference with actual
base_year_values <- predict(model, newdata = data.orig %>% filter(year == last.historical.year), level = 0) %>%
  data.frame(country_id = data.orig$country_id[data.orig$year == last.historical.year], ES_pcap = .)

# Calculate offset value given difference between base_year value and model prediction
offset_values <- base_year_values %>%
  left_join(predictions_data %>% filter(year == last.historical.year) %>% select(country_id, ES_pcap), by = "country_id") %>%
  mutate(offset = ES_pcap.y - ES_pcap.x) %>%
  select(country_id, offset)

# Add offset value to predictions_data for year >= 2024
predictions_data <- predictions_data %>%
  left_join(offset_values, by = "country_id") %>%
  mutate(
    ES_pcap = if_else(year > last.historical.year, ES_pcap + offset, ES_pcap),
    lag_ES_pcap = if_else(year > last.historical.year, lag(ES_pcap), lag_ES_pcap)
  ) %>%
  select(-offset)

################################################################################
# PREPARE FUTURE DATA FOR VISUALIZATION
################################################################################

# Step 1: Extract future data (from 2024 onward) and assign predicted ES_pcap
future_data <- predictions_data %>%
  filter(year > last.historical.year) %>%
  mutate(predicted_ES_pcap = ES_pcap)

################################################################################
# COMBINE HISTORICAL AND PREDICTED DATA FOR VISUALIZATION
################################################################################

# Step 3: Select relevant columns from historical and future datasets
selected1 <- data.orig %>%
  filter(year <= last.historical.year) %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

selected2 <- future_data %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

# Step 4: Combine and label data
aggregated.data <- rbind(selected1, selected2) %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= last.historical.year, "Historical", "Predicted"))

# Step 5: Split into historical and predicted subsets
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")

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
    title = "Predicted Energy Service per Capita using Weibull 4 Model: With Gini Simple Variant 1",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme1(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_4_fe.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibullc_4_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)


#################################################################################

################################################################################
# FIT WEIBULL 5 MODEL
################################################################################
# Check for invalid combinations
# Create parameter grid
# param_grid <- expand.grid(
#   alpha_0 = seq(100, 75000, length.out = 10),
#   alpha_1 = seq(-10, 10, length.out = 10),
#   xmid = seq(10000, 70000, length.out = 10),
#   scal = seq(1000, 100000, length.out = 10),
#   beta_1 = seq(-10000, 10000, length.out = 10)
# )
# 
# # Sample one row of your data to test the model
# test_row <- data1[1, ]
# 
# # Function to test a parameter combination
# test_combination <- function(params) {
#   tryCatch({
#     result <- logistic_model_4(
#       GDP_PPP_pcap = test_row$GDP_PPP_pcap,
#       density_psqkm = test_row$density_psqkm,
#       Gini = test_row$Gini_01,
#       alpha_0 = params["alpha_0"],
#       alpha_1 = params["alpha_1"],
#       xmid = params["xmid"],
#       scal = params["scal"],
#       beta_1 = params["beta_1"]
#     )
#     if (is.na(result) || is.nan(result) || is.infinite(result)) return(FALSE)
#     return(TRUE)
#   }, error = function(e) FALSE)
# }
# 
# # Apply the test
# valid_combinations <- apply(param_grid, 1, test_combination)
# 
# # View problematic combinations
# bad_params <- param_grid[!valid_combinations, ]
# head(bad_params)

################################################################################

nls2_fit_5 <- nls2(
  ES_pcap ~ weibull_model_5(GDP_PPP_pcap, density_psqkm, alpha_0, alpha_1, scal, shape),
  data = data1,
  start = data.frame(
    alpha_0 = seq(1000, 50000, length.out = 10),
    alpha_1 = seq(-10, 10, length.out = 10),
    scal = seq(1000, 70000, length.out = 10),  # avoid 0
    shape = seq(0.1, 10, length.out = 10)      # avoid 0 and large values
  ),
  na.action = na.exclude,,
  algorithm = "brute-force"
)

summary(nls2_fit_5)

best_start_vals <- as.list(coef(nls2_fit_5))

# Add beta_0 to best_start_vals
#best_start_vals$beta_0 <- 1  # Set a reasonable starting value for beta_0


# Fit the model using nlsLM with starting values
nlsLM_fit_5 <- nlsLM(
  ES_pcap ~ weibull_model_5(GDP_PPP_pcap, density_psqkm, alpha_0, alpha_1, scal, shape),
  data = data1,
  start = best_start_vals,
  #lower = c(alpha_0 = 0, alpha_1 = -Inf, xmid = 2000, scal = 0, beta_1 = -Inf),
  control = nls.lm.control(maxiter = 500, ftol = 1e-8)
)

summary(nlsLM_fit_5)
plot(residuals(nls2_fit_5))
plot(residuals(nlsLM_fit_5))


# Plot residuals using ggplot2 against index and colour and group by country_name
ggplotly(data1 %>%
           mutate(residuals = residuals(nlsLM_fit_5)) %>%
           ggplot(aes(x = seq_along(residuals), y = residuals, color = country_name, group = country_name)) +
           geom_point() +
           geom_line() +
           facet_wrap(~ country_name, scales = "free") +
           labs(title = "Residuals of Weibull 4 Model by Country", x = "Index", y = "Residuals") +
           theme_minimal() +
           theme(legend.position = "none"))

# Residual diagnostics: Plot residuals against fitted values
# First get fitted values
res.data <- data1 %>%
  mutate(fitted_values = predict(nlsLM_fit_5, newdata = data1)) %>%
  mutate(residuals = residuals(nlsLM_fit_5))

ggplotly(res.data %>%
           ggplot(aes(x = fitted_values, y = residuals, color = country_name, group = country_name)) +
           geom_point() +
           #geom_line() +
           #facet_wrap(~ country_name, scales = "free") +
           labs(title = "Residuals of Weibull 4 Model by Country", x = "Fitted values", y = "Residuals") +
           theme_minimal() +
           theme(legend.position = "none"))

model_chosen_5 <- nlsLM_fit_5

fit_stats5 <- data.frame(
  Model = "Weibull 5: Without Gini Simple Variant 1",
  AIC = AIC(model_chosen_5),
  BIC = BIC(model_chosen_5)#,
  #R_squared = summary(fit_weibull_4)$r.squared
)


# Create a kable table for model fit statistics
# kable(fit_stats2, digits = 3, caption = "Model Fit Statistics for Logistic 2 Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/model_fit_statistics_logistic_2.html"))

# Save model fit statistics to kable html


################################################################################
# CHECKING FOR NEGATIVE MATURITY TERMS
################################################################################

# Extract estimated coefficients from the fitted model
coefs <- coef(model_chosen_5)

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
# parameters <- coef(fit_weibull_4)
# write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

################################################################################
# MODEL DIAGNOSTICS: PREPARE DATA
################################################################################

model_data <- 
  data1[complete.cases(data1[, c(
    "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
  )]), ]

# Add model predictions and residuals
model_data$predicted <- predict(model_chosen_5, level = 0)
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

fit1 <- model_chosen_5 #fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit4 <- broom.mixed::tidy(fit1)

# Create a neat HTML table for viewing in RStudio Viewer or browser
# kable(tidy_fit, digits = 3, caption = "Summary of Non-linear Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/tidy_model_summary_logistic3.html"))

################################################################################
# MODEL SELECTION AND PREDICTION
################################################################################

# Assign the chosen model to a generic variable for clarity
fit <- model_chosen_5#fit_weibull_4

# Prepare dataset for prediction: remove rows with missing values in key variables

data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
)]), ]


# Generate predictions using the fitted model
data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model, level = 0)

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
  filter(year > last.historical.year)

# Step 2: Prepare dataset for forecasting
# Set lag_ES_pcap to NA for years >= 2024 to simulate unknown future values
predictions_data <- data %>%
  mutate(lag_ES_pcap = ifelse(year > last.historical.year, NA, lag_ES_pcap))

# Step 3: Initialize variables
predictions <- list()  # To store predictions if needed
data.orig <- data.frame(predictions_data)  # Working copy of the dataset
model <- model_chosen_5 #fit_weibull_4  # Fitted nlsLM model

################################################################################
# ITERATIVE FORECAST LOOP: YEAR-BY-YEAR PREDICTION
################################################################################

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

# Calculate offset value by letting model predict 2023 values (base year) and calculate difference with actual
base_year_values <- predict(model, newdata = data.orig %>% filter(year == last.historical.year), level = 0) %>%
  data.frame(country_id = data.orig$country_id[data.orig$year == last.historical.year], ES_pcap = .)

# Calculate offset value given difference between base_year value and model prediction
offset_values <- base_year_values %>%
  left_join(predictions_data %>% filter(year == last.historical.year) %>% select(country_id, ES_pcap), by = "country_id") %>%
  mutate(offset = ES_pcap.y - ES_pcap.x) %>%
  select(country_id, offset)

# Add offset value to predictions_data for year >= 2024
predictions_data <- predictions_data %>%
  left_join(offset_values, by = "country_id") %>%
  mutate(
    ES_pcap = if_else(year > last.historical.year, ES_pcap + offset, ES_pcap),
    lag_ES_pcap = if_else(year > last.historical.year, lag(ES_pcap), lag_ES_pcap)
  ) %>%
  select(-offset)

################################################################################
# PREPARE FUTURE DATA FOR VISUALIZATION
################################################################################

# Step 1: Extract future data (from 2024 onward) and assign predicted ES_pcap
future_data <- predictions_data %>%
  filter(year > last.historical.year) %>%
  mutate(predicted_ES_pcap = ES_pcap)

################################################################################
# COMBINE HISTORICAL AND PREDICTED DATA FOR VISUALIZATION
################################################################################

# Step 3: Select relevant columns from historical and future datasets
selected1 <- data.orig %>%
  filter(year <= last.historical.year) %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

selected2 <- future_data %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

# Step 4: Combine and label data
aggregated.data <- rbind(selected1, selected2) %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= last.historical.year, "Historical", "Predicted"))

# Step 5: Split into historical and predicted subsets
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")

weibull_5_predicted <- predicted_data

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
    title = "Predicted Energy Service per Capita using Weibull 5 Model: Without Gini Simple Variant 1",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme1(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_5_fe.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibullc_5_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)

#################################################################################
# FIT WEIBULL MODEL 6
#################################################################################
# 

#####################################################################################


# Define a reasonable parameter grid for weibull_model_6
param_grid <- expand.grid(
  alpha_0     = seq(1000, 50000, length.out = 5),
  alpha_1     = seq(-5, 5, length.out = 5),
  scal_base   = seq(1000, 50000, length.out = 5),
  scal_slope  = seq(-1000, 1000, length.out = 5),
  shape_base  = seq(0.5, 5, length.out = 5),
  shape_slope = seq(-5, 5, length.out = 5)
)

# Sample one row of your data to test the model
test_row <- data1[1, ]

# Function to test a parameter combination
# test_combination <- function(params) {
#   tryCatch({
#     result <- weibull_model_6(
#       GDP_PPP_pcap = test_row$GDP_PPP_pcap,
#       density_psqkm = test_row$density_psqkm,
#       Gini = test_row$Gini_01,
#       alpha_0     = params["alpha_0"],
#       alpha_1     = params["alpha_1"],
#       scal_base   = params["scal_base"],
#       scal_slope  = params["scal_slope"],
#       shape_base  = params["shape_base"],
#       shape_slope = params["shape_slope"]
#     )
#     
#     # Check for invalid values
#     if (any(is.na(result), is.nan(result), is.infinite(result), result < 0)) return(FALSE)
#     return(TRUE)
#   }, error = function(e) FALSE)
# }

# Sample 10 rows from your data
test_rows <- data1[sample(nrow(data1), 10), ]

# Updated test function
test_combination <- function(params) {
  tryCatch({
    results <- apply(test_rows, 1, function(row) {
      result <- weibull_model_6(
        GDP_PPP_pcap = as.numeric(row["GDP_PPP_pcap"]),
        density_psqkm = as.numeric(row["density_psqkm"]),
        Gini = as.numeric(row["Gini_01"]),
        alpha_0     = params["alpha_0"],
        alpha_1     = params["alpha_1"],
        scal_base   = params["scal_base"],
        scal_slope  = params["scal_slope"],
        shape_base  = params["shape_base"],
        shape_slope = params["shape_slope"]
      )
      !any(is.na(result), is.nan(result), is.infinite(result), result < 0)
    })
    all(results)
  }, error = function(e) FALSE)
}


# param_grid <- param_grid %>%
#   dplyr::rowwise() %>%
#   dplyr::filter(
#     all((scal_base + scal_slope * test_row$Gini_01) > 0),
#     all((shape_base + shape_slope * (test_row$Gini_01 - gini_ref)) > 0)
#   ) %>%
#   dplyr::ungroup()

# Apply the test
valid_combinations <- apply(param_grid, 1, test_combination)

# Save valid combinations for a start grid
param_grid <- param_grid[valid_combinations, ]

# Choose random sample of n from param_grid
set.seed(1234)
param_grid <- param_grid[sample(nrow(param_grid), min(500, nrow(param_grid))), ]

# View problematic combinations (optional)
bad_params <- param_grid[!valid_combinations, ]
head(bad_params)

# Step 4: Fit the model using nls2

# nls2_fit_6 <- nls2(
#   ES_pcap ~ weibull_model_6(GDP_PPP_pcap, density_psqkm, Gini,
#                             alpha_0, alpha_1, scal_base, scal_slope, shape_base, shape_slope),
#   data = data1,
#   start = param_grid,
#   na.action = na.exclude,
#   algorithm = "brute-force"
# )
# 
# # Step 5: View summary
# summary(nls2_fit_6)


nlsLM_fit_6 <- nlsLM(
  ES_pcap ~ weibull_model_6(GDP_PPP_pcap, density_psqkm, Gini_01,
                            alpha_0, alpha_1, scal_base, scal_slope, shape_base, shape_slope),
  data = data1,
  start = as.list(param_grid[1, ]),
  control = nls.lm.control(maxiter = 500),
  na.action = na.exclude
)

summary(nlsLM_fit_6)
plot(residuals(nlsLM_fit_6))

# Residual diagnostics: Plot residuals against fitted values
# First get fitted values
library(broom)

res.data <- augment(nlsLM_fit_6, data = data1)

ggplot(res.data, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ country_name, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted Values") +
  theme_minimal()


model_chosen_6 <- nlsLM_fit_6

fit_stats6 <- data.frame(
  Model = "Weibull 6: Wit Gini Scale and Shape",
  AIC = AIC(model_chosen_6),
  BIC = BIC(model_chosen_6)#,
  #R_squared = summary(fit_weibull_4)$r.squared
)
# 
# 
# # Create a kable table for model fit statistics
# # kable(fit_stats2, digits = 3, caption = "Model Fit Statistics for Logistic 2 Model") %>%
# #   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
# #   save_kable(here::here("results/model_fit_statistics_logistic_2.html"))
# 
# # Save model fit statistics to kable html
# 
# 
################################################################################
# CHECKING FOR NEGATIVE MATURITY TERMS
################################################################################

# Extract estimated coefficients from the fitted model
coefs <- coef(model_chosen_6)

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
# parameters <- coef(fit_weibull_4)
# write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

################################################################################
# MODEL DIAGNOSTICS: PREPARE DATA
################################################################################

model_data <-
  data1[complete.cases(data1[, c(
    "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
  )]), ]

# Add model predictions and residuals
model_data$predicted <- predict(model_chosen_6, level = 0)
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

fit1 <- model_chosen_5 #fit_weibull_4  # Replace with your fitted model if needed
# Tidy the model output (example shown with `fit1`; replace with your model if needed)
tidy_fit4 <- broom.mixed::tidy(fit1)

# Create a neat HTML table for viewing in RStudio Viewer or browser
# kable(tidy_fit, digits = 3, caption = "Summary of Non-linear Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/tidy_model_summary_logistic3.html"))

################################################################################
# MODEL SELECTION AND PREDICTION
################################################################################

# Assign the chosen model to a generic variable for clarity
fit <- model_chosen_6#fit_weibull_4

# Prepare dataset for prediction: remove rows with missing values in key variables

data1_model <- full.dataset[complete.cases(full.dataset[, c(
  "ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01"
)]), ]


# Generate predictions using the fitted model
data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model, level = 0)

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
  filter(year > last.historical.year)

# Step 2: Prepare dataset for forecasting
# Set lag_ES_pcap to NA for years >= 2024 to simulate unknown future values
predictions_data <- data %>%
  mutate(lag_ES_pcap = ifelse(year > last.historical.year, NA, lag_ES_pcap))

# Step 3: Initialize variables
predictions <- list()  # To store predictions if needed
data.orig <- data.frame(predictions_data)  # Working copy of the dataset
model <- model_chosen_6 #fit_weibull_4  # Fitted nlsLM model

################################################################################
# ITERATIVE FORECAST LOOP: YEAR-BY-YEAR PREDICTION
################################################################################

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

# Calculate offset value by letting model predict 2023 values (base year) and calculate difference with actual
base_year_values <- predict(model, newdata = data.orig %>% filter(year == last.historical.year), level = 0) %>%
  data.frame(country_id = data.orig$country_id[data.orig$year == last.historical.year], ES_pcap = .)

# Calculate offset value given difference between base_year value and model prediction
offset_values <- base_year_values %>%
  left_join(predictions_data %>% filter(year == last.historical.year) %>% select(country_id, ES_pcap), by = "country_id") %>%
  mutate(offset = ES_pcap.y - ES_pcap.x) %>%
  select(country_id, offset)

# Add offset value to predictions_data for year >= 2024
predictions_data <- predictions_data %>%
  left_join(offset_values, by = "country_id") %>%
  mutate(
    ES_pcap = if_else(year > last.historical.year, ES_pcap + offset, ES_pcap),
    lag_ES_pcap = if_else(year > last.historical.year, lag(ES_pcap), lag_ES_pcap)
  ) %>%
  select(-offset)

################################################################################
# PREPARE FUTURE DATA FOR VISUALIZATION
################################################################################

# Step 1: Extract future data (from 2024 onward) and assign predicted ES_pcap
future_data <- predictions_data %>%
  filter(year > last.historical.year) %>%
  mutate(predicted_ES_pcap = ES_pcap)

################################################################################
# COMBINE HISTORICAL AND PREDICTED DATA FOR VISUALIZATION
################################################################################

# Step 3: Select relevant columns from historical and future datasets
selected1 <- data.orig %>%
  filter(year <= last.historical.year) %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

selected2 <- future_data %>%
  ungroup() %>%
  select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)

# Step 4: Combine and label data
aggregated.data <- rbind(selected1, selected2) %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= last.historical.year, "Historical", "Predicted"))

# Step 5: Split into historical and predicted subsets
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")

weibull_6_predicted <- predicted_data

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
    title = "Predicted Energy Service per Capita using Weibull 6 Model: With Gini Scale and Shape",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme1(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_6_fe.html"),
    selfcontained = TRUE
  )

# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibullc_6_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)

# 

###################################################################################
### COMPARISON OF PREDICTIONS ACROSS SELECTED MODELS

################################################################################
# CREATE PLOT: GDP vs. ES_pcap (Historical vs. Predicted)
################################################################################
weibull_6_predicted$line_type <- "Predicted: Weibull 6 (Gini complex)"
weibull_3_predicted$line_type <- "Predicted: Weibull 3 (Gini exponent)"
weibull_5_predicted$line_type <- "Predicted: Weibull 5 (wo Gini)"


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
        "<br>ES per capita:", scales::comma(ES_pcap),
        "<br>Line Type:", line_type
      )
    ),
    linetype = "solid"
  ) +
  # Predicted lines
  geom_path(
    data = weibull_6_predicted,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = country_name,
      group = country_name,
      text = paste(
        "Country:", country_name,
        "<br>Year:", year,
        "<br>GDP per capita:", scales::comma(GDP_PPP_pcap),
        "<br>ES per capita:", scales::comma(ES_pcap),
        "<br>Line Type:", line_type
      )
    ),
    linetype = "dashed"
  ) +
  geom_path(
    data = weibull_3_predicted,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = country_name,
      group = country_name,
      text = paste(
        "Country:", country_name,
        "<br>Year:", year,
        "<br>GDP per capita:", scales::comma(GDP_PPP_pcap),
        "<br>ES per capita:", scales::comma(ES_pcap),
        "<br>Line Type:", line_type
      )
    ),
    linetype = "twodash"
  ) +
  geom_path(
    data = weibull_5_predicted,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = country_name,
      group = country_name,
      text = paste(
        "Country:", country_name,
        "<br>Year:", year,
        "<br>GDP per capita:", scales::comma(GDP_PPP_pcap),
        "<br>ES per capita:", scales::comma(ES_pcap),
        "<br>Line Type:", line_type
      )
    ),
    linetype = "dotted"
  ) +
  scale_color_manual(values = country_colors) +
  labs(
    title = "Predicted Energy Service per Capita using Weibull Model",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme1(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

################################################################################
# EXPORT INTERACTIVE AND STATIC VERSIONS OF THE PLOT
################################################################################

# Save interactive HTML version
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/predicted_ES_pcap_weibull_best_fe.html"),
    selfcontained = TRUE
  )

p1 <- p1 + create_theme1(24)
# Save static PNG version
ggsave(
  filename = here::here("plots/predicted_ES_pcap_weibull_best_fe.png"),
  plot = p1,
  width = 16, height = 16, dpi = 150
)

#####################################################################################
###### NB TO CHECK!!!

best_start_vals <- as.list(coef(nls2_fit_3))

# Check if model generalizes well across subsets of data
set.seed(1234)
k <- 10
folds <- sample(rep(1:k, length.out = nrow(data1)))

cv_results <- data.frame(fold = integer(), RMSE = numeric())

for (i in 1:k) {
  train_data <- data1[folds != i, ]
  test_data  <- data1[folds == i, ]
  
  try({
    fit <- nlsLM(
      ES_pcap ~ weibull_model_3(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape, gamma),
      data = train_data,
      start = best_start_vals,
      control = nls.lm.control(maxiter = 500, ftol = 1e-8)
    )
    
    preds <- predict(fit, newdata = test_data)
    rmse <- sqrt(mean((test_data$ES_pcap - preds)^2))
    
    cv_results <- rbind(cv_results, data.frame(fold = i, RMSE = rmse))
  }, silent = TRUE)
}

print(cv_results)
cat("Mean RMSE across folds:", mean(cv_results$RMSE), "\n")
# 

### Bootstrapping to check parameter stability
set.seed(1234)
n_boot <- 1000
param_estimates <- data.frame()

for (i in 1:n_boot) {
  boot_data <- data1[sample(1:nrow(data1), replace = TRUE), ]
  
  try({
    fit <- nlsLM(
      ES_pcap ~ weibull_model_3(GDP_PPP_pcap, density_psqkm, Gini_01, alpha_0, alpha_1, scal, shape, gamma),
      data = boot_data,
      start = best_start_vals,
      control = nls.lm.control(maxiter = 500, ftol = 1e-8)
    )
    
    coefs <- coef(fit)
    param_estimates <- rbind(param_estimates, as.data.frame(t(coefs)))
  }, silent = TRUE)
}

# Summary statistics
summary_stats <- apply(param_estimates, 2, function(x) c(mean = mean(x), sd = sd(x), 
                                                         lower = quantile(x, 0.025), 
                                                         upper = quantile(x, 0.975)))
print(summary_stats)


#################################################################################
# MODEL FIT STATISTICS FOR LOGISTIC MODELS
#################################################################################
fit_stats_logistic <- rbind(fit_stats1, fit_stats2, fit_stats3, fit_stats4, fit_stats5, fit_stats6)

kable(fit_stats_logistic, digits = 3, caption = "Model Fit Statistics for Weibull Models") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable(here::here("results/model_fit_statistics_weibull_all.html"))

# # Export tidy_fit for all models to html
# htmlreg(
#   list(model_chosen_1, model_chosen_2, model_chosen_3, model_chosen_4, model_chosen_5),
#   file = here::here("results/tidy_model_summary_weibull_all.html"),
#   custom.model.names = c("Model 1", "Model 2"),#, "Model 3", "Model 4"),
#   
#   #custom.gof.names = c("Residual Std. Error", "AIC", "BIC"), # Only include what you want
#   override.gof = list(
#     summary(model_chosen_1)$sigma, AIC(model_chosen_1),
#     summary(model_chosen_2)$sigma, AIC(model_chosen_2)
#   ),
#   include.deviance = FALSE,
#  # override.gof.names = c("Residual Std. Error", "AIC", "BIC"),
#   
#   caption = "Weibull Model Summaries",
#  caption.above = TRUE
# )

htmlreg(
  list(model_chosen_1, model_chosen_2, model_chosen_3, model_chosen_4, model_chosen_5, model_chosen_6),
  file = here::here("results/tidy_model_summary_weibull_all.html"),
  custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  override.gof = list(
    summary(model_chosen_1)$sigma, AIC(model_chosen_1), BIC(model_chosen_1),
    summary(model_chosen_2)$sigma, AIC(model_chosen_2), BIC(model_chosen_2),
    summary(model_chosen_3)$sigma, AIC(model_chosen_3), BIC(model_chosen_3),
    summary(model_chosen_4)$sigma, AIC(model_chosen_4), BIC(model_chosen_4),
    summary(model_chosen_5)$sigma, AIC(model_chosen_5), BIC(model_chosen_5),
    summary(model_chosen_6)$sigma, AIC(model_chosen_6), BIC(model_chosen_6)
  ),
  override.gof.names = c("Sigma", "AIC", "BIC"),
  include.deviance = FALSE,
  caption = "Weibull Model Summaries",
  caption.above = TRUE
)

# Save parameters of all models_chosen into one xlsx file
# library(openxlsx)

model_params <- list(
  model_chosen_1 = coef(model_chosen_1),
  model_chosen_2 = coef(model_chosen_2),
  model_chosen_3 = coef(model_chosen_3),
  model_chosen_4 = coef(model_chosen_4),
  model_chosen_5 = coef(model_chosen_5),
  model_chosen_6 = coef(model_chosen_6)
)


# Step 1: Get all unique parameter names
all_params <- unique(unlist(lapply(model_params, names)))

# Step 2: Convert each model's coefficients to a full row with all parameters
model_params_df <- do.call(rbind, lapply(model_params, function(x) {
  x_full <- setNames(rep(NA, length(all_params)), all_params)
  x_full[names(x)] <- x
  as.data.frame(t(x_full))
}))

# Step 3: Save to .rds
saveRDS(model_params_df, here::here("results/model_parameters_all_weibull_models.rds"))

# 
# # models to HTML
# tidy_fit_logistic <- list(
#   tidy_fit1 = broom.mixed::tidy(model_chosen_1),
#   tidy_fit2 = broom.mixed::tidy(model_chosen_2),
#   tidy_fit3 = broom.mixed::tidy(model_chosen_3),
#   tidy_fit4 = broom.mixed::tidy(model_chosen_4)
# )
# 
# library(broom)
# library(knitr)
# library(kableExtra)
# 
# # Tidy the models
# tidy_models <- list(
#   model1 = tidy(model_chosen_1),
#   model2 = tidy(model_chosen_2),
#   model3 = tidy(model_chosen_3),
#   model4 = tidy(model_chosen_4)
# )
# 
# # Combine into one table
# combined <- dplyr::bind_rows(tidy_models, .id = "Model")
# 
# # Save as HTML
# kable(combined, format = "html", caption = "Logistic Model Summaries") %>%
#   kable_styling(full_width = FALSE) %>%
#   save_kable(file = here::here("results/tidy_model_summary_logistic_all-2.html"))

# Save tidy_fit_logistic to HTML files
# kable(tidy_fit_logistic$tidy_fit1, digits = 3, caption = "Summary of Non-linear Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/tidy_model_summary_logistic1.html"))

# Save to HTML file
# kable(tidy_fit, digits = 3, caption = "Summary of Non-linear Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   save_kable(here::here("results/tidy_model_summary_logistic1.html"))

################################################################################
# FIT LOGISTIC MODEL USING nlsLM (INCLUDES LAGGED ES_pcap)
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

################################################################################
# EVALUATE MODEL OVER A GRID OF VALUES
################################################################################

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
#     add_surface() %>%
#     layout(
#       title = "Interactive 3D Surface Plot of Logistic Model",
#       scene = list(
#         xaxis = list(title = "Population Density (per sq km)"),
#         yaxis = list(title = "GDP PPP per Capita"),
#         zaxis = list(title = "ES per capita")
#       )
#     ))
# 
# htmlwidgets::saveWidget(plot1, here::here("plots/logistic_model_1_plot.html"))

##################################################################################
# Logistic 3 grid values

# # Set parameter values
# alpha_0 <- 20000
# alpha_1 <- -0.15
# xmid <- 25000
# scal <- 45500
# beta_1 <- 0.05
# beta_0 <- 0.01
# 
# # Create a grid of values
# GDP_PPP_pcap_vals <- seq(10000, 100000, length.out = 100)
# density_vals <- seq(10, 1000, length.out = 100)
# gini_vals <- seq(0, 1, length.out = 100)
# grid <- expand.grid(GDP_PPP_pcap = GDP_PPP_pcap_vals, density_psqkm = density_vals, Gini = gini_vals)
# 
# # Compute model values
# grid$logistic_value <- with(grid, logistic_model_3(GDP_PPP_pcap, density_psqkm, Gini, alpha_0, alpha_1, xmid, scal, beta_1, beta_0))
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