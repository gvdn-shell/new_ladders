# Random Effects: https://stats.stackexchange.com/questions/242759/calculate-random-effect-predictions-manually-for-a-linear-mixed-model
# Remove all objects in R workspace (use with caution)
rm(list = ls())

# Check that required packages are installed and install if missing
ncpus <- 12
packages <- c("dplyr",
              "minpack.lm",
              "tidyr",
              "purrr",
              "ggplot2",
              "nlme",
              "broom.mixed",
              "gt",
              "prophet",
              "readr",
              "purrr",
              "here",
              "stargazer",
              "plotly",
              "lme4",
              "car",
              "sysfonts",
              "extrafont",
              "showtext") #
installed_packages <- packages %in% rownames(installed.packages())

if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load required packages
invisible(lapply(packages, library, character.only = TRUE))

loadfonts()
extrafont::loadfonts(device="win")

##################################################################################################

##################### Plotting for presentation
# syntax: font_add(family = "<family_name>", regular = "/path/to/font/file")
font_add("ShellMedium", "ShellMedium.ttf")
font_families()

## automatically use showtext for new devices
showtext_auto()

####################################################################################


data <- readRDS("data/all_data_wem_espcap_imputation_wem_urban.rds") %>%
  group_by(country_id) %>%
  arrange(country_id, year) %>%
  #mutate(ES_pcap = ES_pcap / 1000) %>%
  mutate(lag_ES_pcap = lag(ES_pcap, order_by = year),
         Gini_00 = Gini / 100,
         Gini_01 = gini_case1 / 100,
         Gini_02 = gini_case2 / 100,
         Gini_03 = gini_case3 / 100,
         GDP_PPP_pcap_thousands = GDP_PPP_pcap / 1000
         ) %>%
  ungroup()
# 
# shell.brand.palette <- readxl::read_excel(here::here("Data", "Shell Scenarios v14.6 2023_06_23.xlsm"), sheet = "Settings",
#                                           range = "L10:M270", col_names = TRUE) %>%
#   dplyr::select(-Colour)
# 
# saveRDS(shell.brand.palette, file = here::here("data", "shell_brand_palette_extended.rds"))

shell.brand.palette <- readRDS(here::here("data", "shell_brand_palette_extended.rds"))

data <- shell.brand.palette %>%
  mutate(Hex = paste0("#", Hex)) %>%
  mutate(country_id = row_number()) %>% 
  # Join with data but only where country_id is in both datasets
  select(country_id, Hex) %>%
  dplyr::distinct() %>%
  merge(data , by = "country_id") #%>%
  #distinct()


#####
# Filter out countries with less than 2 data points for Gini, urbanization_perc and density_psqkm
set.seed(1234)
data <- data %>%
  group_by(country_name) %>%
  filter(
    sum(!is.na(Gini_01)) >= 2,
    sum(!is.na(urbanization_perc)) >= 2,
    sum(!is.na(density_psqkm)) >= 2
  ) %>%
  ungroup() %>%
  # Add random noise since nls doesn't hanndle zero-residual data well.
  mutate(Gini_noise = Gini + rnorm(1, mean = 0, sd = 0.01))


# Historical data
data1 <- data %>%
  filter(year <= 2024)

##########################################################################################

data1 <- data1 %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()

# Fit model by country by using Scurve function above and nlsLM function or similar
# Fit the model using nlsLM for each country
# Note: nlsLM is from the minpack.lm package
# Note: The model fitting process may take a while depending on the size of the dataset
# Note: The model fitting process may take a while depending on the size of the dataset
############################################################################################################################
### Plots

create_theme1 <- function(rel.size = 1) {
  theme(
    axis.title.y = element_text(size = 18 * rel.size, family = "ShellMedium"),
    axis.title.x = element_text(size = 18 * rel.size, family = "ShellMedium"),
    axis.text.y = element_text(size = 16 * rel.size, family = "ShellMedium"),
    axis.text.x = element_text(size = 16 * rel.size, family = "ShellMedium", angle = 45, hjust = 1),
    plot.margin = margin(l = 5, r = 5, t = 5, b = 5),
    legend.title = element_text(family = "ShellMedium", size = 16 * rel.size),
    legend.position = "top",
    legend.text = element_text(size = 15 * rel.size, family = "ShellMedium"),
    legend.background = element_rect(fill = "white", color = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18 * rel.size, family = "ShellMedium", hjust = 0.5),
    plot.caption = element_blank(),
    plot.subtitle = element_blank(),
    strip.text = element_text(size = 16 * rel.size, family = "ShellMedium"),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    panel.border = element_blank()
    
  )
}

ggplotly(ggplot(data = data1, aes(x = GDP_PPP_pcap, y = ES_pcap, colour = Hex)) +
  #geom_point() +
  geom_line() +
    # show country_name in hover
  geom_point(aes(shape = country_name), size = 1.5, alpha = 0.7) +
  #geom_line(aes(linetype = country_name), size = 0.5, alpha = 0.7) +
  geom_text(data = data1 %>% filter(year == max(year) - 5), aes(label = country_name), hjust = -0.1, size = 2) +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per Capita vs Energy Service per Capita",
       x = "GDP per Capita",
       y = "Energy Service per Capita") +
  theme_minimal() +
  create_theme1(1) +
  scale_color_identity() +
  theme(legend.position = "none"))

############################################################################################################################
# Remove funny countries from estimation

# Define countries to exclude
oil_gas_countries <- c("Iran", "Malaysia", "Algeria", "Syria", "Sudan", "Ecuador", "Yemen", "Angola", "Nigeria", "Venezuela")

former_ussr_and_others <- c("Russia", "Uzbekistan", "Belarus", "Poland", "Romania", "Ukraine", "Bulgaria",
                            "Azerbaijan", "Kazakhstan", "Turkmenistan", "North Korea")

rest_of_countries <- c("International Marine Bunkers", "Rest of Europe West Other", "Rest of Europe East Other",
                       "Rest of EU New 12", "Baltic States", "Rest of Central Asia", "Rest of East Asia",
                       "Rest of SE Asia", "Rest of South Asia", "Rest of Middle East", "Rest Of Arabian Peninsula",
                       "Rest of North Africa", "Rest of East Africa", "Rest of Southern Africa",
                       "Rest of West Africa", "Rest of North America", "Rest of Central America & Caribbean",
                       "Rest of South America", "Rest of Oceania")

excluded.countries <- c("Luxembourg", "United Arab Emirates", "Qatar", "Kuwait", "Libya", "Saudi Arabia")

# Combine all into one exclusion list
exclude.countries <- c(oil_gas_countries, former_ussr_and_others, rest_of_countries, excluded.countries)


full.dataset <- data1

data1 <- data1 %>%
  filter(!country_name %in% exclude.countries)

ggplotly(ggplot(data = data1, aes(x = GDP_PPP_pcap, y = ES_pcap, colour = Hex)) +
           #geom_point() +
           geom_line() +
           # show country_name in hover
           geom_point(aes(shape = country_name), size = 1.5, alpha = 0.7) +
           #geom_line(aes(linetype = country_name), size = 0.5, alpha = 0.7) +
           geom_text(data = data1 %>% filter(year == max(year) - 5), aes(label = country_name), hjust = -0.1, size = 2) +
           #geom_smooth(method = "lm", se = FALSE) +
           labs(title = "GDP per Capita vs Energy Service per Capita",
                x = "GDP per Capita",
                y = "Energy Service per Capita") +
           theme_minimal() +
           create_theme1(1) +
           scale_color_identity() +
           theme(legend.position = "none"))
### https://stats.stackexchange.com/questions/205918/how-to-use-a-sigmoidal-function-in-a-multiple-nonlinear-regression

# Define the non-linear model function
gately_model <- deriv(
  ~ (gamma_max + lambda * d_bar + phi * u_bar) * (theta_r * rising_income + theta_f * falling_income) * exp(alpha * exp(beta_i * GDP_PPP_pcap)) +
    (1 - theta_r * rising_income - theta_f * falling_income) * lag_ES_pcap,
  namevec = c("gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i"),
  function.arg = c("GDP_PPP_pcap", "d_bar", "u_bar", "rising_income", "falling_income", "lag_ES_pcap", "gamma_max", "lambda", "phi", "theta_r", "theta_f", "alpha", "beta_i")
)

summary(data1)
sapply(data1, function(x) sum(!is.finite(x)))

# Drop rows where lag_ES_pcap is missing
data1 <- data1 %>%
  filter(!is.na(lag_ES_pcap))
sapply(data1, function(x) sum(!is.finite(x)))

### check for collinearity between variables

library(car)
alias(lm(ES_pcap ~ GDP_PPP_pcap_thousands + d_bar + u_bar + rising_income + falling_income + lag_ES_pcap, data = data1))

# # 1. Define the grid of starting values
# start_grid <- expand.grid(
#   gamma_max = c(500, 1000, 1500),
#   lambda = c(-1, 0, 1),
#   phi = c(-1, 0, 1),
#   theta_r = c(0.5, 1),
#   theta_f = c(0.5, 1),
#   alpha = c(0.0001, 0.001),
#   beta_i = c(0.0001, 0.001)
# )
# 
# # 2. Run the grid search
# results <- list()
# for (i in 1:nrow(start_grid)) {
#   start_vals <- as.list(start_grid[i, ])
#   cat("Trying combination", i, "of", nrow(start_grid), "\n")
#   tryCatch({
#     fit <- nls(
#       ES_pcap ~ gately_model(GDP_PPP_pcap_thousands, d_bar, u_bar, rising_income, falling_income, lag_ES_pcap,
#                              gamma_max, lambda, phi, theta_r, theta_f, alpha, beta_i),
#       data = data1,
#       start = start_vals,
#       control = nls.control(maxiter = 200, warnOnly = TRUE)
#     )
#     results[[i]] <- list(success = TRUE, fit = fit, start = start_vals, rss = sum(resid(fit)^2))
#   }, error = function(e) {
#     results[[i]] <- list(success = FALSE, error = e$message, start = start_vals)
#   })
# }
# 
# # 3. Filter successful fits
# successful_fits <- Filter(function(x) x$success, results)
# 
# # 4. Extract RSS values safely
# rss_values <- sapply(successful_fits, function(x) {
#   if (is.numeric(x$rss)) x$rss else NA
# })
# 
# # 5. Identify the best fit
# best_index <- which.min(rss_values)
# best_fit <- successful_fits[[best_index]]
# 
# # 6. Show summary of the best model
# summary(best_fit$fit)


# 
# fit_gately_nls <- nls(
#   ES_pcap ~ gately_model(GDP_PPP_pcap_thousands, d_bar, u_bar, rising_income, falling_income, lag_ES_pcap, gamma_max, lambda, phi, theta_r, theta_f, alpha, beta_i),
#   data = data1,
#   #fixed = a0 + a1 + xmid + scal + b0 + b1 ~ 1,
#   #random = pdDiag(~ 1 | country_name),
#   #random =  xmid ~ 1 | country_name,
#   start = c(gamma_max = 1000, lambda = 0, phi = 0, theta_r = 1, theta_f = 1, alpha = 0.001, beta_i = 0.001),
#   na.action = na.exclude, #na.exclude to retain original number of rows
#   control = nls.control(
#     maxiter = 1000,       # Increase max iterations
#     tol = 1e-6,           # Tolerance for convergence (smaller = more precise)
#     minFactor = 1e-12,    # Minimum step factor (helps with small parameter updates)
#     printEval = TRUE,     # Print evaluation progress (optional, for debugging)
#     warnOnly = TRUE       # Prevents stopping on warnings
#   )
# )


library(minpack.lm)
fit_gately_nlsLM <- nlsLM(
  ES_pcap ~ gately_model(GDP_PPP_pcap_thousands, d_bar, u_bar, rising_income, falling_income, lag_ES_pcap, gamma_max, lambda, phi, theta_r, theta_f, alpha, beta_i),
  data = data1,
  start = c(
    gamma_max = 1000, # If ES in original, set to 1000
    lambda = 0,
    phi = 0,
    theta_r = 1,
    theta_f = 1,
    alpha = 1e-6,
    beta_i = 1e-6
    ),
  # Setting constraints
  # lower = c(
  #   gamma_max = 100,
  #   lambda = -Inf,
  #   phi = -Inf,
  #   theta_r = -Inf,
  #   theta_f = -Inf,
  #   alpha = -Inf,
  #   beta_i = -Inf
  # ),
  na.action =  na.exclude,
  control = nls.lm.control(maxiter = 500)
)

summary(fit_gately_nlsLM)

# copy parameters to clipboard
parameters <- coef(fit_gately_nlsLM)
write.table(parameters, file = "clipboard", sep = "\t", row.names = TRUE, col.names = TRUE)

### diagnostics

# Create a clean dataset used in the model
model_data <- na.omit(data1[, c("ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
                                "rising_income", "falling_income", "lag_ES_pcap")])

# full_model_data <- na.omit(full.dataset[, c("ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
#                                 "rising_income", "falling_income", "lag_ES_pcap")]) %>%
#   filter(!is.na(lag_ES_pcap))
# # Use full dataset
# model_data <- full_model_data

# Add predictions and residuals
model_data$predicted <- predict(fit_gately_nlsLM)
model_data$residuals <- model_data$ES_pcap - model_data$predicted


# 1. Actual vs. Predicted
plot(model_data$ES_pcap, model_data$predicted,
     xlab = "Actual ES_pcap", ylab = "Predicted ES_pcap",
     main = "Actual vs. Predicted ES_pcap",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(a = 0, b = 1, col = "red", lty = 2)

# 2. Residuals vs. Fitted
plot(model_data$predicted, model_data$residuals,
     xlab = "Predicted ES_pcap", ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lty = 2)

# 3. Histogram of Residuals
hist(model_data$residuals,
     breaks = 30,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "lightblue", border = "white")


# fit_gately_nlme <- nlme(
#   ES_pcap ~ gately_model(GDP_PPP_pcap_thousands, d_bar, u_bar, rising_income, falling_income, lag_ES_pcap, gamma_max, lambda, phi, theta_r, theta_f, alpha, beta_i),
#   data = data1,
#   fixed = a0 + a1 + a2 + xmid + scal ~ 1,
#   #random = xmid + a2 ~ 1 | country_name, # This was done
#   random = xmid + scal  ~ 1 | country_name,
#   #random = xmid + a1 ~ 1 | country_name,
#   #random = xmid ~ 1 | country_name,
#   start = c(a0 = 5000, a1 = 10, a2 = 2, xmid = 10000, scal = 1000),
#   #correlation = corARMA(p = 1, q = 2, form = ~ year | country_name),,  # <-- this models autocorrelation
#   na.action = na.exclude, #na.exclude to retain original number of rows
#   control = nlmeControl(pnlsTol = 0.5, maxIter = 500, minFactor = 1e-10, msMaxIter = 500, warnOnly = TRUE)
#   #control = nlmeControl(pnlsTol = 0.1, maxIter = 100)
# )
# 
# summary(fit_gately)


###################################################################################################################################################################


##################################################################################################################
#### Summarizing  model output
# Copy summary table in neatly formatted condition and save it as a .csv file using stargazer, including random effects variance and full model summary
# Install required packages
# install.packages(c("broom.mixed", "kableExtra"))
# library(broom.mixed)
# library(knitr)
# library(kableExtra)
# 
# # Tidy the model
# tidy_fit <- broom.mixed::tidy(fit1)
# 
# # Create a neat table
# kable(tidy_fit, digits = 3, caption = "Summary of nlme Model") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# summary_table <- stargazer(fit_gately_nlsLM, type = "html", title = "Model Summary", out = here::here("results/model_summary_gately.html"))

#   mutate(
#     term = gsub("random\\(Intercept\\)", "Random Intercept", term),
#     term = gsub("fixed\\(Intercept\\)", "Fixed Intercept", term)
#   )
# write.csv(summary_table, here::here("results/summary_table_logistic1.csv"), row.names = FALSE)
# 
# intervals <- intervals(fit1)

################################################################################################
#### Getting Predicted Values and Graphs

create_theme <- function(text_size) {
  theme(
    axis.title.y = element_text(size = text_size, family = "ShellMedium"),
    axis.title.x = element_text(size = text_size, family = "ShellMedium"),
    axis.text.y = element_text(size = text_size - 2, family = "ShellMedium"),
    axis.text.x = element_text(size = text_size - 2, family = "ShellMedium", angle = 45, hjust = 1),
    #plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
    legend.position = c(1.05, 0.9),
    legend.text = element_text(size = text_size - 2, family = "ShellMedium"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "transparent", color = NA),  # Remove black box around legend
    legend.key = element_rect(fill = "transparent", color = NA),  # Remove black box around legend keys
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(), # element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = text_size, family = "ShellMedium", hjust = 0.5),  # Center-align the title
    plot.caption = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, size = text_size - 4, family = "ShellMedium"),
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm"),  # Customize the length of the tick marks
    panel.border = element_rect(color = "black", fill = NA),  # Add a square border inside the axis
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )
}
# Model chosen

fit <- fit_gately_nlsLM



# Create a clean dataset used in the model

# data1a <- data1
# 
# data1a$predicted_ES_pcap <- predict(fit, newdata = data1a)
# Historical fitted values
# Predictions from model:
data1_model <- data1[complete.cases(data1[, c("ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
                                              "rising_income", "falling_income", "lag_ES_pcap")]), ]

# Full dataset of all countries
data1_model <- full.dataset[complete.cases(full.dataset[, c("ES_pcap", "GDP_PPP_pcap_thousands", "d_bar", "u_bar",
                                              "rising_income", "falling_income", "lag_ES_pcap")]), ]

data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model)

#### Check
View(data1_model %>% filter(country_name == "Libya"))
# Step 1: Create a copy of the data used in model fitting (after NA removal)

data1_model$residuals <- data1_model$ES_pcap - data1_model$predicted_ES_pcap

ggplot(data1_model, aes(x = ES_pcap, y = predicted_ES_pcap, color = country_name)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted ES_pcap", x = "Actual", y = "Predicted") +
  theme(legend.position = "none")

ggplot(data1_model, aes(x = year, y = residuals, color = country_name)) +
  geom_point() +
  geom_line() +
  # facet by country
  facet_wrap(~ country_name, scales = "free") +
  labs(title = "Residuals by Year and Country",
       x = "Year",
       y = "Residuals") +
  theme_minimal() +
  theme(legend.position = "none")

####### Displaying Output
# Extract random effects
#random_effects <- ranef(fit)

# Convert to a tidy format
# random_df <- random_effects %>%
#   tibble::rownames_to_column("country_name") %>%
#   tidyr::pivot_longer(-country_name, names_to = "term", values_to = "estimate") %>%
#   mutate(effect_type = "Random")

# Extract fixed effects
fixed_df <- broom::tidy(fit) %>%
  mutate(effect_type = "Fixed")

# Combine both
combined_df <- bind_rows(fixed_df)#, random_df)

# Create a GT table
# gt(combined_df) %>%
#   tab_header(title = "Fixed and Random Effects from Nonlinear Mixed-Effects Model") %>%
#   cols_label(
#     country_name = "Country",
#     term = "Parameter",
#     estimate = "Estimate",
#     effect_type = "Effect Type"
#   ) %>%
#   fmt_number(columns = "estimate", decimals = 3)
# 

# plot predicted versus actual values
ggplot(data1_model, aes(x = year, y = ES_pcap)) +
  geom_point() +
  geom_line() +
  # Add another geom of point and line for predicted_ES_pcap
  geom_point(aes(y = predicted_ES_pcap), color = "red") +
  geom_line(aes(y = predicted_ES_pcap), color = "red") +
  # facet by country
  facet_wrap(~ country_name, scales = "free") +
  #geom_abline(slope = 1, intercept = 0, color = "red") +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Predicted vs Actual Energy Service per Capita",
       x = "Predicted Energy Service per Capita",
       y = "Actual Energy Service per Capita") +
  theme_minimal()


# Including random effects for known countries
future_data <- data %>%
  filter(#country_id <= 15 & 
    year >= 2024)

head(future_data)

### Want all data

predictions_data <- data %>%#data %>%
  # Set lag_ES_pcap to NA for year >= 2024
  mutate(lag_ES_pcap = ifelse(year >= 2024, NA, lag_ES_pcap))

# Initialize
predictions <- list()
data.orig <- data.frame(predictions_data)  # Replace with your actual data
model <- fit_gately_nlsLM  # fitted nlsLM model

#######################################################################################################################
for (i in 2024:2100) {
  # Update lag_ES_pcap only for the current year
  data.orig <- data.orig %>%
    group_by(country_id) %>%
    arrange(year) %>%
    mutate(lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)) %>%
    ungroup()
  
  # Filter data for the current year
  data.used <- filter(data.orig, year == i)
  
  # Predict using the model
  predictions <- predict(model, newdata = data.used)
  prediction_df <- data.frame(
    country_id = data.used$country_id,
    year = data.used$year,
    predicted_ES_pcap = predictions
  )
  
  # Update ES_pcap in data.orig with predictions
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


predictions_data <- data.orig %>%
  arrange(country_id, year)




#########
### This worked
# for (i in 2024:2100) {
#   data.orig <- data.orig %>%
#     group_by(country_id) %>%
#     arrange(country_id, year) %>%
#     mutate(lag_ES_pcap = case_when(year == i ~ dplyr::lag(ES_pcap), TRUE ~ lag_ES_pcap)) %>%
#     ungroup()
# 
#   data.used <- data.orig %>% filter(year == i)
# 
#   # Predict lg.trns.LFPR for year i
#   predictions.loop <- predict(model, newdata = data.used)
#   prediction_df <- data.frame(predictions.loop, data.used[c("country_id", "year")])#data.frame(predictions.loop, attr(predictions.loop, "index"))
# 
#   prediction.data <- merge(data.orig, prediction_df, by = c("country_id", "year"), all.x = TRUE) %>%
#     group_by(country_id) %>%
#     arrange(country_id, year) %>%
#     #mutate(offset = ES_pcap - predictions.loop) %>%
#     mutate(
#       ES_pcap = case_when(year == i ~ predictions.loop, TRUE ~ ES_pcap),
#       lag_ES_pcap = case_when(year == i ~ dplyr::lag(ES_pcap), TRUE ~ lag_ES_pcap)
#     ) %>%
#     select(-predictions.loop) %>%
#     ungroup()
# 
#   # Update data.orig with prediction.data
#   data.orig <- prediction.data
# 
#   # Store the prediction data in the list
#   #predictions[[i - 2023]] <- prediction.data
# }
# 
# predictions_data <- data.orig#bind_rows(predictions)
### up to here
#future_data$predicted_ES_pcap <- predict(fit, newdata = future_data) #, level = 1) # level = 1 to incorporate random effects

# Plot predicted values for future years and historical data faceted by country
# ggplot() +
#   # Historical data (solid line)
#   geom_point(data = data1, aes(x = year, y = ES_pcap, color = country_name)) +
#   geom_line(data = data1, aes(x = year, y = ES_pcap, color = country_name, linetype = "Historical")) +
#   
#   # Future predictions (dashed line)
#   geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
#   geom_line(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name, linetype = "Predicted")) +
#   
#   facet_wrap(~ country_name, scales = "fixed") +
#   scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
#   labs(title = "Predicted Energy Service per Capita for Future Years",
#        x = "Year",
#        y = "Energy Service per Capita",
#        linetype = "Data Type") +
#   theme_minimal() +
#   theme(legend.position = "none")

# Plot predicted values for future years and historical data faceted by country
# ggplot() +
#   # Historical data (solid line)
#   geom_point(data = data1_model, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
#   geom_line(data = data1_model, aes(x = year, y = predicted_ES_pcap, color = country_name, linetype = "Historical")) +
#   
#   # Future predictions (dashed line)
#   geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
#   geom_line(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name, linetype = "Predicted")) +
#   
#   facet_wrap(~ country_name, scales = "fixed") +
#   scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
#   labs(title = "Predicted Energy Service per Capita for Future Years",
#        x = "Year",
#        y = "Energy Service per Capita",
#        linetype = "Data Type") +
#   theme_minimal() +
#   theme(legend.position = "none")


### theme for plotting
# create_theme <- function(rel.size = 1) {
#   theme(
#     axis.title.y = element_text(size = 18 * rel.size, family = "ShellMedium"),
#     axis.title.x = element_text(size = 18 * rel.size, family = "ShellMedium"),
#     axis.text.y = element_text(size = 16 * rel.size, family = "ShellMedium"),
#     axis.text.x = element_text(size = 16 * rel.size, family = "ShellMedium", angle = 45, hjust = 1),
#     plot.margin = margin(l = 5, r = 5, t = 5, b = 5),
#     legend.title = element_text(family = "ShellMedium", size = 16 * rel.size),
#     legend.position = "top",
#     legend.text = element_text(size = 15 * rel.size, family = "ShellMedium"),
#     legend.background = element_rect(fill = "white", color = "black"),
#     plot.background = element_rect(fill = "white"),
#     panel.background = element_rect(fill = "white"),
#     panel.grid.major = element_line(color = "gray", linetype = "dashed"),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 18 * rel.size, family = "ShellMedium", hjust = 0.5),
#     plot.caption = element_blank(),
#     plot.subtitle = element_blank(),
#     strip.text = element_text(size = 16 * rel.size, family = "ShellMedium"),
#     strip.background = element_rect(fill = "lightgray", color = "black"),
#     panel.border = element_blank()
#     
#   )
# }

future_data <- predictions_data %>%
  filter(year >= 2024) %>%
  mutate(
    predicted_ES_pcap = ES_pcap
  )


################################################################################################################################
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

selected.libya <- future_data %>% filter(country_name == "Libya") %>%
  select(country_name, year, d_bar, u_bar, rising_income, falling_income, lag_ES_pcap, GDP_PPP_pcap_thousands, ES_pcap)
# Copy selected.libya to clipboard
write.table(selected.libya, file = "clipboard", sep = "\t", row.names = FALSE, col.names = TRUE)

# Incorporate actual Libya data etc.
selected1 <- data1_model %>% ungroup() %>% select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex) 
selected2 <- future_data %>% ungroup() %>% select(country_name, GDP_PPP_pcap, ES_pcap, year, Hex)
  
aggregated.data <- rbind(selected1, selected2)  %>%
  ungroup() %>%
  arrange(country_name, year) %>%
  mutate(line_type = if_else(year <= 2023, "Historical", "Predicted"))

# Split the data
historical_data <- aggregated.data %>% filter(line_type == "Historical")
predicted_data  <- aggregated.data %>% filter(line_type == "Predicted")


# Create a named vector of colors
country_colors <- historical_data %>%
  select(country_name, Hex) %>%
  distinct()

country_colors <- setNames(country_colors$Hex, country_colors$country_name)
# creates a named vector: names = country_name, values = Hex

p1 <- ggplot() +
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
    title = "Predicted Energy Service per Capita using Gately Model",
    x = "GDP per capita (USD)",
    y = "Energy Service per Capita (passenger km / capita)",
    color = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  create_theme(16) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

# Save to interactive html widget
ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  #layout(title = "Predicted Energy Service per Capita using Additive Logistic Model",
  #       xaxis = list(title = "GDP per capita (USD)"),
  #       yaxis = list(title = "Energy Service per Capita (passenger km / capita)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_ES_pcap_gately.html"), selfcontained = TRUE)

ggsave(here::here("plots/predicted_ES_pcap_gately.png"), plot = p1, width = 16, height = 16, dpi = 150)







