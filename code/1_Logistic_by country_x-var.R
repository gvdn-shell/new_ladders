# Remove all objects in R workspace (use with caution)
rm(list = ls())

# Check that required packages are installed and install if missing
ncpus <- 12
packages <- c("dplyr",
              "minpack.lm",
              "tidyr",
              "purrr",
              "ggplot2",
              "nlme") #
installed_packages <- packages %in% rownames(installed.packages())

if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load required packages
invisible(lapply(packages, library, character.only = TRUE))


data <- readRDS("data/all_data_wem_espcap_gapfill.rds")

# Define the S-curve function with additional variables
Scurve <- function(x, Asym, xmid, scal) {
  result <- Asym / (1 + exp(-scal * (x - xmid)))
  return(result)
}

Scurve2 <- function(x, gini_index, Asym, xmid, scal, c, b) {
  result <- (c + b * gini_index) / (1 + exp(-scal * (x - xmid)))
  return(result)
}

# gompertz <- function(t, a, b, c, d, e, pop_density, gini_index) {
#   a * exp(-b * exp(-c * t)) + d * pop_density + e * gini_index
# }

gompertz <- function(t, a, b, c, pop_density, gini_index) {
  a * exp(-b * exp(-c * t * pop_density)) ^ gini_index
}


# Filter data for country_id <= 15 and befpre 2024 as historical data
data1 <- data %>%
  filter(country_id <= 15 & year <= 2024)

# Impute missing values with the mean of the respective columns by country
# data <- data %>%
#   group_by(country_name) %>%
#   mutate(
#     pop_density = ifelse(is.na(density_psqkm), mean(density_psqkm, na.rm = TRUE), density_psqkm),
#     gini_index = ifelse(is.na(Gini), mean(Gini, na.rm = TRUE), Gini)
#   ) %>%
#   ungroup()

# Use linear model by country_id to predict missing values for Gini, density_psqkm and urbanization_perc
data1 <- data1 %>%
  group_by(country_name) %>%
  mutate(
    Gini = ifelse(is.na(Gini) & year >= 2000, predict(lm(Gini ~ poly(year, 1)), newdata = data.frame(year = year)), Gini),
    density_psqkm = ifelse(is.na(density_psqkm) & year >= 2000, predict(lm(density_psqkm ~ poly(year, 1)), newdata = data.frame(year = year)), density_psqkm),
    urbanization_perc = ifelse(is.na(urbanization_perc) & year >= 2000, predict(lm(urbanization_perc ~ poly(year, 1)), newdata = data.frame(year = year)), urbanization_perc)
  ) %>%
  ungroup()

# Plot the values for Gini, density_psqkm and urbanization_perc by country and year (facet plot with 3 facets)
ggplot(data1, aes(x = year)) +
  geom_line(aes(y = Gini, color = country_name)) +
#  facet_wrap(~ country_name, scales = "free_y") +
  labs(title = "Gini Index by Country and Year",
       x = "Year",
       y = "Gini Index") +
  theme_minimal()
ggplot(data1, aes(x = year)) +
  geom_line(aes(y = density_psqkm, color = country_name)) +
#  facet_wrap(~ country_name, scales = "free_y") +
  labs(title = "Population Density by Country and Year",
       x = "Year",
       y = "Population Density (people per sq km)") +
  theme_minimal()
ggplot(data1, aes(x = year)) +
  geom_line(aes(y = urbanization_perc, color = country_name)) +
#  facet_wrap(~ country_name, scales = "free_y") +
  labs(title = "Urbanization Percentage by Country and Year",
       x = "Year",
       y = "Urbanization Percentage") +
  theme_minimal()


data1 <- data1 %>%
  group_by(country_name) %>%
  mutate(Year0 = year - min(year)) %>%
  ungroup()

# Fit model by country by using Scurve function above and nlsLM function or similar
# Fit the model using nlsLM for each country
# Note: nlsLM is from the minpack.lm package
# Note: The model fitting process may take a while depending on the size of the dataset
# Note: The model fitting process may take a while depending on the size of the dataset

Scurve2 <- function(x, gini_index, Asym, xmid, scal, c, b) {
  result <- (c + b * gini_index) / (1 + exp(-scal * (x - xmid)))
  return(result)
}

# Plot GDP_PPP_pcap vs ES_pcap (y-axis) for each country_name
ggplot(data1, aes(x = GDP_PPP_pcap, y = ES_pcap, color = country_name)) +
  geom_point() +
  # facet by country
  facet_wrap(~ country_name, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per Capita vs Energy Service per Capita",
       x = "GDP per Capita",
       y = "Energy Service per Capita") +
  theme_minimal()

ggplot(data1, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_point() +
  # facet by country
  facet_wrap(~ country_name, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Year vs Energy Service per Capita",
       x = "Year",
       y = "Energy Service per Capita") +
  theme_minimal()

ggplot(data1, aes(x = year, y = GDP_PPP_pcap, color = country_name)) +
  geom_point() +
  # facet by country
  facet_wrap(~ country_name, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Year vs GDP per Capita",
       x = "Year",
       y = "GDP per Capita") +
  theme_minimal()

### https://stats.stackexchange.com/questions/205918/how-to-use-a-sigmoidal-function-in-a-multiple-nonlinear-regression

# Define the nonlinear model function
logistic_model <- deriv(
  ~ Asym / (1 + exp((xmid - GDP_PPP_pcap) / scal)),
  namevec = c("Asym", "xmid", "scal"),
  function.arg = c("GDP_PPP_pcap", "Asym", "xmid", "scal")
)

# Fit the model
fit <- nlme(
  ES_pcap ~ logistic_model(GDP_PPP_pcap, Asym, xmid, scal),
  data = data1,
  fixed = Asym + xmid + scal ~ 1,
  #random = Asym + xmid + scal ~ 1 | country_name, # Allows for Asym, xmid and scal to vary by country
  random = Asym ~ 1 | country_name,
  start = c(Asym = 5000, xmid = 10000, scal = 1000),
  na.action = na.omit,
  control = nlmeControl(pnlsTol = 0.1, maxIter = 100)
)

summary(fit)

data1$predicted_ES_pcap <- predict(fit)


data1$residuals <- data1$ES_pcap - data1$predicted_ES_pcap

# plot predicted versus actual values
ggplot(data1, aes(x = year, y = ES_pcap)) +
  geom_point() +
  geom_line() +
  # Add another geom of point and line for predicted_ES_pcap
  geom_point(aes(y = predicted_ES_pcap), color = "red") +
  geom_line(aes(y = predicted_ES_pcap), color = "red") +
  # facet by country
  facet_wrap(~ country_name, scales = "free") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Predicted vs Actual Energy Service per Capita",
       x = "Predicted Energy Service per Capita",
       y = "Actual Energy Service per Capita") +
  theme_minimal()


# Including random effects for known countries
future_data <- data %>%
  filter(country_id <= 15 & year > 2024)

head(future_data)

future_data$predicted_ES_pcap <- predict(fit, newdata = future_data, level = 1) # level = 1 to incorporate random effects

# Plot predicted values for future years and historical data faceted by country
ggplot() +
  # Historical data (solid line)
  geom_point(data = data1, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_line(data = data1, aes(x = year, y = ES_pcap, color = country_name, linetype = "Historical")) +
  
  # Future predictions (dashed line)
  geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
  geom_line(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name, linetype = "Predicted")) +
  
  facet_wrap(~ country_name, scales = "fixed") +
  scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
  labs(title = "Predicted Energy Service per Capita for Future Years",
       x = "Year",
       y = "Energy Service per Capita",
       linetype = "Data Type") +
  theme_minimal()

# Plot predicted values for future years and historical data faceted by country
ggplot() +
  # Historical data (solid line)
  geom_point(data = data1, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
  geom_line(data = data1, aes(x = year, y = predicted_ES_pcap, color = country_name, linetype = "Historical")) +
  
  # Future predictions (dashed line)
  geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
  geom_line(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name, linetype = "Predicted")) +
  
  facet_wrap(~ country_name, scales = "fixed") +
  scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
  labs(title = "Predicted Energy Service per Capita for Future Years",
       x = "Year",
       y = "Energy Service per Capita",
       linetype = "Data Type") +
  theme_minimal()

ggplot() +
  # Historical data
  #geom_point(data = data1, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_line(data = data1, aes(x = year, y = ES_pcap, color = country_name, linetype = "Historical", group = country_name)) +
  
  # Future predictions
  #geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
  geom_line(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name, linetype = "Predicted", group = country_name)) +
  
  facet_wrap(~ country_name, scales = "fixed") +
  scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
  labs(title = "Predicted Energy Service per Capita for Future Years",
       x = "Year",
       y = "Energy Service per Capita",
       linetype = "Data Type") +
  theme_minimal()


# Making adjustments
# Incorporate last historical value as baseline

# baseline <- data %>%
#   filter(country_id <= 15 & year == 2024) %>%
#   select(country_name, GDP_PPP_pcap, ES_pcap_2024 = ES_pcap)
# 
# 
# # Get 2025 predicted values to compute adjustment
# first_pred <- future_data %>%
#   filter(country_id <= 15 & year == 2025) %>%
#   select(country_name, GDP_PPP_pcap, predicted_2025 = predicted_ES_pcap)
# 
# 
# # Join and compute adjustment
# adjustments <- baseline %>%
#   inner_join(first_pred, by = "country_name") %>%
#   mutate(adjustment = ES_pcap_2024 - predicted_2025)
# 
# 
# # Apply adjustment to all future years
# future_data <- future_data %>%
#   left_join(adjustments %>% select(country_name, adjustment), by = "country_name") %>%
#   mutate(adjusted_ES_pcap = predicted_ES_pcap + adjustment)

#############################################################################################
# Model diagnostics

data1$fitted <- fitted(fit)
data1$residuals <- residuals(fit)

# Residuals versus fitted plot


ggplot(data1, aes(x = fitted, y = residuals, color = country_name)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ country_name, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()



qqnorm(data1$residuals)
qqline(data1$residuals, col = "red")


# Fitted versus observed plot

ggplot(data1, aes(x = fitted, y = ES_pcap, color = country_name)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ country_name, scales = "free") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Fitted vs Observed Values",
       x = "Fitted Values",
       y = "Observed ES_pcap") +
  theme_minimal()

# Breusch-Pagan test for Heteroscedasticity

bp_test <- lm(residuals^2 ~ fitted, data = data1)
summary(bp_test)


# Evidence of heteroskedasticity
fit_weighted <- update(fit, weights = varPower())


AIC(fit, fit_weighted)
BIC(fit, fit_weighted)

# Extract residuals and fitted values


data1$fitted_original <- fitted(fit)
data1$residuals_original <- resid(fit)
# 
# data1$fitted_weighted <- fitted(fit_weighted)
# data1$residuals_weighted <- resid(fit_weighted)


# Extract the data actually used in the model
model_data <- getData(fit_weighted)

# Add fitted and residuals to that data
model_data$fitted_weighted <- fitted(fit_weighted)
model_data$residuals_weighted <- resid(fit_weighted)

model_data$row_id <- as.integer(rownames(model_data))
data1$row_id <- as.integer(rownames(data1))

data1 <- left_join(data1, model_data %>% select(row_id, fitted_weighted, residuals_weighted), by = "row_id")

# Original
p1 <- ggplot(data1, aes(x = fitted_original, y = residuals_original)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ country_name, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (Original Model)")

# Weighted
p2 <- ggplot(data1, aes(x = fitted_weighted, y = residuals_weighted)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ country_name, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (Weighted Model)")

# Display side by side
library(patchwork)
p1 + p2


qqnorm(data1$residuals_original); qqline(data1$residuals_original, col = "red")
qqnorm(data1$residuals_weighted); qqline(data1$residuals_weighted, col = "red")



#############################################################################################



models <- data %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      mod <- nlsLM(ES_pcap ~ SSlogis(GDP_PPP_pcap, Asym, xmid, scal),
                   data = .x#,
                   #start = list(Asym=5000, xmid=1, scal=0.1)
                   )
      list(model = mod, data = .x)
    }, error = function(e) {
      message("Model failed for ", unique(.x$country_name), ": ", e$message)
      NULL
    })
  }, .keep = TRUE)

data %>% filter(country_name == "Russia") %>% summary()


start_vals <- list(
  Asym = max(.x$ES_pcap, na.rm = TRUE),
  xmid = median(.x$GDP_PPP_pcap, na.rm = TRUE),
  scal = sd(.x$GDP_PPP_pcap, na.rm = TRUE)
)


models <- data %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      xmid_start <- median(.x$GDP_PPP_pcap, na.rm = TRUE)
      scal_start <- 0.1
      c_start <- min(.x$ES_pcap, na.rm = TRUE)
      b_start <- (max(.x$ES_pcap, na.rm = TRUE) - c_start) / max(.x$Gini, na.rm = TRUE)
      
      mod <- nlsLM(ES_pcap ~ (c + b * Gini) / (1 + exp(-scal * (GDP_PPP_pcap - xmid))),
                   data = .x,
                   start = list(xmid = xmid_start, scal = scal_start, c = c_start, b = b_start))
      list(model = mod, data = .x)
    }, error = function(e) {
      message("Model failed for ", unique(.x$country_name), ": ", e$message)
      NULL
    })
  }, .keep = TRUE)



models <- data %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      mod <- nlsLM(ES_pcap ~ (c + b * Gini) / (1 + exp(-scal * (GDP_PPP_pcap - xmid))),
                   data = .x,
                   start = list(xmid = 1, scal = 0.1, c = 0.1, b = 0.1))
      list(model = mod, data = .x)
    }, error = function(e) {
      message("Model failed for ", unique(.x$country_name), ": ", e$message)
      NULL
    })
  }, .keep = TRUE)


models <- data %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      mod <- nlsLM(ES_pcap ~ Scurve2(x = GDP_PPP_pcap, gini_index = Gini, Asym, xmid, scal, c, b),
                   data = .x,
                   start = list(Asym=5000, xmid=1, scal=0.1, c=0.1, b=0.1))
      list(model = mod, data = .x)
    }, error = function(e) NULL)
  }, .keep = TRUE)

models <- data %>%
  group_by(country_name) %>%
  group_map(~{
    tryCatch({
      mod <- nlsLM(ES_pcap ~ a * exp(-b * exp(-c * t * pop_density)) ^ gini_index,
                   data = .x,
                   start = list(a=5000, b=1, c=0.1))
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
    gini_index = mean(data$gini_index) * (1 + 0.01)^(new_years - last_year) # Assuming a 1% annual increase
  )
  new_data$ES_pcap <- predict(model, newdata = new_data)
  new_data
})


# Combine original and predicted data
combined <- data %>%
  select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm, Gini) %>%
  mutate(Source = "Observed") %>%
  bind_rows(
    predictions %>%
      left_join(data %>% group_by(country_name) %>%
                  summarise(last_GDP = last(GDP_PPP_pcap)), by = "country_name") %>%
      mutate(
        GDP_PPP_pcap = last_GDP * (1 + 0.03)^(year - max(data$year)),
        Source = "Predicted"
      ) %>%
      select(country_name, year, ES_pcap, GDP_PPP_pcap, density_psqkm, Gini, Source)
  )


# Plot historical and predicted data
ggplot(combined, aes(x = GDP_PPP_pcap, y = energy_service, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  labs(title = "Energy Service Over Time",
       x = "GDP per Capita",
       y = "Energy Service",
       color = "Country") +
  theme_minimal()

# Plot historical and predicted data Es vs Year
ggplot(combined, aes(x = year, y = energy_service, color = country_name)) +
  geom_line(data = combined %>% filter(Source == "Observed"), size = 1) +
  geom_line(data = combined %>% filter(Source == "Predicted"), size = 1, linetype = "dotted") +
  labs(title = "Energy Service Over Time",
       x = "Year",
       y = "Energy Service",
       color = "Country") +
  theme_minimal()
