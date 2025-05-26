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


data <- readRDS("data/all_data_wem_espcap_imputation.rds")

shell.brand.palette <- readRDS(here::here("data", "shell_brand_palette.rds"))

data <- shell.brand.palette %>%
  mutate(Hex = paste0("#", Hex)) %>%
  mutate(country_id = row_number()) %>% 
  left_join(data , by = "country_id") #%>%
  #distinct()


#####

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

# Filter out countries with less than 2 data points for Gini, urbanization_perc and density_psqkm
set.seed(1234)
data <- data %>%
  group_by(country_name) %>%
  filter(
    sum(!is.na(Gini)) >= 2,
    sum(!is.na(urbanization_perc)) >= 2,
    sum(!is.na(density_psqkm)) >= 2
  ) %>%
  ungroup() %>%
  # Add random noise since nls doesn't hanndle zero-residual data well.
  mutate(Gini_noise = Gini + rnorm(1, mean = 0, sd = 0.01))

### Interpolate missing values
# 
# data <- data %>%
#   group_by(country_name) %>%
#   mutate(
#     # Impute Gini, clamped between 0 and 100
#     Gini = ifelse(
#       is.na(Gini) & (year >= 2000 & year < 2024),
#       pmin(100, pmax(0, predict(lm(Gini ~ poly(year, 1)), newdata = data.frame(year = year)))),
#       Gini
#     ),
#     
#     # Impute density_psqkm (no clamping unless you want to restrict it)
#     density_psqkm = ifelse(
#       is.na(density_psqkm) & (year >= 2000 & year < 2024),
#       predict(lm(density_psqkm ~ poly(year, 1)), newdata = data.frame(year = year)),
#       density_psqkm
#     ),
#     
#     # Impute urbanization_perc, clamped between 0 and 100
#     urbanization_perc = ifelse(
#       is.na(urbanization_perc) & (year >= 2000 & year < 2024),
#       pmin(100, pmax(0, predict(lm(urbanization_perc ~ poly(year, 1)), newdata = data.frame(year = year)))),
#       urbanization_perc
#     )
#   ) %>%
#   ungroup()


# Group by country_name, find last year with missing for each of Gini, urbanization_perc and density_psqkm
# and then project forward into time by decreasing Gini for 0.01% per year, urbanization_perc by 0.1% and density_psqkm by 0.5%
# but ensuring that Gini and urbanization_perc stay in range [0,100] and density_psqkm is positive
# data <- data %>%
#   group_by(country_name) %>%
#   mutate(
#     Gini = ifelse(
#       is.na(Gini) & !all(is.na(Gini)) & year > max(year[!is.na(Gini)]),
#       pmax(0, pmin(100, last(Gini[!is.na(Gini)]) - 0.01 * (year - max(year[!is.na(Gini)])))),
#       Gini
#     ),
#     urbanization_perc = ifelse(
#       is.na(urbanization_perc) & !all(is.na(urbanization_perc)) & year > max(year[!is.na(urbanization_perc)]),
#       pmax(0, pmin(100, last(urbanization_perc[!is.na(urbanization_perc)]) + 0.1 * (year - max(year[!is.na(urbanization_perc)])))),
#       urbanization_perc
#     ),
#     density_psqkm = ifelse(
#       is.na(density_psqkm) & !all(is.na(density_psqkm)) & year > max(year[!is.na(density_psqkm)]),
#       pmax(0, last(density_psqkm[!is.na(density_psqkm)]) + 0.5 * (year - max(year[!is.na(density_psqkm)]))),
#       density_psqkm
#     )
#   ) %>%
#   ungroup()



# Historical data
data1 <- data %>%
  filter(year <= 2024)

# Impute missing values with the mean of the respective columns by country
# data <- data %>%
#   group_by(country_name) %>%
#   mutate(
#     pop_density = ifelse(is.na(density_psqkm), mean(density_psqkm, na.rm = TRUE), density_psqkm),
#     gini_index = ifelse(is.na(Gini), mean(Gini, na.rm = TRUE), Gini)
#   ) %>%
#   ungroup()

#View(data1 %>% filter(country_name == "Qatar"))

# Use linear model by country_id to predict missing values for Gini, density_psqkm and urbanization_perc


############## Data wrangling
### Generating forecasts for key independent variables

# forecast_variable <- function(df, country_col, year_col, value_col, forecast_to = 2100, clamp_0_100 = FALSE) {
#   df <- df %>%
#     rename(country = {{country_col}}, year = {{year_col}}, value = {{value_col}}) %>%
#     filter(!is.na(value)) %>%
#     mutate(ds = as.Date(paste0(year, "-01-01")), y = value) %>%
#     select(country, ds, y)
#   
#   # Nest by country
#   nested <- df %>%
#     group_by(country) %>%
#     nest()
#   
#   # Apply Prophet per country
#   nested <- nested %>%
#     mutate(
#       model = map(data, ~ prophet(.x)),
#       future = map2(model, data, ~ make_future_dataframe(.x, periods = forecast_to - max(as.numeric(format(.y$ds, "%Y"))), freq = "year")),
#       forecast = map2(model, future, predict)
#     )
#   
#   # Extract and clean forecasts
#   forecasts <- nested %>%
#     select(country, forecast) %>%
#     unnest(forecast) %>%
#     mutate(
#       year = as.numeric(format(ds, "%Y")),
#       yhat = if (clamp_0_100) pmin(100, pmax(0, yhat)) else yhat
#     ) %>%
#     select(country, year, yhat)
#   
#   return(forecasts)
# }
# 
# # Forecast Gini, density_psqkm and urbanization_perc
# gini_forecast <- forecast_variable(data1, country_name, year, Gini, forecast_to = 2100, clamp_0_100 = TRUE)
# 
# ggplot(gini_forecast, aes(x = year, y = yhat, color = country)) +
#   geom_line() +
#   labs(title = "Forecasted Gini Index by Country",
#        x = "Year",
#        y = "Gini Index") +
#   theme_minimal()

# #################
# forecast_variable <- function(df, country_col, year_col, value_col, forecast_to = 2100, 
#                               clamp_0_100 = FALSE, type = c("sigmoid", "exp"), decreasing = FALSE) {
#   type <- match.arg(type)
#   
#   df <- df %>%
#     rename(country = {{country_col}}, year = {{year_col}}, value = {{value_col}}) %>%
#     filter(!is.na(value))
#   
#   if (nrow(df) < 3) return(NULL)  # Not enough data to fit a model
#   
#   last_year <- max(df$year, na.rm = TRUE)
#   future_years <- (last_year + 1):forecast_to
#   if (length(future_years) == 0) return(NULL)  # No future years to forecast
#   
#   # Fit model
#   model <- tryCatch({
#     if (type == "sigmoid") {
#       fit_sigmoid(df, value_col = "value", year_col = "year", decreasing = decreasing)
#     } else {
#       fit_exponential(df, value_col = "value", year_col = "year")
#     }
#   }, error = function(e) return(NULL))
#   
#   if (is.null(model)) return(NULL)
#   
#   # Predict
#   preds <- tryCatch({
#     forecast_from_model(model, future_years, type = type, clamp = clamp_0_100)
#   }, error = function(e) return(NULL))
#   
#   tibble(
#     country = unique(df$country),
#     year = future_years,
#     !!value_col := preds
#   )
# }
# 
# # Forecast Gini, density_psqkm and urbanization_perc
# 
# # Wrapper to forecast all three variables for one country
# forecast_all_variables <- function(df_country, forecast_to = 2100) {
#   country <- unique(df_country$country_name)
#   
#   safe_forecast <- function(var, type, clamp = FALSE, decreasing = FALSE) {
#     if (!var %in% names(df_country)) return(NULL)
#     var_sym <- rlang::sym(var)
#     forecast_variable(df_country, country_name, year, var_sym, 
#                       type = type, clamp_0_100 = clamp, decreasing = decreasing)
#   }
#   
#   gini <- safe_forecast("Gini", type = "sigmoid", clamp = TRUE, decreasing = TRUE)
#   urban <- safe_forecast("urbanization_perc", type = "sigmoid", clamp = TRUE, decreasing = FALSE)
#   density <- safe_forecast("density_psqkm", type = "exp", clamp = FALSE)
#   
#   forecasts <- list(gini, urban, density) %>%
#     discard(is.null)
#   
#   if (length(forecasts) == 0) return(NULL)
#   
#   reduce(forecasts, full_join, by = c("country", "year")) %>%
#     rename(country_name = country)
# }
# 
# 
# 
# 
# # Split data by country
# country_data_list <- data1 %>%
#   group_by(country_name) %>%
#   group_split()
# 
# # Apply forecasting to each country
# all_forecasts <- map_dfr(country_data_list, forecast_all_variables)
# 
# # Combine original and forecasted data
# data_combined <- data1 %>%
#   full_join(all_forecasts, by = c("country_name", "year")) %>%
#   mutate(
#     Gini = coalesce(Gini.x, Gini.y),
#     urbanization_perc = coalesce(urbanization_perc.x, urbanization_perc.y),
#     density_psqkm = coalesce(density_psqkm.x, density_psqkm.y)
#   ) %>%
#   select(-ends_with(".x"), -ends_with(".y")) %>%
#   arrange(country_name, year)
# 
# ggplot(data_combined, aes(x = year)) +
#   geom_line(aes(y = Gini, color = country_name)) +
#   geom_line(aes(y = Gini.y, color = "Forecasted Gini"), linetype = "dashed") +
#   labs(title = "Gini Index by Country and Year",
#        x = "Year",
#        y = "Gini Index") +
#   theme_minimal()


##########################################################################


##########################################################################################

# Plot the values for Gini, density_psqkm and urbanization_perc by country and year (facet plot with 3 facets)
ggplot(data, aes(x = year)) +
  geom_line(aes(y = Gini, color = country_name)) +
#  facet_wrap(~ country_name, scales = "free_y") +
  labs(title = "Gini Index by Country and Year",
       x = "Year",
       y = "Gini Index") +
  theme_minimal()

ggplot(data, aes(x = year)) +
  geom_line(aes(y = density_psqkm, color = country_name)) +
#  facet_wrap(~ country_name, scales = "free_y") +
  labs(title = "Population Density by Country and Year",
       x = "Year",
       y = "Population Density (people per sq km)") +
  theme_minimal()

ggplot(data, aes(x = year)) +
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
  #facet_wrap(~ country_name, scales = "free") +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per Capita vs Energy Service per Capita",
       x = "GDP per Capita",
       y = "Energy Service per Capita") +
  # Hide legend
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(data1, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_point() +
  # facet by country
  #facet_wrap(~ country_name, scales = "free") +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Year vs Energy Service per Capita",
       x = "Year",
       y = "Energy Service per Capita") +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(data1, aes(x = year, y = GDP_PPP_pcap, color = country_name)) +
  geom_point() +
  # facet by country
  #facet_wrap(~ country_name, scales = "free") +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Year vs GDP per Capita",
       x = "Year",
       y = "GDP per Capita") +
  theme_minimal() +
  theme(legend.position = "none") 
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

p1 <- ggplot(data1, aes(y = ES_pcap, x = GDP_PPP_pcap, colour = Hex)) +
  geom_point(aes(group = country_id)) +
  geom_line(aes(group = country_id)) +
  theme_bw() + create_theme1(2) +
  geom_text(data = subset(data1, !duplicated(country_name, fromLast = TRUE)),
            aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
            size = rel(8)) +
  theme(legend.position = "none") +
  labs(title = "Aggregate Energy Service Ladder: Passenger Transport Road Energy Service vs GDP / capita", 
       x = "GDP PPP / capita (US$ 2018)", y = "Energy Service  (vehicle km/ capita/ year)") +
  scale_color_identity() #+
  # Add Gompertz line and add label saying Gompertz Model fit
  #geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
#annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
#         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 
p1
# Save p1 to png
ggsave(filename = here::here("plots", "pt_road_gdp_es.png"), plot = p1, width = 10, height = 6.3, dpi = 250)

p1 <- ggplot(data1, aes(y = Gini, x = GDP_PPP_pcap, colour = Hex)) +
  geom_point(aes(group = country_id)) +
  geom_line(aes(group = country_id)) +
  theme_bw() + create_theme1(2) +
  geom_text(data = subset(data1, !duplicated(country_name, fromLast = TRUE)),
            aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
            size = rel(8)) +
  theme(legend.position = "none") +
  labs(title = "Visualling the Historical Relationship between Gini and GDP / capita", 
       x = "GDP PPP / capita (US$ 2018)", y = "Income inequality (Gini)") +
  scale_color_identity() #+
# Add Gompertz line and add label saying Gompertz Model fit
#geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
#annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
#         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 
p1
# Save p1 to png
ggsave(filename = here::here("plots", "gini_gdp.png"), plot = p1, width = 10, height = 6.3, dpi = 250)


### https://stats.stackexchange.com/questions/205918/how-to-use-a-sigmoidal-function-in-a-multiple-nonlinear-regression

# Define the non-linear model function
logistic_model <- deriv(
  ~ Asym / (1 + exp((xmid - GDP_PPP_pcap) / scal)),
  namevec = c("Asym", "xmid", "scal"),
  function.arg = c("GDP_PPP_pcap", "Asym", "xmid", "scal")
)

multiple_logistic_model <- deriv(
  ~ (a0 + a1 * density_psqkm) / (1 + exp((xmid - GDP_PPP_pcap) / scal)),
  namevec = c("a0", "a1", "xmid", "scal"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "a0", "a1", "xmid", "scal")
)

multiple_logistic_model2 <- deriv(
  ~ (a0 + a1 * density_psqkm) * (1 / (1 + exp((xmid - GDP_PPP_pcap) / scal))) ^ (g0 + g1 * Gini),
  namevec = c("a0", "a1", "xmid", "scal", "g0","g1"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "a0", "a1", "xmid", "scal", "g0","g1","Gini")
)

multiple_logistic_model3 <- deriv(
  ~ (a0 + a1 * density_psqkm) * (1 / (1 + exp((xmid - GDP_PPP_pcap) / scal))) ^ (g1 * Gini),
  namevec = c("a0", "a1", "xmid", "scal", "g1"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "a0", "a1", "xmid", "scal", "g1","Gini")
)

multiple_logistic_model4 <- deriv(
  ~ (a0 + a1 * density_psqkm) * (1 / (1 + exp((xmid - GDP_PPP_pcap) / scal))) ^ (g0 + g1 * Gini),
  namevec = c("a0", "a1", "xmid", "scal", "g0", "g1"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "a0", "a1", "xmid", "scal", "g0", "g1", "Gini")
)

multiple_logistic_model5 <- deriv(
  ~ Asym * (1 / (1 + exp((xmid - GDP_PPP_pcap) / scal))) * exp(g0 * exp (g1 * Gini)),
  namevec = c("Asym", "xmid", "scal", "g0", "g1"),
  function.arg = c("GDP_PPP_pcap", "Asym","xmid", "scal", "g0", "g1", "Gini")
)

#############################################################################################################
#### Logistic function and iterations
#### In specifying random effects, let ones you suspect to be most stable estimated first
#############################################################################################################
# Additive effect of Gini -f believe a direct contributor to ES_pcap

multiple_logistic_model_6 <- deriv(
  ~ (a0 + a1 * density_psqkm + a2 * Gini) / (1 + exp((xmid - GDP_PPP_pcap) / scal)),
  namevec = c("a0", "a1", "a2", "xmid", "scal"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "a0", "a1", "a2", "xmid", "scal")
)

fit1 <- nlme(
  ES_pcap ~ multiple_logistic_model_6(GDP_PPP_pcap, density_psqkm, Gini, a0, a1, a2, xmid, scal),
  data = data1,
  fixed = a0 + a1 + a2 + xmid + scal ~ 1,
  #random = xmid + a2 ~ 1 | country_name, # This was done
  random = xmid + scal  ~ 1 | country_name,
  #random = xmid + a1 ~ 1 | country_name,
  #random = xmid ~ 1 | country_name,
  start = c(a0 = 5000, a1 = 10, a2 = 2, xmid = 10000, scal = 1000),
  #correlation = corARMA(p = 1, q = 2, form = ~ year | country_name),,  # <-- this models autocorrelation
  na.action = na.exclude, #na.exclude to retain original number of rows
  control = nlmeControl(pnlsTol = 0.5, maxIter = 500, minFactor = 1e-10, msMaxIter = 500, warnOnly = TRUE)
  #control = nlmeControl(pnlsTol = 0.1, maxIter = 100)
)

summary(fit1)


# Multiplicative effect of Gini - if believe inequality amplifies or dampens the effect of income
# and popilation density on ES_pcap - modulates the overall level of ES_pcap
# multiple_logistic_model_7 <- deriv(
#   ~ ((a0 + a1 * density_psqkm) / (1 + exp((xmid - GDP_PPP_pcap) / scal))) * exp(b0 + b1 * Gini),
#   namevec = c("a0", "a1", "xmid", "scal", "b0", "b1"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "a0", "a1", "xmid", "scal", "b0", "b1")
# )

multiple_logistic_model_7 <- deriv(
  ~ ((a0 + a1 * density_psqkm) / (1 + exp((xmid - GDP_PPP_pcap) / scal))) * exp(b1 * Gini),
  namevec = c("a0", "a1", "xmid", "scal", "b1"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "a0", "a1", "xmid", "scal", "b1")
)

fit2a <- nls(
  ES_pcap ~ multiple_logistic_model_7(GDP_PPP_pcap, density_psqkm, Gini, a0, a1, xmid, scal, b1),
  data = data1,
  #fixed = a0 + a1 + xmid + scal + b0 + b1 ~ 1,
  #random = pdDiag(~ 1 | country_name),
  #random =  xmid ~ 1 | country_name,
  start = c(a0 = 5000, a1 = 10, xmid = 10000, scal = 1000, b1 = 0.001),
  na.action = na.exclude, #na.exclude to retain original number of rows
  control = nlmeControl(pnlsTol = 0.5, maxIter = 200, minFactor = 1e-10, msMaxIter = 200, warnOnly = TRUE)
)

summary(fit2a)

fit2 <- nlme(
  ES_pcap ~ multiple_logistic_model_7(GDP_PPP_pcap, density_psqkm, Gini, a0, a1, xmid, scal, b1),
  data = data1,
  fixed = a0 + a1 + xmid + scal + b1 ~ 1,
  random =  xmid + scal ~ 1 | country_name,
  #random =  xmid + b1 ~ 1 | country_name,
  start = c(a0 = 5000, a1 = 10, xmid = 10000, scal = 1000, b1 = 0.001),
  na.action = na.exclude, #na.exclude to retain original number of rows
  control = nlmeControl(pnlsTol = 0.5, maxIter = 500, minFactor = 1e-10, msMaxIter = 500, warnOnly = TRUE)
  )

summary(fit2)

# Checking Variation of Gini within groups:
aggregate(Gini ~ country_name, data = data1, FUN = function(x) length(unique(x)))

# Check stability of outcomes
# with(data1, {
#   y_hat <- ((5000 + 10 * density_psqkm) / (1 + exp((10000 - GDP_PPP_pcap) / 1000))) * exp(1 + 0.001 * Gini)
#   summary(y_hat)
# })

# Interaction effect of Gini - modifies the effect of GDP - changes the steepens of the logistic
# curve based on inequality - changes how GDP affects ES_pcap
multiple_logistic_model_8 <- deriv(
  ~ (a0 + a1 * density_psqkm) / (1 + exp((xmid - GDP_PPP_pcap) / (scal + a2 * Gini))),
  namevec = c("a0", "a1", "a2", "xmid", "scal"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "a0", "a1", "a2", "xmid", "scal")
)

fit3 <- nlme(
  ES_pcap ~ multiple_logistic_model_8(GDP_PPP_pcap, density_psqkm, Gini, a0, a1, a2, xmid, scal),
  data = data1,
  fixed = a0 + a1 + a2 + xmid + scal ~ 1,
  #random = pdDiag(~ 1 | country_name),
  random =  xmid + a2 ~ 1 | country_name, # This was chosen
  #random =  xmid ~ 1 | country_name,
  start = c(a0 = 50000, a1 = -80, a2 = -400, xmid = 50000, scal = 50000),
  na.action = na.exclude, #na.exclude to retain original number of rows
  #control = nls.control(maxiter = 5000, minFactor = 1e-10, warnOnly = TRUE)
  control = nlmeControl(pnlsTol = 0.5, maxIter = 500, minFactor = 1e-10, msMaxIter = 500, warnOnly = TRUE)
  )

summary(fit3)
##############################################################################################################
# Analysis
##############################################################################################################
###################################################
### Correlation check

# Select relevant variables
vars <- data1[, c("GDP_PPP_pcap", "density_psqkm", "Gini")]

# Compute correlation matrix
cor_matrix <- cor(vars, use = "complete.obs")

# Print it
print(cor_matrix)

# Fit a linear model
lm_model <- lm(GDP_PPP_pcap ~ density_psqkm + Gini, data = data1)

# Compute VIF
vif(lm_model)

pairs(vars, main = "Scatterplot Matrix")

#### further troubleshooting
summary(data1)
any(!is.finite(data1$GDP_PPP_pcap))
any(!is.finite(data1$Gini_noise))
any(!is.finite(data1$ES_pcap))

# Plot ES_pcap versus GDP_PPP_pcap
ggplotly(ggplot(data1, aes(x = GDP_PPP_pcap, y = ES_pcap, color = country_name)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per Capita vs Energy Service per Capita",
       x = "GDP per Capita",
       y = "Energy Service per Capita") +
  theme_minimal())

### Residual analysis
# Extract residuals

# 1. Extract normalized residuals and add country info
# resid_df <- data.frame(data1,
#   residuals = resid(fit1, type = "normalized")
# )
# 
# # 2. Loop through each country and plot ACF and PACF
# unique_countries <- unique(resid_df$country_name)
# 
# for (country in unique_countries) {
#   country_resid <- resid_df %>%
#     filter(country_name == country) %>%
#     pull(residuals)
#   
#   # Plot ACF and PACF
#   par(mfrow = c(1, 2))  # side-by-side plots
#   acf(country_resid, main = paste("ACF -", country))
#   pacf(country_resid, main = paste("PACF -", country))
# }
# 
# 
# resid_ar1 <- resid(fit1, type = "normalized")
# 
# # Plot ACF
# acf(resid_ar1)
# pacf(resid_ar1)

##################################################################################################################
#### Summarizing Logistic model output
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

summary_table <- stargazer(fit1, type = "html", title = "Model Summary", out = here::here("results/model_summary_logistic1.html"))
summary_table <- stargazer(fit2, type = "html", title = "Model Summary", out = here::here("results/model_summary_logistic2.html"))
summary_table <- stargazer(fit3, type = "html", title = "Model Summary", out = here::here("results/model_summary_logistic3.html"))
# 
# summary_table <- broom.mixed::tidy(fit2)
# summary_table <- summary_table %>%
#   mutate(
#     term = gsub("random\\(Intercept\\)", "Random Intercept", term),
#     term = gsub("fixed\\(Intercept\\)", "Fixed Intercept", term)
#   )
# write.csv(summary_table, here::here("results/summary_table_logistic1.csv"), row.names = FALSE)
# 
# intervals <- intervals(fit1)

################################################################################################
#### Getting Predicted Values and Graphs
# Model chosen

fit <- fit1

# data1a <- data1
# 
# data1a$predicted_ES_pcap <- predict(fit, newdata = data1a)

# Predictions from model:
data1_model <- data1[complete.cases(data1[, c("ES_pcap", "GDP_PPP_pcap", "Gini")]), ]

data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model)
# Step 1: Create a copy of the data used in model fitting (after NA removal)

data1_model$residuals <- data1_model$ES_pcap - data1_model$predicted_ES_pcap

ggplot(data1_model, aes(x = ES_pcap, y = predicted_ES_pcap, color = country_name)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted ES_pcap", x = "Actual", y = "Predicted")

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
random_effects <- ranef(fit)

# Convert to a tidy format
random_df <- random_effects %>%
  tibble::rownames_to_column("country_name") %>%
  tidyr::pivot_longer(-country_name, names_to = "term", values_to = "estimate") %>%
  mutate(effect_type = "Random")

# Extract fixed effects
fixed_df <- broom::tidy(fit) %>%
  mutate(effect_type = "Fixed")

# Combine both
combined_df <- bind_rows(fixed_df, random_df)

# Create a GT table
gt(combined_df) %>%
  tab_header(title = "Fixed and Random Effects from Nonlinear Mixed-Effects Model") %>%
  cols_label(
    country_name = "Country",
    term = "Parameter",
    estimate = "Estimate",
    effect_type = "Effect Type"
  ) %>%
  fmt_number(columns = "estimate", decimals = 3)


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
    year > 2024)

head(future_data)

future_data$predicted_ES_pcap <- predict(fit, newdata = future_data, level = 1) # level = 1 to incorporate random effects

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

p1 <- ggplot() +
  # Historical data
  #geom_point(data = data1, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_line(data = data1_model, aes(x = GDP_PPP_pcap, y = ES_pcap, color = country_name, linetype = "Historical", group = country_name)) +
  
  # Future predictions
  #geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
  geom_line(data = future_data, aes(x = GDP_PPP_pcap, y = predicted_ES_pcap, color = country_name, linetype = "Predicted", group = country_name)) +
  
  #facet_wrap(~ country_name, scales = "fixed") +
  # Display country_name label next to line
  #geom_text(data = future_data %>% filter(year == max(year)-5), aes(x = year, y = predicted_ES_pcap, label = country_name), hjust = -0.1, size = 2) +
  scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
  labs(title = "Predicted Energy Service per Capita using Additive Logistic Model",
       x = "GDP per capita (USD)",
       y = "Energy Service per Capita (passenger km / capita)",
       linetype = "Data Type") +
  theme_bw()  + create_theme(30) +
  # Format x-axis which is GDP at level, but want to make it in thousands
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Save to interactive html widget
ggplotly(p1 + theme(legend.position = "right")) %>%
  layout(title = "Predicted Energy Service per Capita using Additive Logistic Model",
         xaxis = list(title = "GDP per capita (USD)"),
         yaxis = list(title = "Energy Service per Capita (passenger km / capita)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_ES_pcap_logistic1.html"), selfcontained = TRUE)

ggsave(here::here("plots/predicted_ES_pcap_logistic1.png"), plot = p1, width = 16, height = 16, dpi = 150)

###############################################################
### Multiplicative Logistic

fit <- fit2

# Predictions from model:
data1_model <- data1[complete.cases(data1[, c("ES_pcap", "GDP_PPP_pcap", "Gini")]), ]

data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model)
# Step 1: Create a copy of the data used in model fitting (after NA removal)

data1_model$residuals <- data1_model$ES_pcap - data1_model$predicted_ES_pcap

####### Displaying Output
# Extract random effects
random_effects <- ranef(fit)

# Convert to a tidy format
random_df <- random_effects %>%
  tibble::rownames_to_column("country_name") %>%
  tidyr::pivot_longer(-country_name, names_to = "term", values_to = "estimate") %>%
  mutate(effect_type = "Random")

# Extract fixed effects
fixed_df <- broom::tidy(fit) %>%
  mutate(effect_type = "Fixed")

# Combine both
combined_df <- bind_rows(fixed_df, random_df)

# Create a GT table
gt(combined_df) %>%
  tab_header(title = "Fixed and Random Effects from Nonlinear Mixed-Effects Model") %>%
  cols_label(
    country_name = "Country",
    term = "Parameter",
    estimate = "Estimate",
    effect_type = "Effect Type"
  ) %>%
  fmt_number(columns = "estimate", decimals = 3)


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
    year > 2024)


future_data$predicted_ES_pcap <- predict(fit, newdata = future_data, level = 1) # level = 1 to incorporate random effects

p2 <- ggplot() +
  # Historical data
  #geom_point(data = data1, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_line(data = data1_model, aes(x = GDP_PPP_pcap, y = ES_pcap, color = country_name, linetype = "Historical", group = country_name)) +
  
  # Future predictions
  #geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
  geom_line(data = future_data, aes(x = GDP_PPP_pcap, y = predicted_ES_pcap, color = country_name, linetype = "Predicted", group = country_name)) +
  
  #facet_wrap(~ country_name, scales = "fixed") +
  # Display country_name label next to line
  #geom_text(data = future_data %>% filter(year == max(year)-5), aes(x = year, y = predicted_ES_pcap, label = country_name), hjust = -0.1, size = 2) +
  scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
  labs(title = "Predicted Energy Service per Capita using Multiplicative Logistic Model",
       x = "GDP per capita (USD)",
       y = "Energy Service per Capita (passenger km / capita)",
       linetype = "Data Type") +
  theme_bw()  + create_theme(30) +
  # Format x-axis which is GDP at level, but want to make it in thousands
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Save to interactive html widget
ggplotly(p2+ theme(legend.position = "right")) %>%
  layout(title = "Predicted Energy Service per Capita using Multiplicative Logistic Model",
         xaxis = list(title = "GDP per capita (USD)"),
         yaxis = list(title = "Energy Service per Capita (passenger km / capita)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_ES_pcap_logistic2.html"), selfcontained = TRUE)

ggsave(here::here("plots/predicted_ES_pcap_logistic2.png"), plot = p2, width = 16, height = 16, dpi = 150)

###############################################################
### Interaction Logistic

fit <- fit3

# Predictions from model:
data1_model <- data1[complete.cases(data1[, c("ES_pcap", "GDP_PPP_pcap", "Gini")]), ]

data1_model$predicted_ES_pcap <- predict(fit, newdata = data1_model)
# Step 1: Create a copy of the data used in model fitting (after NA removal)

data1_model$residuals <- data1_model$ES_pcap - data1_model$predicted_ES_pcap

####### Displaying Output
# Extract random effects
random_effects <- ranef(fit)

# Convert to a tidy format
random_df <- random_effects %>%
  tibble::rownames_to_column("country_name") %>%
  tidyr::pivot_longer(-country_name, names_to = "term", values_to = "estimate") %>%
  mutate(effect_type = "Random")

# Extract fixed effects
fixed_df <- broom::tidy(fit) %>%
  mutate(effect_type = "Fixed")

# Combine both
combined_df <- bind_rows(fixed_df, random_df)

# Create a GT table
gt(combined_df) %>%
  tab_header(title = "Fixed and Random Effects from Nonlinear Mixed-Effects Model") %>%
  cols_label(
    country_name = "Country",
    term = "Parameter",
    estimate = "Estimate",
    effect_type = "Effect Type"
  ) %>%
  fmt_number(columns = "estimate", decimals = 3)


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
    year > 2024)


future_data$predicted_ES_pcap <- predict(fit, newdata = future_data, level = 1) # level = 1 to incorporate random effects

p3 <- ggplot() +
  # Historical data
  #geom_point(data = data1, aes(x = year, y = ES_pcap, color = country_name)) +
  geom_line(data = data1_model, aes(x = GDP_PPP_pcap, y = ES_pcap, color = country_name, linetype = "Historical", group = country_name)) +
  
  # Future predictions
  #geom_point(data = future_data, aes(x = year, y = predicted_ES_pcap, color = country_name)) +
  geom_line(data = future_data, aes(x = GDP_PPP_pcap, y = predicted_ES_pcap, color = country_name, linetype = "Predicted", group = country_name)) +
  
  #facet_wrap(~ country_name, scales = "fixed") +
  # Display country_name label next to line
  #geom_text(data = future_data %>% filter(year == max(year)-5), aes(x = year, y = predicted_ES_pcap, label = country_name), hjust = -0.1, size = 2) +
  scale_linetype_manual(values = c("Historical" = "solid", "Predicted" = "dashed")) +
  labs(title = "Predicted Energy Service per Capita using Interaction Logistic Model",
       x = "GDP per capita (USD)",
       y = "Energy Service per Capita (passenger km / capita)",
       linetype = "Data Type") +
  theme_bw()  + create_theme(30) +
  # Format x-axis which is GDP at level, but want to make it in thousands
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Save to interactive html widget
ggplotly(p3 + theme(legend.position = "right")) %>%
  layout(title = "Predicted Energy Service per Capita using Interaction Logistic Model",
         xaxis = list(title = "GDP per capita (USD)"),
         yaxis = list(title = "Energy Service per Capita (passenger km / capita)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_ES_pcap_logistic3.html"), selfcontained = TRUE)

ggsave(here::here("plots/predicted_ES_pcap_logistic3.png"), plot = p3, width = 16, height = 16, dpi = 150)

###########################################################################################
### Baseline adjustments
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

##################################################################################################################################
#####
# Model diagnostics
# 
# data1$fitted <- fitted(fit)
# data1$residuals <- residuals(fit)
# 
# # Residuals versus fitted plot
# 
# 
# ggplot(data1, aes(x = fitted, y = residuals, color = country_name)) +
#   geom_point(alpha = 0.6) +
#   facet_wrap(~ country_name, scales = "free") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(title = "Residuals vs Fitted Values",
#        x = "Fitted Values",
#        y = "Residuals") +
#   theme_minimal()
# 
# 
# 
# qqnorm(data1$residuals)
# qqline(data1$residuals, col = "red")
# 
# 
# # Fitted versus observed plot
# 
# ggplot(data1, aes(x = fitted, y = ES_pcap, color = country_name)) +
#   geom_point(alpha = 0.6) +
#   facet_wrap(~ country_name, scales = "free") +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
#   labs(title = "Fitted vs Observed Values",
#        x = "Fitted Values",
#        y = "Observed ES_pcap") +
#   theme_minimal()
# 
# # Breusch-Pagan test for Heteroscedasticity
# 
# bp_test <- lm(residuals^2 ~ fitted, data = data1)
# summary(bp_test)
# 
# 
# # Evidence of heteroskedasticity
# fit_weighted <- update(fit, weights = varPower())
# 
# 
# AIC(fit, fit_weighted)
# BIC(fit, fit_weighted)
# 
# # Extract residuals and fitted values
# 
# 
# data1$fitted_original <- fitted(fit)
# data1$residuals_original <- resid(fit)
# # 
# # data1$fitted_weighted <- fitted(fit_weighted)
# # data1$residuals_weighted <- resid(fit_weighted)
# 
# 
# # Extract the data actually used in the model
# model_data <- getData(fit_weighted)
# 
# # Add fitted and residuals to that data
# model_data$fitted_weighted <- fitted(fit_weighted)
# model_data$residuals_weighted <- resid(fit_weighted)
# 
# model_data$row_id <- as.integer(rownames(model_data))
# data1$row_id <- as.integer(rownames(data1))
# 
# data1 <- left_join(data1, model_data %>% select(row_id, fitted_weighted, residuals_weighted), by = "row_id")
# 
# # Original
# p1 <- ggplot(data1, aes(x = fitted_original, y = residuals_original)) +
#   geom_point(alpha = 0.6) +
#   facet_wrap(~ country_name, scales = "free") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#   labs(title = "Residuals vs Fitted (Original Model)")
# 
# # Weighted
# p2 <- ggplot(data1, aes(x = fitted_weighted, y = residuals_weighted)) +
#   geom_point(alpha = 0.6) +
#   facet_wrap(~ country_name, scales = "free") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#   labs(title = "Residuals vs Fitted (Weighted Model)")
# 
# # Display side by side
# library(patchwork)
# p1 + p2
# 
# 
# qqnorm(data1$residuals_original); qqline(data1$residuals_original, col = "red")
# qqnorm(data1$residuals_weighted); qqline(data1$residuals_weighted, col = "red")
# 
# 
# 
# 
# # saveRDS(all.country.mappings, here::here("data", "all_country_mappings.rds"))