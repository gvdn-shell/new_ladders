# Remove all objects in R workspace (use with caution)
rm(list = ls())

# Check that required packages are installed and install if missing
ncpus <- 12
packages <- c("tidyverse",
              "ggplot2", 
              "nls2",
              "extrafont",
              "here",
              "gridExtra",
              "minpack.lm",
              "sysfonts",
              "showtext",
              "DBI","odbc","readxlsb","RODBC",
              "WDI",
              "openxlsx",
              "plotly",
              "plm",
              "nlme",
              "nls2") #
installed_packages <- packages %in% rownames(installed.packages())

if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load required packages
invisible(lapply(packages, library, character.only = TRUE))
# Load the extrafont package
# font_import(paths = "C:/Windows/Fonts")

# Access the proxy settings from environment variables
http_proxy <- Sys.getenv("http_proxy")
Sys.setenv(http_proxy = http_proxy)
Sys.setenv(https_proxy = http_proxy)

# syntax: font_add(family = "<family_name>", regular = "/path/to/font/file")
font_add("ShellMedium", "ShellMedium.ttf")
font_families()

## automatically use showtext for new devices
showtext_auto()

#Plot: need to open Windows graphics device as showtext does not work well with RStudio built-in graphics device
windows()

##################################################################################################################################
### Load data

all.data <- readRDS( here::here("data", "all_data_wem.rds"))

panel_data <- all.data %>%
  rename(
    country = country_name,
    gini =SI.POV.GINI,
    GDP = GDP_PPP_pcap,
    density = EN.POP.DNST,
    urbanization = SP.URB.TOTL.IN.ZS,
    ) 

### Preprocessing of Dbar and Ubar
usa_values <- panel_data %>%
  filter(country == 'USA') %>%
  select(year, density, urbanization) %>%
  rename(density_USA = density, urbanization_USA = urbanization)

# Merge USA values with original data

all.data1 <- panel_data %>%
  left_join(usa_values, by = "year")


# Calculate Dbar and Ubar

panel_data <- all.data1 %>%
  ungroup() %>%
  mutate(Dbar = ifelse(density > density_USA, density - density_USA, 0),
         Ubar = ifelse(urbanization > urbanization_USA, urbanization - urbanization_USA, 0),
         Rising = ifelse(GDP > lag_GDP_PPP_pcap, 1, 0),
         Falling = ifelse(GDP < lag_GDP_PPP_pcap, 1, 0))


# Define the Gompertz model function
gompertz_model <- function(params, data) {
  gamma_max <- 5000
  lambda <- params[1]
  phi <- params[2]
  alpha <- params[3]
  beta <- params[4:(length(params)-2)]
  theta_R <- params[length(params)-1]
  theta_F <- params[length(params)]
  
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  exp_term <- exp(alpha * exp(beta[as.numeric(as.factor(data$country))] * data$GDP))
  energy_service_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - (theta_R * data$Rising + theta_F * data$Falling)) * data$lag_energy_service
  
  return(energy_service_pred)
}

# Initial parameter values
initial_params <- c(-0.000388, -0.007765, -5.897, rep(-0.15, length(unique(panel_data$country))), 0.095, 0.084)

# Fit the model with cleaned data
fit <- nls(
  energy_service ~ gompertz_model(params, panel_data),
  data = panel_data,
  start = list(params = initial_params)
)

# View the summary of the model fit
summary(fit)





######################################################################################

# Define the Gompertz model function
gompertz_model <- function(params, data) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  alpha <- params[4]
  beta <- params[5:length(params)]
  theta_R <- params[length(params) - 1]
  theta_F <- params[length(params)]
  
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  exp_term <- exp(alpha * exp(beta[as.numeric(as.factor(data$country))] * data$GDP))
  energy_service_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - (theta_R * data$Rising + theta_F * data$Falling)) * data$lag_energy_service
  
  return(energy_service_pred)
}

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, -5.897, rep(-0.15, length(unique(panel_data$country))), 0.095, 0.084)

# Fit the model
fit <- nls(
  energy_service ~ gompertz_model(params, panel_data),
  data = panel_data,
  start = list(params = initial_params)
)
####
sum(is.na(panel_data))
sum(is.infinite(panel_data))

# Drop rows with NA values in density, urbanization, energy_service, GDP, lag_GDP_PPP_pcap, lag_energy_service, Rising, Falling, Ubar, Dbar
panel_data1 <- panel_data %>%
  filter(!is.na(density) & !is.na(urbanization) & !is.na(energy_service) & 
           !is.na(GDP) & !is.na(lag_GDP_PPP_pcap) & !is.na(lag_energy_service) &
           !is.na(Rising) & !is.na(Falling) & !is.na(Ubar) & !is.na(Dbar))


fit <- nls(
  energy_service ~ gompertz_model(params, panel_data1),
  data = panel_data,
  start = list(params = initial_params)
)
# View the summary of the model fit
summary(fit)

# Example structure of your panel dataset
# panel_data <- data.frame(
#   country = rep(c("USA", "Canada", "Mexico", "Germany"), each = 20),
#   year = rep(2000:2019, times = 4),
#   GDP = rnorm(80, mean = 50000, sd = 10000),
#   density = rnorm(80, mean = 100, sd = 20),
#   urbanization = runif(80, min = 50, max = 90),
#   vehicle_ownership = rnorm(80, mean = 200, sd = 50)
# )

# View the first few rows of the dataset
head(panel_data)

gompertz_model <- function(params, data) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  alpha <- params[4]
  beta <- params[5:length(params)]
  
  data <- data %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(
      #lag_vehicle_ownership = lag(vehicle_ownership),
      R_it = ifelse(GDP > lag(GDP), 1, 0),
      F_it = ifelse(GDP < lag(GDP), 1, 0)
    ) %>%
    ungroup()
  
  D_it <- data$density
  U_it <- data$urbanization
  D_USA <- mean(data$density[data$country == "USA"])
  U_USA <- mean(data$urbanization[data$country == "USA"])
  
  D_bar_it <- ifelse(D_it > D_USA, D_it - D_USA, 0)
  U_bar_it <- ifelse(U_it > U_USA, U_it - U_USA, 0)
  
  saturation_level_it <- gamma_max + lambda * D_bar_it + phi * U_bar_it
  
  exp_term <- exp(alpha * exp(beta[as.numeric(as.factor(data$country))] * data$GDP))
  vehicle_ownership_pred <- saturation_level_it * (params[6] * data$R_it + params[7] * data$F_it) * exp_term + (1 - (params[6] * data$R_it + params[7] * data$F_it)) * data$lag_vehicle_ownership
  
  return(vehicle_ownership_pred)
}

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, -5.897, rep(-0.15, length(unique(panel_data$country))), 0.095, 0.084)

# Fit the model
fit <- nls(
  vehicle_ownership ~ gompertz_model(params, panel_data),
  data = panel_data,
  start = list(params = initial_params)
)

# View the summary of the model
summary(fit)

# Extract estimated parameters
estimated_params <- coef(fit)

# View the estimated parameters
print(estimated_params)

############################################################################################################



# Define parameters of Gompertz function
fraction_saturation_higher <- 1.1
country_id_max_saturation <- 1
gamma_max <- fraction_saturation_higher * max(all.data.gompertz$energy_service[all.data.gompertz$country_id == country_id_max_saturation], na.rm = TRUE) 

### data frame in following format:
# country, year, density, GDP, lag(GDP), ES, lag(ES), urbanization

set.seed(123) # For reproducibility

# Define the Modified Gompertz according to Gately et. al (2006)

model_function <- function(params, data) {
  gamma_max_USA <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha <- params[6]
  beta_i <- params[7:length(params)]
  
  # Group by country and arrange by year
  data <- data %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(
      R_it = ifelse(GDP > lag(GDP), 1, 0),
      F_it = ifelse(GDP < lag(GDP), 1, 0),
      V_i_t_minus_1 = lag(V_it)
    )
  
  # Extract density values
  D_it <- data$density
  D_USA_t <- data$density[data$country == "USA" & data$year == data$year]
  
  # Extract urbanization values (assuming U_it and U_USA_t are similarly defined)
  U_it <- data$urbanization
  U_USA_t <- data$urbanization[data$country == "USA" & data$year == data$year]
  
  # Define Ubar_it and Dbar_it for country i at time t
  D_bar_it <- ifelse(D_it > D_USA_t, D_it - D_USA_t, 0)
  U_bar_it <- ifelse(U_it > U_USA_t, U_it - U_USA_t, 0)
  
  # Saturation levels defined by the USA
  saturation_level_it <- gamma_max_USA + lambda * D_bar_it + phi * U_bar_it
  
  # Define the exponential term
  exp_term <- exp(alpha * exp(beta_i[as.numeric(as.factor(data$country))] * data$GDP))
  V_it <- saturation_level_it * (theta_R * data$R_it + theta_F * data$F_it) * exp_term + (1 - (theta_R * data$R_it + theta_F * data$F_it)) * data$V_i_t_minus_1
  
  return(V_it)
}

#######################################################################





#######################################################################

# Define the objective function (sum of squared residuals)
objective_function <- function(params, data) {
  V_it_pred <- model_function(params, data)
  SSR <- sum((data$V_it - V_it_pred)^2)
  return(SSR)
}

# Example data frame
data <- data.frame(
  country = rep(c("USA", "CAN", "MEX"), each = 10),
  year = rep(1960:1969, times = 3),
  density = runif(30, 0, 100),
  GDP = runif(30, 1000, 50000),
  #GDP_i_t_minus_1 = runif(30, 1000, 50000),
  V_i_t_minus_1 = runif(30, 0, 1000),
  V_it = runif(30, 0, 1000),
  R_it = runif(30, 0, 1),
  F_it = runif(30, 0, 1),
  urbanization = runif(30, 0, 100),
  D_USA_t = runif(30, 0, 100),
  U_USA_t = runif(30, 0, 100)
)

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, 0.095, 0.084, -5.897, rep(-0.20, length(unique(data$country))))

# Optimize the parameters using iterative least squares
result <- optim(par = initial_params, fn = objective_function, data = data, method = "BFGS")

# Estimated parameters
estimated_params <- result$par
print(estimated_params)


#########################################################################################################################
# Define the model function
model_function <- function(params, data, q = 1.1) {
  # Extract parameters
  gamma_max_USA <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha <- params[6]
  beta_i <- params[7:length(params)] # Beta_i for each country
  
  # Calculate gamma_max_USA based on the maximum V_it value for the USA
  gamma_max_USA <- max(data %>% filter(country == "USA") %>% pull(V_it)) * q
  
  # Ensure the data is in the correct format
  data <- data %>%
    mutate(D_it = ifelse(D_it > 0, D_it, 0))
  
  # Define Ubar_it and Dbar_it for country i at time t
  data <- data %>%
    mutate(
      D_bar_it = ifelse(D_it > D_USA_t, D_it - D_USA_t, 0),
      U_bar_it = ifelse(U_it > U_USA_t, U_it - U_USA_t, 0)
    )
  
  # Saturation levels defined by the USA
  data <- data %>%
    mutate(saturation_level_it = gamma_max_USA + lambda * D_bar_it + phi * U_bar_it)
  
  # Defining different responses to falling and rising incomes
  data <- data %>%
    mutate(
      R_it = ifelse(GDP_it > GDP_i_t_minus_1, 1, 0),
      F_it = ifelse(GDP_it < GDP_i_t_minus_1, 1, 0),
      theta = theta_R * R_it + theta_F * F_it
    )
  
  # Define the exponential term
  data <- data %>%
    mutate(exp_term = exp(alpha + beta_i[as.numeric(as.factor(country))] * log(GDP_it)))
  
  # Calculate V_it based on the modified Gompertz curve
  data <- data %>%
    mutate(V_it_pred = saturation_level_it * theta * exp_term + (1 - theta) * V_i_t_minus_1)
  
  return(data$V_it_pred)
}

# Define the objective function (sum of squared residuals)
objective_function <- function(params, data, q = 1.1) {
  V_it_pred <- model_function(params, data, q)
  SSR <- sum((data$V_it - V_it_pred)^2)
  return(SSR)
}

# Example data frame
data <- data.frame(
  country = rep(c("USA", "CAN", "MEX"), each = 10),
  year = rep(1960:1969, times = 3),
  D_it = runif(30, 0, 100),
  GDP_it = runif(30, 1000, 50000),
  GDP_i_t_minus_1 = runif(30, 1000, 50000),
  V_i_t_minus_1 = runif(30, 0, 1000),
  V_it = runif(30, 0, 1000),
  U_it = runif(30, 0, 100),
  D_USA_t = runif(30, 0, 100),
  U_USA_t = runif(30, 0, 100)
)

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, 0.095, 0.084, -5.897, rep(-0.20, length(unique(data$country))))

# Optimize the parameters using iterative least squares
result <- optim(par = initial_params, fn = objective_function, data = data, method = "BFGS")

# Estimated parameters
estimated_params <- result$par
print(estimated_params)

# Plot the results
data$V_it_pred <- model_function(estimated_params, data)

library(ggplot2)
ggplot(data, aes(x = year)) +
  geom_line(aes(y = V_it, color = "Observed")) +
  geom_line(aes(y = V_it_pred, color = "Predicted")) +
  facet_wrap(~ country) +
  labs(title = "Observed vs Predicted V_it", y = "V_it") +
  theme_minimal()
