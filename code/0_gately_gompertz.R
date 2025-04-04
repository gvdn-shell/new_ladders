# Not working but trying to flesh out the method
library(minpack.lm)
library(dplyr)
library(ggplot2)

# Define the modified Gompertz model function to predict V_it
model_function <- function(params, data) {
  gamma_max_USA <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha <- params[6]
  beta_i <- params[7:length(params)]
  
  #######
  # Saturation levels defined by the USA
  # Define Ubar_it and Dbar_it for country i at time t
  D_bar_it <- ifelse(D_it > D_USA_t, D_it - D_USA_t, 0) # If more densely populated, should have lower saturation levels
  U_bar_it <- ifelse(U_it > U_USA_t, U_it - U_USA_t, 0) # If more urbanized, should have lower saturation levels
  saturation_level_it <- gamma_max_USA + lambda * data$Dbar_it + phi * data$Ubar_it # For country i at time t,lambda and phi negative
  
  # Defining different responses to falling and rising incomes
  R_it = ifelse(GDP_it > GDP_i_t_minus_1, 1, 0) 
  F_it = ifelse(GDP_it < GDP_i_t_minus_1, 1, 0)
  theta <- theta_R * R_it + theta_F * F_it
  
  # Define the exponential term
  exp_term <- exp(alpha * exp(beta_i[as.numeric(as.factor(data$Country))] * data$GDP_it))
  V_it <- saturation_level_it * theta * exp_term + (1 - theta) * data$V_i_t_minus_1
  return(V_it)
}


#######################################################################



library(dplyr)

# Define the model function
model_function <- function(params, data) {
  gamma_max_USA <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha <- params[6]
  beta_i <- params[7:length(params)] # Beta_i for each country
  
  # Ensure the data is in the correct format
  data <- data %>%
    mutate(D_it = ifelse(D_it > 0, D_it, 0))
  
  # Define Ubar_it and Dbar_it for country i at time t
  D_bar_it <- ifelse(data$D_it > data$D_USA_t, data$D_it - data$D_USA_t, 0)
  U_bar_it <- ifelse(data$U_it > data$U_USA_t, data$U_it - data$U_USA_t, 0)
  
  # Saturation levels defined by the USA
  saturation_level_it <- gamma_max_USA + lambda * D_bar_it + phi * U_bar_it
  
  # Defining different responses to falling and rising incomes
  R_it <- ifelse(data$GDP_it > data$GDP_i_t_minus_1, 1, 0)
  F_it <- ifelse(data$GDP_it < data$GDP_i_t_minus_1, 1, 0)
  theta <- theta_R * R_it + theta_F * F_it
  
  # Define the exponential term
  exp_term <- exp(alpha + beta_i[as.numeric(as.factor(data$country))] * log(data$GDP_it))
  
  # Calculate V_it based on the modified Gompertz curve
  V_it <- saturation_level_it * theta * exp_term + (1 - theta) * data$V_i_t_minus_1
  
  return(V_it)
}

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
  D_it = runif(30, 0, 100),
  GDP_it = runif(30, 1000, 50000),
  GDP_i_t_minus_1 = runif(30, 1000, 50000),
  V_i_t_minus_1 = runif(30, 0, 1000),
  V_it = runif(30, 0, 1000),
  R_it = runif(30, 0, 1),
  F_it = runif(30, 0, 1),
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
