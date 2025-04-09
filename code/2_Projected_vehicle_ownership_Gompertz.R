
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(minpack.lm)

# Example historical data (replace with actual data)
years <- 1960:2022
GDP_per_capita <- c(90, 76, 71, 74, 85, 98, 104, 97, 91, 100, 113, 119, 132, 157, 160, 178, 165, 185, 156, 195, 197, 203, 225, 251, 294, 282, 252, 284, 311, 318, 333, 366, 377, 473, 610, 709, 782, 829, 873, 959, 1053, 1149, 1289, 1509, 1753, 2099, 2694, 3468, 3832, 4550, 5614, 6301, 7020, 7636, 8016, 8094, 8817, 9905, 10144, 10409, 12618, 12663, 12614)
vehicle_ownership <- c(12, 15, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 235, 240, 245, 250, 255, 260, 265, 270, 275, 280, 285, 290, 295, 300, 305, 310, 315)

# Ensure all vectors have the same length
length(years) == length(GDP_per_capita) && length(GDP_per_capita) == length(vehicle_ownership)

# Create data frame
historical_data <- data.frame(
  year = years,
  GDP_per_capita = GDP_per_capita,
  vehicle_ownership = vehicle_ownership
)

# Check for missing or infinite values
sum(is.na(historical_data))
sum(is.infinite(historical_data))

# Plot historical data
ggplot(historical_data, aes(x = year)) +
  geom_line(aes(y = GDP_per_capita, color = "GDP per capita")) +
  geom_line(aes(y = vehicle_ownership, color = "Vehicle Ownership")) +
  labs(title = "Historical Data",
       x = "Year",
       y = "Value") +
  scale_color_manual(values = c("GDP per capita" = "blue", "Vehicle Ownership" = "red")) +
  theme_minimal()



# Define the Gompertz function
gompertz_function <- function(alpha, beta, V_star, GDP_per_capita) {
  V_star * exp(-alpha * exp(-beta * GDP_per_capita))
}

# Provide initial estimates
initial_alpha <- -2
initial_beta <- -0.0001
initial_V_star <- 800

# Fit the Gompertz function to historical data using nlsLM
fit_gompertz <- nlsLM(vehicle_ownership ~ gompertz_function(alpha, beta, V_star, GDP_per_capita),
                      data = historical_data,
                      start = list(alpha = initial_alpha, beta = initial_beta, V_star = initial_V_star),
                      control = nls.lm.control(maxiter = 1000))

# Extract fitted parameters
params <- coef(fit_gompertz)
alpha <- params["alpha"]
beta <- params["beta"]
V_star <- params["V_star"]

# Print fitted parameters
print(params)


# Example future GDP per capita data (replace with actual projections)
future_years <- 2024:2050
future_GDP_per_capita <- c(13000, 13500, 14000, 14500, 15000, 15500, 16000, 16500, 17000, 17500, 18000, 18500, 19000, 19500, 20000, 20500, 21000, 21500, 22000, 22500, 23000, 23500, 24000, 24500, 25000, 25500, 26000)

# Ensure both vectors have the same length
length(future_years) == length(future_GDP_per_capita)

# Calculate projected vehicle ownership
projected_vehicle_ownership <- gompertz_function(alpha, beta, V_star, future_GDP_per_capita)

# Create a data frame for plotting
projection_data <- data.frame(year = future_years, GDP_per_capita = future_GDP_per_capita, vehicle_ownership = projected_vehicle_ownership)

# Plot the projections
ggplot(projection_data, aes(x = year, y = vehicle_ownership)) +
  geom_line(color = "blue") +
  labs(title = "Projected Vehicle Ownership in China (2024-2050)",
       x = "Year",
       y = "Vehicle Ownership (per 1000 people)") +
  theme_minimal()


# Define energy consumption function
energy_consumption <- function(vehicle_ownership, AFE, population) {
  vehicle_ownership * AFE * population / 1000
}

# Example data for energy consumption calculation
AFE <- 0.1 # Average fuel economy (toe per vehicle)
population <- 1.4e9 # Population of China

# Calculate energy consumption
projected_energy_consumption <- energy_consumption(projected_vehicle_ownership, AFE, population)

# Add energy consumption to the projection data frame
projection_data$energy_consumption <- projected_energy_consumption

# Plot energy consumption projections
ggplot(projection_data, aes(x = year, y = energy_consumption)) +
  geom_line(color = "red") +
  labs(title = "Projected Energy Consumption of Road Vehicles in China (2024-2050)",
       x = "Year",
       y = "Energy Consumption (toe)") +
  theme_minimal()



# Sensitivity analysis for different GDP growth rates
sensitivity_analysis <- function(alpha, beta, V_star, GDP_per_capita_scenarios) {
  lapply(GDP_per_capita_scenarios, function(GDP_per_capita) {
    gompertz_function(alpha, beta, V_star, GDP_per_capita)
  })
}

# Example GDP per capita scenarios
GDP_per_capita_scenarios <- list(
  baseline = future_GDP_per_capita,
  high_growth = future_GDP_per_capita * 1.1,
  low_growth = future_GDP_per_capita * 0.9
)

# Perform sensitivity analysis
sensitivity_results <- sensitivity_analysis(alpha, beta, V_star, GDP_per_capita_scenarios)

# Create a data frame for plotting
sensitivity_data <- data.frame(
  year = rep(future_years, times = length(sensitivity_results)),
  vehicle_ownership = unlist(sensitivity_results),
  scenario = rep(names(sensitivity_results), each = length(future_years))
)

# Plot sensitivity analysis results
ggplot(sensitivity_data, aes(x = year, y = vehicle_ownership, color = scenario)) +
  geom_line() +
  labs(title = "Sensitivity Analysis of Vehicle Ownership Projections",
       x = "Year",
       y = "Vehicle Ownership (per 1000 people)") +
  theme_minimal()
