# Contour Plots for Skewness and Variance

library(ggplot2)
library(dplyr)
library(directlabels)


# Log-normal Distribution


# Generate data for skewness
lognormal_skewness <- expand.grid(mu = seq(0.05, 1, 0.05), sigma = seq(0.05, 1, 0.05)) %>%
  mutate(skewness = 2 * (exp(sigma^2) + 1) * sqrt(exp(sigma^2) - 1))

# Generate data for variance
lognormal_variance <- expand.grid(mu = seq(0.05, 1, 0.05), sigma = seq(0.05, 1, 0.05)) %>%
  mutate(variance = (exp(sigma^2) - 1) * exp(2 * mu + sigma^2))

# Plot skewness
skewness_plot <- ggplot(lognormal_skewness, aes(x = mu, y = sigma, z = skewness)) +
  geom_contour(aes(colour = ..level..)) +
  labs(title = "Skewness of Lognormal Distribution", x = "mu", y = "sigma")

direct.label(skewness_plot, "top.points")

# Plot variance
variance_plot <- ggplot(lognormal_variance, aes(x = mu, y = sigma, z = variance)) +
  geom_contour(aes(colour = ..level..)) +
  labs(title = "Variance of Lognormal Distribution", x = "mu", y = "sigma")

direct.label(variance_plot, "top.points")

# Weibull Distribution


# Generate data for skewness
weibull_skewness <- expand.grid(alpha = seq(1, 10, 0.1), beta = seq(0.1, 5, 0.1)) %>%
  mutate(skewness = (2 * gamma(1 + 1/alpha)^3 - 3 * gamma(1 + 1/alpha) * gamma(1 + 2/alpha) + gamma(1 + 3/alpha)) / (gamma(1 + 2/alpha) - gamma(1 + 1/alpha)^2)^(3/2))

# Generate data for variance
weibull_variance <- expand.grid(alpha = seq(1, 10, 0.1), beta = seq(0.1, 5, 0.1)) %>%
  mutate(variance = beta^2 * (gamma(1 + 2/alpha) - gamma(1 + 1/alpha)^2))

# Plot skewness
skewness_plot <- ggplot(weibull_skewness, aes(x = alpha, y = beta, z = skewness)) +
  geom_contour(aes(colour = ..level..)) +
  labs(title = "Skewness of Weibull Distribution", x = "alpha", y = "beta")

direct.label(skewness_plot, "top.points")

# Plot variance
variance_plot <- ggplot(weibull_variance, aes(x = alpha, y = beta, z = variance)) +
  geom_contour(aes(colour = ..level..)) +
  labs(title = "Variance of Weibull Distribution", x = "alpha", y = "beta")

direct.label(variance_plot, "top.points")
