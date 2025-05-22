### https://www.youtube.com/watch?v=Kln0ZQ7sX8k


library(ggplot2)

# Define Jacobian matrix calculation
jacobian <- function(f, a, b, x) {
  eps <- 1e-6
  # Numerical approximation of the gradient
  grad_a <- (f(a + eps, b, x) - f(a - eps, b, x)) / (2 * eps) 
  grad_b <- (f(a, b + eps, x) - f(a, b - eps, x)) / (2 * eps)
  return(cbind(grad_a, grad_b))
}

# Function for Gauss-Newton optimization

Gauss_Newton <- function(f, x, y, a0, b0, tol, max_iter) {
  old <- new <- c(a0, b0)
  
  for (itr in 1:max_iter) {
    old <- new
    J <- jacobian(f, old[1], old[2], x)
    dy <- y - f(old[1], old[2], x)
    new <- old + solve(t(J) %*% J) %*% t(J) %*% dy
    
    if (sqrt(sum((old - new)^2)) < tol) {
      break
    }
  }
  
  return(new)
}

## General Jacobian


General_Jacobian <- function(f, b, x) {
  eps <- 1e-6
  grads <- list()
  
  for (i in seq_along(b)) {
    t <- rep(0, length(b))
    t[i] <- t[i] + eps
    grad <- (f(b + t, x) - f(b - t, x)) / (2 * eps)
    grads[[i]] <- grad
  }
  
  return(do.call(cbind, grads))
}


General_Gauss_Newton <- function(f, x, y, b0, tol, max_iter) {
  old <- new <- b0
  
  for (itr in 1:max_iter) {
    old <- new
    J <- General_Jacobian(f, old, x)
    dy <- y - f(old, x)
    new <- old + solve(t(J) %*% J) %*% t(J) %*% dy
    
    if (sqrt(sum((old - new)^2)) < tol) {
      break
    }
  }
  
  return(new)
}


nl_function1 <- function(a, b, x) {
  a * x / (b + x)
}


# Generate data from real model y = 2x / (3+x)

x <- seq(0, 5, length.out = 50)
y <- nl_function1(2, 3, x) + rnorm(50, 0, 0.1)
#Plot the data
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  labs(title = "Data from real model y = 2x / (3+x)",
       x = "x",
       y = "y") +
  theme_minimal()
# Fit the model using nls
fit <- nls(y ~ nl_function1(a, b, x), data = data.frame(x, y), start = list(a = 1, b = 1))
# Print the summary of the fit
summary(fit)
# Plot the data and the fitted model
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_line(aes(y = predict(fit)), color = "red") +
  labs(title = "Data from real model y = 2x / (3+x)",
       x = "x",
       y = "y") +
  theme_minimal()
# Plot the residuals
residuals <- resid(fit)
ggplot(data.frame(x, residuals), aes(x, residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of the fit",
       x = "x",
       y = "Residuals") +
  theme_minimal()
# Plot the fitted values
fitted_values <- fitted(fit)
ggplot(data.frame(x, fitted_values), aes(x, fitted_values)) +
  geom_point() +
  labs(title = "Fitted values of the model",
       x = "x",
       y = "Fitted values") +
  theme_minimal()
# Plot the fitted values vs the original data
ggplot(data.frame(x, y, fitted_values), aes(x, y)) +
  geom_point() +
  geom_line(aes(y = fitted_values), color = "red") +
  labs(title = "Fitted values vs original data",
       x = "x",
       y = "y") +
  theme_minimal()


# Calculate the Jacobian matrix at the estimated parameters
params <- coef(fit)
jacobian_matrix <- jacobian(nl_function1, params[1], params[2], x)
# Print the Jacobian matrix
print(jacobian_matrix)



# Initial guess
a0 <- 1
b0 <- 1
# Tolerance and maximum iterations
tol <- 1e-6
max_iter <- 100
# Run Gauss-Newton optimization
result <- Gauss_Newton(nl_function1, x, y, a0, b0, tol, max_iter)
# Print the result
print(result)

y_hat <- nl_function1(result[1], result[2], x) # Use estimated coefficients from Gauss-Newton
# Plot scatter of y and x with fitted line
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_line(aes(y = y_hat), color = "red") +
  labs(title = "Fitted values vs original data (Gauss-Newton)",
       x = "x",
       y = "y") +
  theme_minimal()

#####################################################################################################

nl_function2 <- function(a, b, x) {
  (1/(2 * pi * b)^(1/2)) * exp(- 0.5 * (1/b) * (x-a) ^ 2) 
}
# Generate data from real model y = (1/(2 * pi * b)^(1/2)) * exp(- 0.5 * (1/b) * (x-a) ^ 2)
y <- nl_function2(2.5, 0.5, x) + rnorm(50, 0, 0.1)
#Plot the data
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  labs(title = "Data from real model y = (1/(2 * pi * b)^(1/2)) * exp(- 0.5 * (1/b) * (x-a) ^ 2)",
       x = "x",
       y = "y") +
  theme_minimal()

# Fit model using Gauss-Newton
result2 <- Gauss_Newton(nl_function2, x, y, 3, 1, 1e-5, 10)
# Plot the data and the fitted model
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_line(aes(y = nl_function2(result2[1], result2[2], x)), color = "red") +
  labs(title = "Data from real model y = (1/(2 * pi * b)^(1/2)) * exp(- 0.5 * (1/b) * (x-a) ^ 2)",
       x = "x",
       y = "y") +
  theme_minimal()

###############################################################################################
### 2D case:
# Define the function
nl_function3 <- function(a, b, X) {
    return(a - (1 / b) * X[, 1]^2 - X[, 2]^2)
  }

# Generate data from real model

# Create sequences
x1 <- seq(-5, 5, length.out = 50)
x2 <- seq(-5, 5, length.out = 50)

# Create a grid of coordinates
grid <- expand.grid(x1, x2)

# Combine the grid into a matrix
X <- cbind(grid[, 1], grid[, 2])

# Print the result
print(X)
# Generate data
y <- nl_function3(5, 4, X) + rnorm(nrow(X), 0, 1)

# Plot 3D plot of y versus X


scatterplot3d(X[, 1], X[, 2], y, pch = 16, highlight.3d = TRUE,
              main = "3D Scatterplot of y vs X",
              xlab = "X1", ylab = "X2", zlab = "y")




plot_ly(x = X[, 1], y = X[, 2], z = y, type = "scatter3d", mode = "markers", marker = list(color = y))

result3 <- Gauss_Newton(nl_function3, X, y, 3, 1, 1e-5, 10)
# Plot the data and the fitted model
ggplot(data.frame(X, y), aes(X1, X2)) +
  geom_point(aes(color = y)) +
  geom_contour(aes(z = nl_function3(result3[1], result3[2], X)), color = "red") +
  labs(title = "Data from real model y = a - (1 / b) * x1^2 - x2^2",
       x = "x1",
       y = "x2") +
  theme_minimal()


y_hat <- nl_function3(result3[1], result3[2], X)  # Use estimated coefficients from Gauss-Newton

# Create the interactive 3D surface plot and scatter plot
fig <- plot_ly() %>%
  add_surface(x = x1, y = x2, z = matrix(y_hat, nrow = 50), colorscale = "Cool") %>%
  add_markers(x = X[, 1], y = X[, 2], z = y, marker = list(color = y))

# Customize the layout
fig <- fig %>%
  layout(scene = list(
    xaxis = list(title = 'X1'),
    yaxis = list(title = 'X2'),
    zaxis = list(title = 'Y')
  ))

#### General functions

nl_function4 <- function(b, X) {
  return(b[1] - (1 / b[2]) * X[, 1]^2 - (1/b[3]) * X[, 2]^2)
}
# Generate data from real model
y <- nl_function4(c(5, 4, 2), X) + rnorm(nrow(X), 0, 1)
# Plot 3D plot of y versus X
scatterplot3d(X[, 1], X[, 2], y, pch = 16, highlight.3d = TRUE,
              main = "3D Scatterplot of y vs X",
              xlab = "X1", ylab = "X2", zlab = "y")
# Fit model using Gauss-Newton
result4 <- General_Gauss_Newton(nl_function4, X, y, c(3, 1, 1), 1e-5, 10)
# Plot the data and the fitted model


###############################################

# Gompertz_Gately <- function(b, X, country_dummies) {
#   # Assuming country_dummies is a matrix where each column represents a dummy variable for a country
#   country_effect <- rowSums(country_dummies * b[8:length(b)])
#   
#   return((b[1] + b[2] * X[,1] + b[3] * X[,2]) * (b[4] * X[,3] + b[5] * X[,4]) * 
#            exp(b[6] * exp((b[7] + country_effect) * X[,5])))
# }
# 
# Gompertz_Gately <- function(b, X) {
#   return((b[1] + b[2] * X[,1] + b[3] * X[,2]) * (b[4] * X[,3] + b[5] * X[,4]) * 
#            exp(b[6] * exp (b[7] * X[,5])))
# }
# # Generate data from real model
# x1 <- seq(0, 5, length.out = 50)
# x2 <- seq(0, 5, length.out = 50)
# x3 <- seq(0, 5, length.out = 50)
# x4 <- seq(0, 5, length.out = 50)
# x5 <- seq(0, 5, length.out = 50)
# # Create a grid of coordinates
# grid <- expand.grid(x1, x2, x3, x4, x5)
# # Combine the grid into a matrix
# X <- cbind(grid[, 1], grid[, 2], grid[, 3], grid[, 4], grid[, 5])
# # Generate data
# y <- Gompertz_Gately(c(1, 2, 3, 4, 5, 6, 7), X) + rnorm(nrow(X), 0, 1)
# # Plot 3D plot of y versus X
# scatterplot3d(X[, 1], X[, 2], y, pch = 16, highlight.3d = TRUE,
#               main = "3D Scatterplot of y vs X",
#               xlab = "X1", ylab = "X2", zlab = "y")
# # Fit model using Gauss-Newton
# result5 <- General_Gauss_Newton(Gompertz_Gately, X, y, c(1, 1, 1, 1, 1, 1, 1), 1e-5, 10)
# # Plot the data and the fitted model
# ggplot(data.frame(X, y), aes(X1, X2)) +
#   geom_point(aes(color = y)) +
#   geom_contour(aes(z = Gompertz_Gately(result5, X)), color = "red") +
#   labs(title = "Data from real model y = (b[1] + b[2] * x1 + b[3] * x2) * (b[4] * x3 + b[5] * x4) * exp(b[6] * exp (b[7] * x5))",
#        x = "x1",
#        y = "x2") +
#   theme_minimal()