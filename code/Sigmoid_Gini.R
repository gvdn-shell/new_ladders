library(ggplot2)
library(dplyr)

# Define the modified sigmoid function
sigmoid_modified <- function(x, xmid = 10, scal = 1, b1 = 6, x2) {
  10 * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
}

# Generate data
x_vals <- seq(0, 25, length.out = 500)
x2_vals <- seq(0.1, 0.9, by = 0.2)

plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val){
                       data.frame(x = x_vals, 
                                  y = sigmoid_modified(x_vals, xmid = 10, scal = 1, b1 = 6, x2 = x2_val), 
                                  x2 = as.factor(round(x2_val, 2)))
                     }))

# Interpolate y-values at x = 10 and x = 12.5
intersections <- plot_data %>%
  group_by(x2) %>%
  summarise(
    y_10 = approx(x, y, xout = 10)$y,
    y_125 = approx(x, y, xout = 12.5)$y
  ) %>%
  tidyr::pivot_longer(cols = starts_with("y_"), names_to = "x_val", values_to = "y") %>%
  mutate(x_val = ifelse(x_val == "y_10", 10, 12.5))

# Plot
p1 <- ggplot(plot_data, aes(x = x, y = y, color = x2)) +
  geom_line() +
  labs(title = expression(paste("Effect of Gini on Sigmoid Curve when ", b[1], " = 6")),
       x = "GDP per Capita",
       y = "Energy Service",
       color = "Gini") +
  geom_vline(xintercept = c(10, 12.5), linetype = "dashed", color = "grey") +
  geom_hline(data = intersections, aes(yintercept = y, color = x2), 
             linetype = "dotted", show.legend = FALSE) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Save to high resolution dpi
ggsave(here::here("plots/Sigmoid_Gini.png"), plot = p1, width = 15, height = 10, dpi = 300)
