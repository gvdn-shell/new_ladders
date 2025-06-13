library(ggplot2)
library(dplyr)
library(here)

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
    plot.background = element_rect(fill = "white", color = NA),
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

# Define the modified sigmoid function
sigmoid_modified <- function(x, xmid = 13800, scal = 11700, b1 = 6.3, x2, pop_dens, alpha_0 = 7200, alpha_1 = -0.618) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
}

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.1, 0.9, by = 0.2)
pop_dens <- 270  # Mean Population density

# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val){
                       data.frame(
                         x = x_vals, 
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens), 
                         x2 = as.factor(round(x2_val, 2))
                       )
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
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  create_theme(text_size = 16) +
  labs(title = expression(paste("Effect of Gini on Sigmoid Curve when ", b[1], " = 6")),
       x = "GDP per Capita",
       y = "Energy Service",
       color = "Gini") +
  geom_vline(xintercept = c(10, 12.5), linetype = "dashed", color = "grey") +
  geom_hline(data = intersections, aes(yintercept = y, color = x2), 
             linetype = "dotted", show.legend = FALSE) +
  # Add legend and display to right of plot
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(linetype = "solid")))
  

# Save to high resolution dpi
ggsave(here::here("plots/Sigmoid_Gini.png"), plot = p1, width = 15, height = 10, dpi = 300)


################################################################################

# Load required libraries
library(plotly)

# Define the modified sigmoid function
sigmoid_modified <- function(x, xmid = 13800, scal = 11700, b1 = 6.3, x2, pop_dens = 270, alpha_0 = 7200, alpha_1 = -0.618) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
}

# Create grid of GDP_PPP (x) and Gini (x2) values
x_vals <- seq(0, 125000, length.out = 200)
x2_vals <- seq(0.1, 0.9, length.out = 100)

# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Compute Energy Service values
grid$z <- with(grid, sigmoid_modified(x, x2 = x2, pop_dens = 270))

# Reshape for plotly
z_matrix <- matrix(grid$z, nrow = length(x2_vals), ncol = length(x_vals), byrow = TRUE)

# Create 3D surface plot
fig <- plot_ly(
  x = ~x_vals,
  y = ~x2_vals,
  z = ~z_matrix,
  type = "surface",
  colorscale = "Viridis"
) %>%
  layout(
    title = "Energy Service Levels as a Function of GDP_PPP and Gini",
    scene = list(
      xaxis = list(title = "GDP per Capita (PPP)"),
      yaxis = list(title = "Gini Coefficient"),
      zaxis = list(title = "Energy Service")
    )
  )

# Save to HTML
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot.html"), selfcontained = TRUE)

#################################

# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, xmid = 13800, scal = 11700, b1 = 6.3, alpha_0 = 7200, alpha_1 = -0.618) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
}

# Create grid of GDP_PPP (x) and Gini (x2) values
x_vals <- seq(0, 125000, length.out = 200)
x2_vals <- seq(0.1, 0.9, length.out = 100)

# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Define population density values for animation
pop_dens_values <- seq(100, 500, length.out = 10)

# Create frames for each population density
frames <- lapply(pop_dens_values, function(pd) {
  grid$z <- with(grid, sigmoid_modified(x, x2, pop_dens = pd))
  z_matrix <- matrix(grid$z, nrow = length(x2_vals), ncol = length(x_vals), byrow = TRUE)
  list(
    data = list(list(z = z_matrix, type = "surface")),
    name = paste0("pop_dens=", round(pd))
  )
})

# Initial frame
initial_pd <- pop_dens_values[1]
grid$z <- with(grid, sigmoid_modified(x, x2, pop_dens = initial_pd))
z_matrix <- matrix(grid$z, nrow = length(x2_vals), ncol = length(x_vals), byrow = TRUE)

# Create plot
fig <- plot_ly(
  x = ~x_vals,
  y = ~x2_vals,
  z = ~z_matrix,
  type = "surface",
  colorscale = "Viridis"
) %>%
  layout(
    title = "Energy Service vs GDP_PPP and Gini (Animated by Population Density)",
    scene = list(
      xaxis = list(title = "GDP per Capita (PPP)"),
      yaxis = list(title = "Gini Coefficient"),
      zaxis = list(title = "Energy Service")
    ),
    updatemenus = list(
      list(
        type = "buttons",
        buttons = list(
          list(label = "Play", method = "animate", args = list(NULL))
        )
      )
    ),
    frames = frames
  )

# Save to HTML
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_animated_plot.html"), selfcontained = TRUE)

####################

library(plotly)
library(dplyr)
library(tidyr)
library(htmlwidgets)

# Define the sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, xmid = 13800, scal = 11700, b1 = 6.3, alpha_0 = 7200, alpha_1 = -0.618) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
}

# Create grid
x_vals <- seq(0, 125000, length.out = 100)
x2_vals <- seq(0.1, 0.9, length.out = 50)
pop_dens_vals <- seq(100, 500, by = 50)

grid <- expand.grid(
  x = x_vals,
  x2 = x2_vals,
  pop_dens = pop_dens_vals
)

# Compute z values
grid$z <- with(grid, sigmoid_modified(x, x2, pop_dens))

# Prepare data for plotly
plot_data <- grid %>%
  mutate(pop_dens = as.factor(pop_dens)) %>%
  group_by(pop_dens) %>%
  nest() %>%
  mutate(
    z_matrix = purrr::map(data, ~ matrix(.x$z, nrow = length(x2_vals), ncol = length(x_vals), byrow = TRUE))
  )

# Create frames
frames <- lapply(1:nrow(plot_data), function(i) {
  list(
    name = as.character(plot_data$pop_dens[i]),
    data = list(
      list(
        type = "surface",
        z = plot_data$z_matrix[[i]],
        x = x_vals,
        y = x2_vals,
        colorscale = "Viridis"
      )
    )
  )
})

# Initial surface
initial_z <- plot_data$z_matrix[[1]]

# Create plot
fig <- plot_ly(
  type = "surface",
  x = ~x_vals,
  y = ~x2_vals,
  z = ~initial_z,
  colorscale = "Viridis"
) %>%
  layout(
    title = "Energy Service vs GDP_PPP and Gini (Animated by Population Density)",
    scene = list(
      xaxis = list(title = "GDP per Capita (PPP)"),
      yaxis = list(title = "Gini Coefficient"),
      zaxis = list(title = "Energy Service")
    ),
    updatemenus = list(
      list(
        type = "buttons",
        buttons = list(
          list(label = "Play", method = "animate", args = list(NULL))
        )
      )
    )
  ) %>%
  animation_opts(frame = 1000, redraw = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Population Density: ")) %>%
  plotly::config(displayModeBar = TRUE)

# Add frames manually
fig$x$frames <- frames

# Save to HTML
saveWidget(fig, file = here::here("plots/energy_service_animated_plot.html"), selfcontained = TRUE)

##################

library(ggplot2)
library(gganimate)
library(transformr)
library(gifski)

# Load required libraries
library(ggplot2)
library(gganimate)
library(dplyr)

# Define the sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, xmid = 13800, scal = 11700, b1 = 6.3, alpha_0 = 7200, alpha_1 = -0.618) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
}

# Define values
x_vals <- seq(0, 125000, length.out = 200)
x2_vals <- c(0.1, 0.3, 0.5, 0.7)  # fixed Gini slices
pop_dens_vals <- seq(100, 500, by = 50)

# Generate data
plot_data <- expand.grid(x = x_vals, x2 = x2_vals, pop_dens = pop_dens_vals) %>%
  mutate(energy_service = sigmoid_modified(x, x2, pop_dens),
         Gini = factor(x2),
         pop_dens_label = paste("Pop. Density:", pop_dens))

# Create animated plot
p <- ggplot(plot_data, aes(x = x, y = energy_service, color = Gini)) +
  geom_line(size = 1) +
  labs(title = "Energy Service vs GDP per Capita",
       subtitle = "{closest_state}",
       x = "GDP per Capita (PPP)",
       y = "Energy Service",
       color = "Gini Coefficient") +
  theme_minimal(base_size = 14) +
  transition_states(pop_dens_label, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Save animation
anim_save(here::here("plots/energy_service_2D_animation.gif"), p, width = 800, height = 600, duration = 10)
