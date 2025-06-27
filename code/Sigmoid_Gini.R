################################################################################
# SETUP: Clean Environment and Load Required Packages
################################################################################

# Remove all objects from the workspace (use with caution)
rm(list = ls())

# Define number of CPU cores for parallel installation
ncpus <- 12

# List of required packages
packages <- c(
  "ggplot2", "dplyr", "here", "plotly", "tibble", "extrafont", "showtext",
  "readxl", "tidyr", "htmlwidgets", "scales"
)

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load all required packages
invisible(lapply(packages, library, character.only = TRUE))

# Load fonts for plotting
extrafont::loadfonts(device = "win")
font_add("ShellMedium", "ShellMedium.ttf")  # Add custom font
showtext_auto()  # Automatically use showtext for new devices

################################################################################
# PLOT: Standardized theme for Scenarios
################################################################################
create_theme1 <- function(text_size = 14) {
  theme(
    axis.title.y = element_text(size = text_size, family = "ShellMedium"),
    axis.title.x = element_text(size = text_size, family = "ShellMedium"),
    axis.text.y = element_text(size = text_size - 2, family = "ShellMedium"),
    axis.text.x = element_text(size = text_size - 2, family = "ShellMedium", angle = 45, hjust = 1),
    legend.position = c(1.05, 0.9),
    legend.text = element_text(size = text_size - 2, family = "ShellMedium"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = text_size, family = "ShellMedium", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = text_size - 4, family = "ShellMedium"),
    plot.caption = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    panel.border = element_rect(color = "black", fill = NA),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
}

################################################################### 

# Load Shell brand color palette
shell.brand.palette <- readRDS(here::here("data", "shell_brand_palette_extended.rds")) %>%
  mutate(
    Hex = paste0("#", Hex),
    country_id = row_number()
  ) %>%
  select(country_id, Hex) %>%
  distinct()


################################################################################
# Load and preprocess main dataset
excluded.countries <- c("Saudi Arabia", "United Arab Emirates", "Singapore", "Kuwait",
                        "Qatar", "Libya", "Oman", "Luxembourg", "Rest Of East Asia", "Rest Of North America",
                        "Rest Of Oceania")
data <- readRDS("data/all_data_wem_espcap_imputation_wem_urban.rds") %>%
  group_by(country_id) %>%
  arrange(country_id, year) %>%
  mutate(
    lag_ES_pcap = lag(ES_pcap, order_by = year),
    # Diving Gini by 100 to ensure stability of estimates    
    Gini_00 = Gini / 100,
    Gini_01 = gini_case1 / 100,
    Gini_02 = gini_case2 / 100,
    Gini_03 = gini_case3 / 100,
    GDP_PPP_pcap_thousands = GDP_PPP_pcap / 1000
  ) %>%
  ungroup() %>%
  filter(year <= 2023, country_id != 82, !country_name %in% excluded.countries) 

summary(data)

# Create buckets for Gini coefficient and buckets for GDP per capita
data1 <- data %>%
  mutate(
    Gini_bucket = case_when(
      Gini_01 < 0.3 ~ "(0,0.3)",
      Gini_01 >= 0.3 & Gini_01 < 0.4 ~ "[0.3,0.4)",
      Gini_01 >= 0.4 & Gini_01 < 0.5 ~ "[0.4,0.5)",
      Gini_01 >= 0.5 & Gini_01 < 0.6 ~ "[0.5,0.6)",
      Gini_01 >= 0.6 ~ "[0.6,1]",
      TRUE ~ NA_character_
    ),
    GDP_PPP_pcap_bucket = case_when(
      GDP_PPP_pcap < 5000 ~ "(0,5000]",
      GDP_PPP_pcap >= 5000 & GDP_PPP_pcap < 10000 ~ "[5000,10000)",
      GDP_PPP_pcap >= 10000 & GDP_PPP_pcap < 15000 ~ "[10000,15000)",
      GDP_PPP_pcap >= 15000 & GDP_PPP_pcap < 20000 ~ "[15000,20000)",
      GDP_PPP_pcap >= 20000 & GDP_PPP_pcap < 25000 ~ "[20000,25000)",
      GDP_PPP_pcap >= 25000 & GDP_PPP_pcap < 30000 ~ "[25000,30000)",
      GDP_PPP_pcap >= 30000 & GDP_PPP_pcap < 35000 ~ "[30000,35000)",
      GDP_PPP_pcap >= 35000 & GDP_PPP_pcap < 40000 ~ "[35000,40000)",
      GDP_PPP_pcap >= 40000 & GDP_PPP_pcap < 45000 ~ "[40000,45000)",
      GDP_PPP_pcap >= 45000 & GDP_PPP_pcap < 50000 ~ "[45000,50000)",
      GDP_PPP_pcap >= 50000 & GDP_PPP_pcap < 55000 ~ "[50000,55000)",
      GDP_PPP_pcap >= 55000 & GDP_PPP_pcap < 60000 ~ "[55000,60000)",
      GDP_PPP_pcap >= 60000 & GDP_PPP_pcap < 65000 ~ "[60000,65000)",
      GDP_PPP_pcap >= 65000 ~ "[65000,Inf)",
      TRUE ~ NA_character_
    ))

data1
summary(data1)

data2 <- data1 %>%
  summarize(
    ES_pcap = median(ES_pcap, na.rm = TRUE),#,
    #GDP_PPP_pcap = mean(GDP_PPP_pcap, na.rm = TRUE),
    #Gini_01 = mean(Gini_01, na.rm = TRUE),
    #Gini_02 = mean(Gini_02, na.rm = TRUE),
    #Gini_03 = mean(Gini_03, na.rm = TRUE),
    #GDP_PPP_pcap = mean(GDP_PPP_pcap, na.rm = TRUE),
    .by = c("Gini_bucket","GDP_PPP_pcap_bucket")
  )
data2
summary(data2)
# Summarize data by calculating the average ES_pcap

# Merge palette with main dataset
data <- merge(shell.brand.palette, data, by = "country_id")
# Create a named vector for country colors
country_colors <- data %>%
  select(country_name, Hex) %>%
  distinct() 

#country_colors <- setNames(country_colors$Hex, country_colors$country_name)

###########################################################################################################
# PLOT: Gini vs GDP per capita
###########################################################################################################
# Create a plot of Energy Service vs GDP per capita by Gini bucket

# Order GDP per capita buckets
data1$GDP_PPP_pcap_bucket <- factor(
  data1$GDP_PPP_pcap_bucket,
  levels = c(
    "(0,5000]", "[5000,10000)", "[10000,15000)", "[15000,20000)",
    "[20000,25000)", "[25000,30000)", "[30000,35000)", "[35000,40000)",
    "[40000,45000)", "[45000,50000)", "[50000,55000)", "[55000,60000)",
    "[60000,65000)", "[65000,Inf)"
  ),
  ordered = TRUE
)
                         
# Order Gini buckets
data1$Gini_bucket <- factor(data1$Gini_bucket, 
                             levels = c("(0,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,1]"))

##### Plot the median ES_pcap per Gini bucket vs GDP_ppcap

p1 <- ggplot() +
  geom_smooth(
    data = data1,
    aes(
      x = GDP_PPP_pcap,
      y = ES_pcap,
      color = Gini_bucket,
      group = Gini_bucket,
      text = paste(
        # "Country:", country_name,
        # "Year:", year,
        # "GDP per capita:", scales::comma(GDP_PPP_pcap_thousands, accuracy = 0.01),
        "<br>Energy Service per capita:", scales::comma(ES_pcap, accuracy = 0.01)
      )
    ),
    se = FALSE,
    size = 2
  ) +
  scale_color_manual(values = country_colors$Hex) +
  labs(
    title = "Relationship Between GDP per Capita and Energy Service per Capita\nGrouped by Gini Coefficient Buckets Across Countries and Years",
    subtitle = "Each line represents a Gini bucket, showing smoothed trends across income levels",
    x = "GDP per Capita (USD 2018)",
    y = "Energy Service (vehicle kms/ capita/ year)",
    color = "Gini Bucket"
  ) +
  theme_minimal() +
  scale_y_continuous(trans = "sqrt") +
  create_theme1(16)

p1


ggplotly(p1 + theme(legend.position = "right"), tooltip = "text") %>%
  htmlwidgets::saveWidget(
    here::here("plots/ES_pcap_GDP_Gini.html"),
    selfcontained = TRUE
  )



###################################################################################################
### Dagum:

weibull_params <- readRDS(here::here("results/model_parameters_all_dagum_models.rds")) %>%
  rownames_to_column(var = "model") 


# Subset Weibull 6 parameters
weibull_params_6 <- weibull_params %>%
  filter(model == "model_chosen_1") %>%
  select(-model)


# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, alpha_0, alpha_1, scal, 
                             shape1_0, shape1_1, shape2) {
  (alpha_0 + alpha_1 * pop_dens) * (1 + (x / scal)^(-(shape1_0 + shape1_1 * x2)))^(-shape2)
}


# Constants
# Get parameters from weibull_params_3
alpha_0 <- weibull_params_6$alpha_0
alpha_1 <- weibull_params_6$alpha_1
scal <- weibull_params_6$scal
shape1_0 <- weibull_params_6$shape1_0
shape1_1 <- weibull_params_6$shape1_1
shape2 <- weibull_params_6$shape2

# Population density
pop_dens <- 270 # Median value

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.3, 0.7, by = 0.2)


# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              scal = scal,
                                              alpha_0 = alpha_0,
                                              alpha_1 = alpha_1,
                                              shape1_0 = shape1_0,
                                              shape1_1 = shape1_1,
                                              shape2 = shape2),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


# Interpolate y-values at 25th percentile of x and 75th percentile of x
intersections <- plot_data %>%
  group_by(x2) %>%
  summarise(
    x_25 = quantile(x, 0.25, na.rm = TRUE),
    x_75 = quantile(x, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(plot_data, by = "x2") %>%
  group_by(x2) %>%
  summarise(
    y_25 = approx(x, y, xout = first(x_25))$y,
    y_75 = approx(x, y, xout = first(x_75))$y,
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("y_"), names_to = "x_val", values_to = "y") %>%
  mutate(x_val = ifelse(x_val == "y_25", "25th percentile", "75th percentile"))

p1 <- ggplot(plot_data, aes(x = x, y = y, color = x2)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = shell.brand.palette$Hex, name = "Gini") +
  create_theme1(text_size = 32) +
  labs(
    title = expression(paste("Effect of Gini on Energy Service Captured by Dagum Model 1")),
    x = "GDP per Capita",
    y = "Energy Service (vehicle kms/ capita/ year)"
  ) +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "italic(y == (alpha[0] + alpha[1]*pop_dens[it]) * (1 + (GDPpcap[it] / scal) ^ (-(shape[0] + shape[1] * Gini[it]))) ^ (-shape[2]))",
    hjust = 1.1, vjust = 8.5,
    parse = TRUE,
    size = 14
  )

p1_wei6 <- p1

# Save to high resolution dpi
ggsave(here::here("plots/Sigmoid_Gini_dagum1.png"), plot = p1, width = 15, height = 10, dpi = 300)



###################################################################################################

weibull_params <- readRDS(here::here("results/model_parameters_all_weibull_models.rds")) %>%
  rownames_to_column(var = "model") 

# PLOT: WEIBULL MODEL 6
################################################################################
###############################################################################################################################################
# Weibull 6:


# Subset Weibull 6 parameters
weibull_params_6 <- weibull_params %>%
  filter(model == "model_chosen_6") %>%
  select(-model)

#### Weibull 3
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             b1, alpha_0, alpha_1, b2, gamma_0, gamma_1) {
  (alpha_0 + alpha_1 * pop_dens) * (1 - exp(-(x/(gamma_0 + gamma_1 * x2))^(b2 + b1*x2)))
}

# Constants
# Get parameters from weibull_params_3
alpha_0 <- weibull_params_6$alpha_0
alpha_1 <- weibull_params_6$alpha_1
scal_base <- weibull_params_6$scal_base
scal_slope <- weibull_params_6$scal_slope
shape_base <- weibull_params_6$shape_base
shape_slope <- weibull_params_6$shape_slope

# Population density
pop_dens <- 270 # Median value

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.3, 0.7, by = 0.2)


# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              b1 = shape_slope,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = shape_base, gamma_0 = scal_base, gamma_1 = scal_slope),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


# Interpolate y-values at 25th percentile of x and 75th percentile of x
intersections <- plot_data %>%
  group_by(x2) %>%
  summarise(
    x_25 = quantile(x, 0.25, na.rm = TRUE),
    x_75 = quantile(x, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(plot_data, by = "x2") %>%
  group_by(x2) %>%
  summarise(
    y_25 = approx(x, y, xout = first(x_25))$y,
    y_75 = approx(x, y, xout = first(x_75))$y,
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("y_"), names_to = "x_val", values_to = "y") %>%
  mutate(x_val = ifelse(x_val == "y_25", "25th percentile", "75th percentile"))

p1 <- ggplot(plot_data, aes(x = x, y = y, color = x2)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = shell.brand.palette$Hex, name = "Gini") +
  create_theme1(text_size = 32) +
  labs(
    title = expression(paste("Effect of Gini on Energy Service Captured by Weibull Model 6")),
    x = "GDP per Capita",
    y = "Energy Service (vehicle kms/ capita/ year)"
  ) +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "italic(y == (alpha[0] + alpha[1]*pop_dens[it]) * (1 + exp(-((GDPpcap[it]/ (gamma[0] + gamma[1]))^(beta[0] + beta[1] * Gini[it]) ))))",
    hjust = 1.1, vjust = 8.5,
    parse = TRUE,
    size = 14
  )

p1_wei6 <- p1

# Save to high resolution dpi
ggsave(here::here("plots/Sigmoid_Gini_weibull6.png"), plot = p1, width = 15, height = 10, dpi = 300)


################################################################################
# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Compute Energy Service values
# grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
#                                       scal = scal, b1 = shape,
#                                       alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))
grid$z <- with(grid, sigmoid_modified(x = x, x2 = x2, pop_dens = pop_dens,
                                      b1 = shape_slope,
                                      alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = shape_base, gamma_0 = scal_base, gamma_1 = scal_slope))



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

# library(plotly)
# 
# fig <- plot_ly(
#   x = ~x_vals,
#   y = ~x2_vals,
#   z = ~z_matrix,
#   type = "surface",
#   colorscale = "Viridis",
#   showscale = TRUE,
#   contours = list(
#     z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "limegreen", project = list(z = TRUE))
#   )
# ) %>%
#   layout(
#     title = "Energy Service Levels as a Function of GDP per Capita and Gini",
#     scene = list(
#       xaxis = list(
#         title = "GDP per Capita (PPP)",
#         tickformat = ".0f",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       yaxis = list(
#         title = "Gini Coefficient",
#         tickformat = ".2f",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       zaxis = list(
#         title = "Energy Service",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8))
#     ),
#     margin = list(l = 0, r = 0, b = 0, t = 50),
#     height = 700
#   )
# 
# fig


# Save to HTML
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot_weibull6.html"), selfcontained = TRUE)


#####################################################################################################################
# Weibull 2
# Subset Weibull 3 parameters
weibull_params_3 <- weibull_params %>%
  filter(model == "model_chosen_2") %>%
  select(-model)

weibull_model_2 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(gamma + scal * Gini))^shape)),
  namevec = c("alpha_0", "alpha_1", "scal", "shape", "gamma"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape", "gamma")
)

#### Weibull 3
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             scal, shape, alpha_0, alpha_1, gamma) {
  (alpha_0 + alpha_1 * pop_dens) * (1 - exp(-(x/(gamma + scal * x2))^(shape)))
}

# Constants
# Get parameters from weibull_params_3
alpha_0 <- weibull_params_3$alpha_0
alpha_1 <- weibull_params_3$alpha_1
scal <- weibull_params_3$scal
shape <- weibull_params_3$shape
gamma <- weibull_params_3$gamma

# Population density
pop_dens <- 270 # Median value

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.3, 0.7, by = 0.2)

# weibull_model_3 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(scal))^(gamma + shape * Gini))),
#   namevec = c("alpha_0", "alpha_1", "scal", "shape", "gamma"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape", "gamma")
# )


# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              scal = scal, shape = shape,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1, gamma = gamma),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


# Interpolate y-values at 25th percentile of x and 75th percentile of x
intersections <- plot_data %>%
  group_by(x2) %>%
  summarise(
    x_25 = quantile(x, 0.25, na.rm = TRUE),
    x_75 = quantile(x, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(plot_data, by = "x2") %>%
  group_by(x2) %>%
  summarise(
    y_25 = approx(x, y, xout = first(x_25))$y,
    y_75 = approx(x, y, xout = first(x_75))$y,
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("y_"), names_to = "x_val", values_to = "y") %>%
  mutate(x_val = ifelse(x_val == "y_25", "25th percentile", "75th percentile"))

p1 <- ggplot(plot_data, aes(x = x, y = y, color = x2)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = shell.brand.palette$Hex, name = "Gini") +
  create_theme1(text_size = 32) +
  labs(
    title = expression(paste("Effect of Gini on Energy Service Captured by Weibull Model 2")),
    x = "GDP per Capita",
    y = "Energy Service (vehicle kms/ capita/ year)"
  ) +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "italic(y == (alpha[0] + alpha[1]*pop_dens[it]) * (1 + exp(-((GDPpcap[it]/ (gamma + scal * Gini))^shape) )))",
    hjust = 1.1, vjust = 8.5,
    parse = TRUE,
    size = 14
  )


# Save to high resolution dpi
ggsave(here::here("plots/Sigmoid_Gini_weibull2.png"), plot = p1, width = 15, height = 10, dpi = 300)




####################################################################################################################


# Weibull 3:

# Subset Weibull 3 parameters
weibull_params_3 <- weibull_params %>%
  filter(model == "model_chosen_3") %>%
  select(-model)

#### Weibull 3
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             scal, b1, alpha_0, alpha_1, b2) {
  (alpha_0 + alpha_1 * pop_dens) * (1 - exp(-(x/scal)^(b2 + b1*x2)))
}

# Constants
# Get parameters from weibull_params_3
alpha_0 <- weibull_params_3$alpha_0
alpha_1 <- weibull_params_3$alpha_1
scal <- weibull_params_3$scal
shape <- weibull_params_3$shape
gamma <- weibull_params_3$gamma

# Population density
pop_dens <- 270 # Median value

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.3, 0.7, by = 0.2)

# weibull_model_3 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(scal))^(gamma + shape * Gini))),
#   namevec = c("alpha_0", "alpha_1", "scal", "shape", "gamma"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape", "gamma")
# )


# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              scal = scal, b1 = shape,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


# Interpolate y-values at 25th percentile of x and 75th percentile of x
intersections <- plot_data %>%
  group_by(x2) %>%
  summarise(
    x_25 = quantile(x, 0.25, na.rm = TRUE),
    x_75 = quantile(x, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(plot_data, by = "x2") %>%
  group_by(x2) %>%
  summarise(
    y_25 = approx(x, y, xout = first(x_25))$y,
    y_75 = approx(x, y, xout = first(x_75))$y,
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("y_"), names_to = "x_val", values_to = "y") %>%
  mutate(x_val = ifelse(x_val == "y_25", "25th percentile", "75th percentile"))

p1 <- ggplot(plot_data, aes(x = x, y = y, color = x2)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = shell.brand.palette$Hex, name = "Gini") +
  create_theme1(text_size = 32) +
  labs(
    title = expression(paste("Effect of Gini on Energy Service Captured by Weibull Model 3")),
    x = "GDP per Capita",
    y = "Energy Service (vehicle kms/ capita/ year)"
  ) +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "italic(y == (alpha[0] + alpha[1]*pop_dens[it]) * (1 + exp(-((GDPpcap[it]/ scal)^(gamma + shape * Gini[it]) ))))",
    hjust = 1.1, vjust = 8.5,
    parse = TRUE,
    size = 14
  )

p1_wei3 <- p1
# Save to high resolution dpi
ggsave(here::here("plots/Sigmoid_Gini_weibull3.png"), plot = p1, width = 15, height = 10, dpi = 300)


################################################################################
# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Compute Energy Service values
# grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
#                                       scal = scal, b1 = shape,
#                                       alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))
grid$z <- with(grid, sigmoid_modified(x = x, x2 = x2, pop_dens = pop_dens,
                                      scal = scal, b1 = shape,
                                      alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))



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

# library(plotly)
# 
# fig <- plot_ly(
#   x = ~x_vals,
#   y = ~x2_vals,
#   z = ~z_matrix,
#   type = "surface",
#   colorscale = "Viridis",
#   showscale = TRUE,
#   contours = list(
#     z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "limegreen", project = list(z = TRUE))
#   )
# ) %>%
#   layout(
#     title = "Energy Service Levels as a Function of GDP per Capita and Gini",
#     scene = list(
#       xaxis = list(
#         title = "GDP per Capita (PPP)",
#         tickformat = ".0f",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       yaxis = list(
#         title = "Gini Coefficient",
#         tickformat = ".2f",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       zaxis = list(
#         title = "Energy Service",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8))
#     ),
#     margin = list(l = 0, r = 0, b = 0, t = 50),
#     height = 700
#   )
# 
# fig


# Save to HTML
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot_weibull3.html"), selfcontained = TRUE)

####################################################################################################################

weibull_params <- readRDS(here::here("results/model_parameters_all_logistic_models.rds")) %>%
  rownames_to_column(var = "model") 
# Logistic 5:

# Subset Weibull 3 parameters
weibull_params_3 <- weibull_params %>%
  filter(model == "model_chosen_5") %>%
  select(-model)

logistic_model_5 <- deriv(
  ~ (alpha_0 + alpha_1 * density_psqkm) * (1 / (1 + exp((xmid - GDP_PPP_pcap)/(gamma_0 + gamma_1 * Gini)))) ^ (beta_0 + beta_1 * Gini),
  namevec = c("alpha_0", "alpha_1", "xmid", "beta_0", "beta_1", "gamma_0", "gamma_1"),
  function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "xmid", "beta_0", "beta_1", "gamma_0", "gamma_1")
)

#### Weibull 3
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             xmid, alpha_0, alpha_1, gamma_0, gamma_1, beta_0, beta_1) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x)/(gamma_0 + gamma_1 * x2)))) ^ (beta_0 + beta_1 * x2)
}

# Constants
# Get parameters from weibull_params_3
alpha_0 <- weibull_params_3$alpha_0
alpha_1 <- weibull_params_3$alpha_1
xmid <- weibull_params_3$xmid
gamma_0 <- weibull_params_3$gamma_0
gamma_1 <- weibull_params_3$gamma_1
beta_0 <- weibull_params_3$beta_0
beta_1 <- weibull_params_3$beta_1


# Population density
pop_dens <- 270 # Median value

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.3, 0.7, by = 0.2)

# weibull_model_3 <- deriv(
#   ~ (alpha_0 + alpha_1 * density_psqkm) * (1 - exp(-(GDP_PPP_pcap/(scal))^(gamma + shape * Gini))),
#   namevec = c("alpha_0", "alpha_1", "scal", "shape", "gamma"),
#   function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape", "gamma")
# )


# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              xmid = xmid, alpha_0 = alpha_0 , alpha_1 = alpha_1, gamma_0 = gamma_0, 
                                              gamma_1 = gamma_1, beta_0 = beta_0, beta_1 =beta_1),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


# Interpolate y-values at 25th percentile of x and 75th percentile of x
intersections <- plot_data %>%
  group_by(x2) %>%
  summarise(
    x_25 = quantile(x, 0.25, na.rm = TRUE),
    x_75 = quantile(x, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(plot_data, by = "x2") %>%
  group_by(x2) %>%
  summarise(
    y_25 = approx(x, y, xout = first(x_25))$y,
    y_75 = approx(x, y, xout = first(x_75))$y,
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("y_"), names_to = "x_val", values_to = "y") %>%
  mutate(x_val = ifelse(x_val == "y_25", "25th percentile", "75th percentile"))

p1 <- ggplot(plot_data, aes(x = x, y = y, color = x2)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = shell.brand.palette$Hex, name = "Gini") +
  create_theme1(text_size = 32) +
  labs(
    title = expression(paste("Effect of Gini on Energy Service Captured by Logistic Model 5")),
    x = "GDP per Capita",
    y = "Energy Service (vehicle kms/ capita/ year)"
  ) +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "italic(y == (alpha[0] + alpha[1]*pop_dens[it]) * (1 / (1 + exp((xmid - GDP_PPP_pcap)/(gamma[0] + gamma[1] * Gini[it])))) ^ (beta[0] + beta[1] * Gini[it]))",
    hjust = 1.1, vjust = 8.5,
    parse = TRUE,
    size = 14
  )

p1_log5 <- p1


# Save to high resolution dpi
ggsave(here::here("plots/Sigmoid_Gini_logistic5.png"), plot = p1, width = 15, height = 10, dpi = 300)

ggsave(here::here("plots/Sigmoid_Gini_logistic5.png"), plot = p1_log5, width = 15, height = 15, dpi = 200)
ggsave(here::here("plots/Sigmoid_Gini_weibull6.png"), plot = p1_wei6, width = 15, height = 15, dpi = 200)
ggsave(here::here("plots/Sigmoid_Gini_weibull3.png"), plot = p1_wei3, width = 15, height = 15, dpi = 200)


#### Create a single chart with 3 columns of plots: p1_wei6, p1_wei3, p1_log5
library(gridExtra)
p_all <- grid.arrange(p1_wei6, p1_log5, p1_wei3,  ncol = 3)
# Save to a png
ggsave(here::here("plots/Sigmoid_Gini_all_models.png"), p_all, width = 30, height = 10, dpi = 300)

################################################################################
# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Compute Energy Service values
# grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
#                                       scal = scal, b1 = shape,
#                                       alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))
grid$z <- with(grid, sigmoid_modified(x = x, x2 = x2, pop_dens = pop_dens,
                                      scal = scal, b1 = shape,
                                      alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))



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

# library(plotly)
# 
# fig <- plot_ly(
#   x = ~x_vals,
#   y = ~x2_vals,
#   z = ~z_matrix,
#   type = "surface",
#   colorscale = "Viridis",
#   showscale = TRUE,
#   contours = list(
#     z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "limegreen", project = list(z = TRUE))
#   )
# ) %>%
#   layout(
#     title = "Energy Service Levels as a Function of GDP per Capita and Gini",
#     scene = list(
#       xaxis = list(
#         title = "GDP per Capita (PPP)",
#         tickformat = ".0f",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       yaxis = list(
#         title = "Gini Coefficient",
#         tickformat = ".2f",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       zaxis = list(
#         title = "Energy Service",
#         backgroundcolor = "rgb(230,230,230)"
#       ),
#       camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8))
#     ),
#     margin = list(l = 0, r = 0, b = 0, t = 50),
#     height = 700
#   )
# 
# fig


# Save to HTML
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot_logistic5.html"), selfcontained = TRUE)




###############################################################################################################################################
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             xmid, scal, b1, alpha_0, alpha_1) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
}

# Constants
xmid <- 23432.86
scal <- 8155.97
alpha_0 <- 7293.36
alpha_1 <- -2.46
beta_1 <- 2.96
pop_dens <- 270

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.1, 0.9, by = 0.05)

# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              xmid = xmid, scal = scal, b1 = beta_1,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


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
  create_theme1(text_size = 16) +
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
ggsave(here::here("plots/Sigmoid_Gini_logistic2.png"), plot = p1, width = 15, height = 10, dpi = 300)


################################################################################


# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Compute Energy Service values
grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2, pop_dens = pop_dens,
                                              xmid = xmid, scal = scal, b1 = beta_1,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1))

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
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot_logistic2.html"), selfcontained = TRUE)

####################################

#### Logistic 3
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             xmid, scal, b1, alpha_0, alpha_1, b0) {
  (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2 + b0)
}

# Constants
xmid <- 22000
scal <- 9598.83
alpha_0 <- 8270.12
alpha_1 <- -2.95
beta_1 <- -1.29
beta_0 <- 1.88
pop_dens <- 270

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.1, 0.9, by = 0.05)

# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              xmid = xmid, scal = scal, b1 = beta_1,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1, b0 = beta_0),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


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
  create_theme1(text_size = 16) +
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
ggsave(here::here("plots/Sigmoid_Gini_logistic3.png"), plot = p1, width = 15, height = 10, dpi = 300)


################################################################################


# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Compute Energy Service values
grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2, pop_dens = pop_dens,
                                      xmid = xmid, scal = scal, b1 = beta_1,
                                      alpha_0 = alpha_0, alpha_1 = alpha_1, b0 = beta_0))

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
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot_logistic3.html"), selfcontained = TRUE)



####################################
####################################

#### Weibull 2
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             scal, b1, alpha_0, alpha_1, b2) {
  (alpha_0 + alpha_1 * pop_dens) * (1 - exp(-(x/(b2 + scal * x2))^b1))
}

# Constants
alpha_0 <- 9617.94
alpha_1 <- -3.08
scal <- -38920.88
shape <- 1.67
gamma <- 50493.51
pop_dens <- 270

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.1, 0.9, by = 0.05)

# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              scal = scal, b1 = shape,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


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
  labs(title = expression(paste("Effect of Gini on Weibull Curve when ", b[1], " = 6")),
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
ggsave(here::here("plots/Sigmoid_Gini_weibull2.png"), plot = p1, width = 15, height = 10, dpi = 300)


################################################################################
# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_val)

# Compute Energy Service values
# grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
#                                       scal = scal, b1 = shape,
#                                       alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))
grid$z <- with(grid, sigmoid_modified(x, x2, pop_dens = pop_dens,
                                      scal = scal, b1 = shape,
                                      alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))


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
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot_weibull2.html"), selfcontained = TRUE)

####################################
####################################

weibull_params <- readRDS(here::here("results/model_parameters_all_weibull_models.rds"))

#### Weibull 3
# Define the modified sigmoid function
sigmoid_modified <- function(x, x2, pop_dens, 
                             scal, b1, alpha_0, alpha_1, b2) {
  (alpha_0 + alpha_1 * pop_dens) * (1 - exp(-(x/scal)^(b2 + b1*x2)))
}

# Constants
alpha_0 <- 9120.25
alpha_1 <- --2.95
scal <- 35773.96
shape <- -0.82
gamma <- 1.91
pop_dens <- 270

# Generate data
x_vals <- seq(0, 125000, length.out = 500)
x2_vals <- seq(0.1, 0.9, by = 0.05)

# Create plot data
plot_data <- do.call(rbind, 
                     lapply(x2_vals, function(x2_val) {
                       data.frame(
                         x = x_vals,
                         y = sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
                                              scal = scal, b1 = shape,
                                              alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma),
                         x2 = as.factor(round(x2_val, 2))
                       )
                     })
)


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
  labs(title = expression(paste("Effect of Gini on Weibull Curve when ", b[1], " = 6")),
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
ggsave(here::here("plots/Sigmoid_Gini_weibull3.png"), plot = p1, width = 15, height = 10, dpi = 300)


################################################################################
# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_val)

# Compute Energy Service values
# grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2_val, pop_dens = pop_dens,
#                                       scal = scal, b1 = shape,
#                                       alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))
grid$z <- with(grid, sigmoid_modified(x, x2, pop_dens = pop_dens,
                                      scal = scal, b1 = shape,
                                      alpha_0 = alpha_0, alpha_1 = alpha_1, b2 = gamma))


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
htmlwidgets::saveWidget(fig, here::here("plots/energy_service_3d_plot_weibull3.html"), selfcontained = TRUE)





################################################################################################################################################
####

#################################

# Define the modified sigmoid function
# sigmoid_modified <- function(x, x2, pop_dens, xmid = 13800, scal = 11700, b1 = 6.3, alpha_0 = 7200, alpha_1 = -0.618) {
#   (alpha_0 + alpha_1 * pop_dens) * (1 / (1 + exp((xmid - x) / scal)))^(b1 * x2)
# }

# Create grid of GDP_PPP (x) and Gini (x2) values
x_vals <- seq(0, 125000, length.out = 200)
x2_vals <- seq(0.1, 0.9, length.out = 100)

# Create meshgrid
grid <- expand.grid(x = x_vals, x2 = x2_vals)

# Define population density values for animation
pop_dens_values <- seq(100, 500, length.out = 10)

# Create frames for each population density
frames <- lapply(pop_dens_values, function(pd) {
  grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2, 
                                        xmid = xmid, scal = scal, b1 = beta_1,
                                        alpha_0 = alpha_0, alpha_1 = alpha_1, pop_dens = pop_dens_values))
  z_matrix <- matrix(grid$z, nrow = length(x2_vals), ncol = length(x_vals), byrow = TRUE)
  list(
    data = list(list(z = z_matrix, type = "surface")),
    name = paste0("pop_dens=", round(pd))
  )
})

# Initial frame
initial_pd <- pop_dens_values[1]
grid$z <- with(grid, sigmoid_modified(x_vals, x2 = x2, 
                                      xmid = xmid, scal = scal, b1 = beta_1,
                                      alpha_0 = alpha_0, alpha_1 = alpha_1, pop_dens = initial_pd))
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
