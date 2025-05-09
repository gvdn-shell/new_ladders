#######################################################################################################################################
# Plot the curves with facets and equations

# Themes and colours

### theme for plotting
create_theme <- function(rel.size = 1) {
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

#### colour imports

# shell.brand.palette <- readxl::read_excel(here::here("data", "Shell Scenarios v14.6 2023_06_23.xlsm"), sheet = "Settings",
#                                           range = "D10:E50", col_names = TRUE) %>%
#   dplyr::select(-Colour)
# 
# saveRDS(shell.brand.palette, here::here("data", "shell_brand_palette.rds"))
# Load the shell.brand.palette data frame
shell.brand.palette <- readRDS(here::here("data", "shell_brand_palette.rds"))

shell.brand.palette.hex <- shell.brand.palette %>%
  mutate(Hex = paste0("#", Hex)) %>%
  mutate(country_id = row_number()) %>% 
  left_join(all.data %>% select(country_id, country_name), by = "country_id") 


# Load necessary libraries
library(ggplot2)
library(dplyr)

#####################################

gompertz_cdf <- function(x, b, c, Asym) {
  Asym * (1 - exp(-b / c * (exp(c * x) - 1)))
}


set.seed(123)
x <- seq(0, 10, length.out = 100)
b <- 0.1
c <- 1
Asym <- 1200
y <- gompertz_cdf(x, b, c, Asym) + rnorm(100, sd = 0.05) # Adding some noise
data <- data.frame(x = x, y = y)


start_params <- list(b = 1, c = 0.1, Asym = 1000)
fit <- nls(y ~ gompertz_cdf(x, b, c, Asym), data = data, start = start_params)
summary(fit)

# Predicted values
data$predicted <- predict(fit, newdata = data)

# Plot
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Gompertz CDF Fit", x = "x", y = "CDF") +
  theme_minimal()

#####################################

# Generate sample data
set.seed(123)
x <- seq(1, 100, length.out = 100)

# Define parameter sets for Gompertz and Weibull functions
gompertz_params <- expand.grid(Asym = c(10, 15), b2 = c(0.1, 0.2), b3 = c(20, 60))
weibull_params <- expand.grid(Asym = c(10, 15), Drop = c(.5, 1), lrc = c(log(20), log(60)), pwr = c(2, 3))

# Generate data for Gompertz functions
gompertz_data <- gompertz_params %>%
  rowwise() %>%
  mutate(x = list(x), y = list(Asym * exp(-exp(-b2 * (x - b3))))) %>%
  unnest(cols = c(x, y)) %>%
  # Round first 3 columns to 2 decimal places
  mutate(across(c(Asym, b2, b3), ~ round(.x, 2))) %>%
  mutate(Parameters = interaction(Asym, b2, b3, sep = ";"))

# Generate data for Weibull functions
weibull_data <- weibull_params %>%
  rowwise() %>%
  mutate(x = list(x), y = list(Asym * (1 -   exp(-(x / exp(lrc))^pwr)))) %>%
  unnest(cols = c(x, y)) %>%
  # Round first 4 columns to 2 decimal places
  mutate(across(c(Asym, Drop, lrc, pwr), ~ round(.x, 2))) %>%
  # Mutate parameters as their interaction but separate by a space
  # to make it look better in the legend
  mutate(Parameters = interaction(Asym, lrc, pwr, sep = ";")) #%>%
  #mutate(Parameters = interaction(Asym, Drop, lrc, pwr))

# Combine data
data_combined <- bind_rows(gompertz_data %>% mutate(Model = "Gompertz"),
                           weibull_data %>% mutate(Model = "Weibull"))

# Define equations
gompertz_eq <- "y = Asym * exp(-exp(-b2 * (x - b3)))"
weibull_eq <- "y = Asym * (1 - exp(-(x / exp(lrc))^pwr))"

# Plot the curves with facets and equations
p1 <- ggplot(data_combined, aes(x = x, y = y, color = Parameters)) +
  geom_line(size = 1) +
  facet_wrap( ~ Model , scales = "free_y") +
  labs(title = "S-Curves using Gompertz and Weibull Functions with Different Parameters",
       x = "x",
       y = "y",
       color = "Parameters") +
  theme_bw() +
  create_theme(rel.size = 1.5) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = shell.brand.palette.hex$Hex, breaks = data_combined$Parameters) +
  geom_text(data = data_combined %>% filter(Model == "Gompertz") %>% slice(1), 
            aes(x = 35, y = max(data_combined$y[data_combined$Model == "Gompertz"]) * 1.1, label = gompertz_eq), 
            parse = FALSE, inherit.aes = FALSE, size = 12) +
  geom_text(data = data_combined %>% filter(Model == "Weibull") %>% slice(1), 
            aes(x = 35, y = max(data_combined$y[data_combined$Model == "Weibull"]) * 1.1, label = weibull_eq), 
            parse = F, inherit.aes = FALSE, size = 12)

ggsave(here::here("plots", "gompertz_weibull_curves.png"), 
       plot = p1, 
       width = 12, height = 8, dpi = 200)
###############

