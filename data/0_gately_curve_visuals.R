# Remove all objects in R workspace (use with caution)
rm(list = ls())

# Check that required packages are installed and install if missing
ncpus <- 12
packages <- c("tidyverse",
              "ggplot2", 
              "nls2",
              "extrafont",
              "here",
              "gridExtra") #
installed_packages <- packages %in% rownames(installed.packages())

if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load required packages
invisible(lapply(packages, library, character.only = TRUE))
# Load the extrafont package
# font_import(paths = "C:/Windows/Fonts")

# Register the fonts you want to use
loadfonts()
extrafont::loadfonts(device="win")

###########################################################################

# Create a function to generate the Gompertz curve
gompertz_curve <- function(lambda, alpha, beta_i, GDPit) {
  lambda * exp(alpha * exp(beta_i * GDPit))
}



# Define the fixed value for lambda and the values for alpha and beta_i
lambda <- 5
alpha_values <- c(-0.1, -0.5, -1, -5)
beta_i_values <- c(-0.1, -0.5, -1, -5)

# Generate a sequence of GDPit values
GDPit <- seq(0, 20, by = 0.1)

# Create a directory to save the plots if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Initialize an empty data frame to store all the data
all_data <- data.frame()

# Loop through the values of beta_i and alpha to generate the data
for (beta_i in beta_i_values) {
  for (alpha in alpha_values) {
    # Generate the Gompertz curve values
    Y_it <- gompertz_curve(lambda, alpha, beta_i, GDPit)
    
    # Create a data frame for the current combination
    data <- data.frame(GDPit = GDPit, Y_it = Y_it, beta_i = as.factor(beta_i), alpha = as.factor(alpha))
    
    # Combine with the main data frame
    all_data <- rbind(all_data, data)
  }
}

# Create the plot
plot <- ggplot(all_data, aes(x = GDPit, y = Y_it, color = alpha)) +
  geom_line() +
  facet_wrap(~ beta_i, scales = "fixed") +
  labs(title = "Gompertz Curve for Different Combinations of alpha and beta_i (lambda = 1)",
       x = "GDPit",
       y = "Y_it",
       color = "alpha") +
  theme_bw() +
  facet_wrap(~ beta_i, scales = "fixed", labeller = label_both)


# Save the plot as a PNG file
ggsave(here::here("plots", paste0("/gompertz_facetted.png")), plot, width = 10, height = 6)


################################################################################################################
