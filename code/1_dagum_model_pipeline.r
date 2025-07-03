# dagum_model_pipeline.R

# Load required packages --------------------------------------------------
load_required_packages <- function() {
  packages <- c(
    "dplyr", "minpack.lm", "tidyr", "purrr", "ggplot2", "nlme", "broom.mixed",
    "gt", "prophet", "readr", "here", "stargazer", "plotly", "lme4", "car",
    "sysfonts", "extrafont", "showtext", "stringr", "knitr", "kableExtra",
    "nls2", "future.apply", "texreg", "openxlsx"
  )

  to_install <- packages[!(packages %in% rownames(installed.packages()))]
  if (length(to_install)) install.packages(to_install, dependencies = TRUE)
  invisible(lapply(packages, library, character.only = TRUE))
}

# Load and preprocess data -----------------------------------------------
preprocess_data <- function(file_path) {
  data <- readRDS(file_path) %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    mutate(
      lag_ES_pcap = lag(ES_pcap),
      Gini_01 = gini_case1 / 100,
      GDP_PPP_pcap_thousands = GDP_PPP_pcap / 1000
    ) %>%
    ungroup()

  data <- data %>%
    group_by(country_name) %>%
    mutate(density_psqkm_std = scale(density_psqkm)) %>%
    ungroup()

  return(data)
}

# Dagum model registry --------------------------------------------------
dagum_model_registry <- list(
  dagum_v1 = list(
    name = "Dagum V1",
    formula = deriv(
      ~ (alpha_0 + alpha_1 * density_psqkm) * 
        (1 + (GDP_PPP_pcap / scal)^(-(shape1_0 + shape1_1 * Gini)))^(-shape2),
      namevec = c("alpha_0", "alpha_1", "scal", "shape1_0", "shape1_1", "shape2"),
      function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "alpha_0", "alpha_1", "scal", "shape1_0", "shape1_1", "shape2")
    ),
    predictors = c("GDP_PPP_pcap", "density_psqkm", "Gini_01"),
    grid_fun = function() expand.grid(
      alpha_0 = seq(1000, 50000, length.out = 5),
      alpha_1 = seq(-10, 10, length.out = 5),
      scal = seq(1000, 70000, length.out = 5),
      shape1_0 = seq(0.1, 10, length.out = 5),
      shape1_1 = seq(0.1, 10, length.out = 5),
      shape2 = seq(0.1, 10, length.out = 5)
    )
  ),

  dagum_v2 = list(
    name = "Dagum V2 (Urbanization + Lag)",
    formula = deriv(
      ~ phi * lag_ES_pcap + 
        (alpha_0 + alpha_1 * density_psqkm + alpha_2 * urbanization) * 
        (1 + (GDP_PPP_pcap / scal)^(-(shape1_0 + shape1_1 * Gini)))^(-shape2),
      namevec = c("alpha_0", "alpha_1", "alpha_2", "scal", "shape1_0", "shape1_1", "shape2", "phi"),
      function.arg = c("GDP_PPP_pcap", "density_psqkm", "Gini", "urbanization", 
                       "alpha_0", "alpha_1", "alpha_2", "scal", 
                       "shape1_0", "shape1_1", "shape2", "lag_ES_pcap", "phi")
    ),
    predictors = c("GDP_PPP_pcap", "density_psqkm", "Gini_01", "urbanization_perc", "lag_ES_pcap"),
    grid_fun = function() expand.grid(
      alpha_0 = seq(1000, 50000, length.out = 5),
      alpha_1 = seq(-10, 10, length.out = 5),
      alpha_2 = seq(-10, 10, length.out = 5),
      scal = seq(1000, 70000, length.out = 5),
      shape1_0 = seq(0.1, 10, length.out = 5),
      shape1_1 = seq(0.1, 10, length.out = 5),
      shape2 = seq(0.1, 10, length.out = 5),
      phi = seq(-1, 1, length.out = 5)
    )
  )
)

# Fit Dagum model --------------------------------------------------------
fit_dagum_model <- function(data, model_func, start_grid) {
  fit_nls2 <- nls2(
    ES_pcap ~ model_func,
    data = data,
    start = start_grid,
    algorithm = "brute-force",
    na.action = na.exclude
  )

  start_vals <- as.list(coef(fit_nls2))

  fit_nlsLM <- nlsLM(
    ES_pcap ~ model_func,
    data = data,
    start = start_vals,
    control = nls.lm.control(maxiter = 500, ftol = 1e-8),
    na.action = na.exclude
  )

  return(fit_nlsLM)
}

# Run diagnostics --------------------------------------------------------
run_diagnostics <- function(fit, data) {
  data$predicted <- predict(fit, newdata = data)
  data$residuals <- data$ES_pcap - data$predicted

  ggplot(data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") +
    theme_minimal()
}

# Main pipeline ----------------------------------------------------------
run_dagum_pipeline <- function(data_path, variant_key = "dagum_v1") {
  load_required_packages()
  data <- preprocess_data(data_path)

  model_info <- dagum_model_registry[[variant_key]]
  model_func <- model_info$formula
  predictors <- model_info$predictors
  grid <- model_info$grid_fun()

  data_clean <- data[complete.cases(data[, predictors]), ]

  fit <- fit_dagum_model(data_clean, model_func, grid)

  cat("Model:", model_info$name, "\n")
  print(summary(fit))
  run_diagnostics(fit, data_clean)

  return(list(fit = fit, model = model_info$name))
}

# Run Analysis

run_dagum_pipeline(
  data_path = here("data", "data1_with_cooks_d.rds"),
  variant_key = "dagum_v2"
)
# Save the model fit
saveRDS(run_dagum_pipeline(
  data_path = here("data", "data1_with_cooks_d.rds"),
  variant_key = "dagum_v2"
), file = here("models", "dagum_model_v2.rds"))
# Save the model summary
saveRDS(
  summary(run_dagum_pipeline(
    data_path = here("data", "data1_with_cooks_d.rds"),
    variant_key = "dagum_v2"
  )$fit),
  file = here("models", "dagum_model_summary_v2.rds")
)
# Save the diagnostics plot
ggsave(
  filename = here("plots", "dagum_model_v2_diagnostics.png"),
  plot = run_dagum_pipeline(
    data_path = here("data", "data1_with_cooks_d.rds"),
    variant_key = "dagum_v2"
  )$fit,
  width = 10, height = 6
)
# Save the model fit object
