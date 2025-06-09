### This is a cleaned-up and modularized version of your original script.
### Includes all three logistic models and iterative forecasting with diagnostics.

# Load and install required packages ---------------------------------------------------
load_required_packages <- function(packages, ncpus = 4) {
  installed <- packages %in% rownames(installed.packages())
  if (any(!installed)) {
    install.packages(packages[!installed], dependencies = TRUE, Ncpus = ncpus)
  }
  invisible(lapply(packages, library, character.only = TRUE))
}

packages <- c(
  "dplyr", "tidyr", "purrr", "ggplot2", "nlme", "broom.mixed",
  "gt", "readr", "here", "stargazer", "plotly", "lme4", "car",
  "stringr", "knitr", "kableExtra", "nls2", "htmlwidgets", "viridis"
)
load_required_packages(packages, ncpus = 12)

# Load fonts safely (Windows only) ----------------------------------------------------
if (.Platform$OS.type == "windows" && file.exists("ShellMedium.ttf")) {
  extrafont::loadfonts(device = "win")
  showtext::font_add("ShellMedium", "ShellMedium.ttf")
  showtext::showtext_auto()
}

# Load and prepare data ---------------------------------------------------------------
prepare_data <- function(path) {
  readRDS(path) %>%
    group_by(country_id) %>%
    arrange(year) %>%
    mutate(
      lag_ES_pcap = lag(ES_pcap, order_by = year),
      Gini_01 = gini_case1 / 100,
      GDP_PPP_pcap_thousands = GDP_PPP_pcap / 1000
    ) %>%
    ungroup()
}

data <- prepare_data("data/all_data_wem_espcap_imputation_wem_urban.rds")

# Filter data --------------------------------------------------------------------------
filter_valid_countries <- function(data) {
  data %>%
    group_by(country_name) %>%
    filter(
      sum(!is.na(Gini_01)) >= 2,
      sum(!is.na(urbanization_perc)) >= 2,
      sum(!is.na(density_psqkm)) >= 2
    ) %>%
    ungroup()
}

data <- filter_valid_countries(data)

# Logistic models ----------------------------------------------------------------------
logistic_model_1 <- function(gdp, dens, a0, a1, xmid, scal) {
  (a0 + a1 * dens) / (1 + exp((xmid - gdp) / scal))
}

logistic_model_2 <- function(gdp, dens, gini, a0, a1, xmid, scal, b1) {
  base <- (a0 + a1 * dens)
  logistic <- 1 / (1 + exp((xmid - gdp) / scal))
  base * logistic^(b1 * gini)
}

logistic_model_3 <- function(gdp, dens, gini, a0, a1, xmid, scal, b1, b0) {
  base <- (a0 + a1 * dens)
  logistic <- 1 / (1 + exp((xmid - gdp) / scal))
  base * logistic^(b1 * gini + b0)
}

# Fit logistic models ------------------------------------------------------------------
fit_model <- function(model_fn, formula, data, start_grid) {
  fit_init <- nls2(formula, data = data, start = start_grid, algorithm = "brute-force")
  start_vals <- as.list(coef(fit_init))
  fit_final <- nlsLM(formula, data = data, start = start_vals, control = nls.lm.control(maxiter = 500))
  fit_final
}

# Prepare data for modeling ------------------------------------------------------------
data_model <- data %>% filter(year <= 2023)
data_model <- data_model[complete.cases(data_model[, c("ES_pcap", "GDP_PPP_pcap", "density_psqkm", "Gini_01")]), ]

# Fit all models -----------------------------------------------------------------------
grid1 <- expand.grid(a0 = seq(1000, 50000, 5), a1 = seq(-10, 10, 5), xmid = seq(10000, 70000, 5), scal = seq(1000, 70000, 5))
fit1 <- fit_model(logistic_model_1, ES_pcap ~ logistic_model_1(GDP_PPP_pcap, density_psqkm, a0, a1, xmid, scal), data_model, grid1)

grid2 <- expand.grid(a0 = seq(1000, 50000, 5), a1 = seq(-10, 10, 5), xmid = seq(10000, 70000, 5), scal = seq(1000, 70000, 5), b1 = seq(0, 5, 5))
fit2 <- fit_model(logistic_model_2, ES_pcap ~ logistic_model_2(GDP_PPP_pcap, density_psqkm, Gini_01, a0, a1, xmid, scal, b1), data_model, grid2)

grid3 <- expand.grid(a0 = seq(1000, 50000, 5), a1 = seq(-10, 10, 5), xmid = seq(10000, 70000, 5), scal = seq(1000, 70000, 5), b1 = seq(0, 5, 5), b0 = seq(0, 2, 5))
fit3 <- fit_model(logistic_model_3, ES_pcap ~ logistic_model_3(GDP_PPP_pcap, density_psqkm, Gini_01, a0, a1, xmid, scal, b1, b0), data_model, grid3)

# Summarize model fits -----------------------------------------------------------------
summary(fit1)
summary(fit2)
summary(fit3)

# Choose one for forecasting (or compare all) ------------------------------------------
fit <- fit3

# Forecasting loop remains the same as before ------------------------------------------
forecast_iteratively <- function(model, data, years = 2024:2100) {
  known_ids <- unique(model$m$groups$country_id)
  for (i in years) {
    data <- data %>%
      group_by(country_id) %>%
      arrange(year) %>%
      mutate(lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)) %>%
      ungroup()

    data_used <- filter(data, year == i)
    data_known <- filter(data_used, country_id %in% known_ids)
    data_new <- filter(data_used, !country_id %in% known_ids)

    preds_known <- if (nrow(data_known) > 0) predict(model, newdata = data_known, level = 1) else numeric(0)
    preds_new <- if (nrow(data_new) > 0) predict(model, newdata = data_new, level = 0) else numeric(0)

    pred_df <- bind_rows(
      data.frame(country_id = data_known$country_id, year = i, predicted_ES_pcap = preds_known),
      data.frame(country_id = data_new$country_id, year = i, predicted_ES_pcap = preds_new)
    )

    data <- data %>%
      left_join(pred_df, by = c("country_id", "year")) %>%
      group_by(country_id) %>%
      arrange(year) %>%
      mutate(
        ES_pcap = if_else(!is.na(predicted_ES_pcap), predicted_ES_pcap, ES_pcap),
        lag_ES_pcap = if_else(year == i, lag(ES_pcap), lag_ES_pcap)
      ) %>%
      ungroup() %>%
      select(-predicted_ES_pcap)
  }
  data
}

forecasted_data <- forecast_iteratively(fit, data)
