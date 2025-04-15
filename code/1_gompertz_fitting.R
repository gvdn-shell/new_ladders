# Remove all objects in R workspace (use with caution)
rm(list = ls())

# Check that required packages are installed and install if missing
ncpus <- 12
packages <- c("tidyverse",
              "ggplot2", 
              "nls2",
              "extrafont",
              "here",
              "gridExtra",
              "minpack.lm",
              "sysfonts",
              "showtext",
              "DBI","odbc","readxlsb","RODBC",
              "WDI",
              "openxlsx",
              "plotly",
              "plm",
              "nlme",
              "nls2",
              "flexmix",
              "fastDummies",
              "statforbiology",
              "conflicted",
              "saemix") #
installed_packages <- packages %in% rownames(installed.packages())

if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load required packages
invisible(lapply(packages, library, character.only = TRUE))
# Load the extrafont package
# font_import(paths = "C:/Windows/Fonts")

# Access the proxy settings from environment variables
http_proxy <- Sys.getenv("http_proxy")
Sys.setenv(http_proxy = http_proxy)
Sys.setenv(https_proxy = http_proxy)

# syntax: font_add(family = "<family_name>", regular = "/path/to/font/file")
font_add("ShellMedium", "ShellMedium.ttf")
font_families()

## automatically use showtext for new devices
showtext_auto()

#Plot: need to open Windows graphics device as showtext does not work well with RStudio built-in graphics device
# windows()

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

##################################################################################################################################
### Load data

all.data <- readRDS( here::here("data", "all_data_wem.rds"))
################################################################################
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

#############################################
#### ES versus log(GDP) plot looks good

filtered_data <- all.data %>%
  filter(year > 1970, country_id <= 40) %>%
  arrange(country_id, year) # Sort by country_id and year



# Join filtered_data with shell.brand.palette.hex to ensure correct color mapping
filtered_data <- filtered_data %>%
  left_join(shell.brand.palette.hex , by = c("country_id", "country_name")) %>%
  mutate(log_GDP_pcap = log(GDP_PPP_pcap)) %>%
  filter(!is.na(ES_pcap) & !is.na(log_GDP_pcap) & !is.infinite(ES_pcap) & !is.infinite(log_GDP_pcap))

summary(filtered_data)

#####################################

# Fit the Gompertz model to the entire dataset
gompertz_fit <- nls(ES_pcap ~ SSgompertz(log_GDP_pcap, Asym, b, c), 
                    data = filtered_data, 
                    #start = list(Asym = 1, b = 0.1, c = 0.1), 
                    control = nls.control(maxiter = 1000))
summary(gompertz_fit)

# Plot

# Create a data frame with the fitted values
fitted_values <- data.frame(log_GDP_pcap = filtered_data$log_GDP_pcap, 
                            ES_pcap = predict(gompertz_fit))

# Plot with the fitted Gompertz curve
p1 <- ggplot(filtered_data, aes(y = ES_pcap, x = log_GDP_pcap, colour = Hex)) +
  geom_point(aes(group = country_id)) +
  geom_line(aes(group = country_id)) +
  theme_bw() + create_theme(2) +
  geom_text(data = subset(filtered_data, !duplicated(country_name, fromLast = TRUE)),
            aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
            size = rel(8)) +
  theme(legend.position = "none") +
  labs(title = "Aggregate Energy Service Ladder: Passenger Transport Road Energy Service vs GDP", x = "GDP PPP (US$ 2018) (log)", y = "vehicle km/ capita/ year") +
  scale_color_identity() +
  # Add Gompertz line and add label saying Gompertz Model fit
  geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
  #annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
  #         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 

# Save p1 to png
ggsave(filename = here::here("plots", "pt_road_gdp_es.png"), plot = p1, width = 10, height = 6.3, dpi = 250)

## Prop TFC that is PTR
p3 <- ggplotly(ggplot(filtered_data, aes(y = prop_TFC_PTR, x = year, colour = Hex)) +
  geom_point(aes(group = country_id)) +
  geom_line(aes(group = country_id)) +
  theme_bw() + create_theme(2) +
  geom_text(data = subset(filtered_data, !duplicated(country_name, fromLast = TRUE)),
            aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
            size = rel(8)) +
  #theme(legend.position = "none") +
    # Ensure that legend displays the country names
  scale_color_manual(values = shell.brand.palette.hex$Hex, labels = shell.brand.palette.hex$country_id) +
    
  labs(title = "TFC in PT - Road as Proportion of Total TFC", x = "Year", y = "Proportion Total TFC") #+
  #scale_color_identity()
  ) 

p3


# Plot with country names in the legend and colour country_name label according to Hex

p3 <- ggplot(filtered_data, aes(y = prop_TFC_PTR, x = year, colour = Hex)) +
  geom_point(aes(group = country_id)) + #, colour = country_name
  geom_line(aes(group = country_id)) + #, colour = country_name
  theme_bw() + create_theme(2) +
  geom_text(data = subset(filtered_data, !duplicated(country_name, fromLast = TRUE)),
            aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
            size = rel(8)) +
  labs(title = "TFC in PT - Road as Proportion of Total TFC", x = "Year", y = "Proportion Total TFC") +
  scale_color_identity() 
  scale_color_manual(values = setNames(shell.brand.palette.hex$Hex, shell.brand.palette.hex$country_name))

# Convert to plotly
p3a <- ggplotly(p3)

p3

ggsave(filename = here::here("plots", "pt_road_prop_tf.png"), plot = p3, width = 10, height = 6.3, dpi = 250)

# save p3 to interactive html ggplotly

htmlwidgets::saveWidget(p3a, file = here::here("plots", "pt_road_prop_tfc.html"), selfcontained = TRUE)



ggplotly(p3)

View(filtered_data %>%
       filter(year == 1995 & prop_TFC_PTR > 27 & prop_TFC_PTR < 28)) 

View(filtered_data %>%
       filter(country_name == "Thailand")) 

###################################################################################################################################################
### New variable names still need to be incorporated below
####################################################################################################################################################

########
# Update below
########
panel_data <- all.data %>%
  rename(
    country = country_name,
    gini =SI.POV.GINI,
    GDP = GDP_PPP_pcap,
    density = EN.POP.DNST,
    urbanization = SP.URB.TOTL.IN.ZS,
    ) 

### Preprocessing of Dbar and Ubar
usa_values <- panel_data %>%
  filter(country == 'USA') %>%
  select(year, density, urbanization) %>%
  rename(density_USA = density, urbanization_USA = urbanization)

# Merge USA values with original data

all.data1 <- panel_data %>%
  left_join(usa_values, by = "year")


# Calculate Dbar and Ubar

panel_data <- all.data1 %>%
  ungroup() %>%
  mutate(Dbar = ifelse(density > density_USA, density - density_USA, 0),
         Ubar = ifelse(urbanization > urbanization_USA, urbanization - urbanization_USA, 0),
         Rising = ifelse(GDP > lag_GDP_PPP_pcap, 1, 0),
         Falling = ifelse(GDP < lag_GDP_PPP_pcap, 1, 0)) %>%
  arrange(country, year)

ggplot(panel_data, aes(y = energy_service, x = GDP, colour = country, group = country)) +
  geom_point() +
  #geom_smooth(method = "lm") +
  labs(title = "GDP vs Energy Service", x = "GDP", y = "Energy Service") +
  theme_minimal()

ggplot(panel_data, aes(y = energy_service, x = gini, colour = country, group = country)) +
  geom_point() +
  geom_line() +
  #geom_smooth(method = "lm") +
  labs(title = "Gini vs Energy Service", x = "Gini", y = "Energy Service") +
  theme_minimal()

ggplotly(ggplot(panel_data, aes(y = GDP, x = year, colour = country, group = country)) +
  geom_point() +
    geom_line() +
  #geom_smooth(method = "lm") +
  labs(title = "GDP over Time", x = "GDP", y = "Year") +
  theme_minimal())

#### EDA


p1 <- ggplot(data = panel_data, aes(x = (GDP), y = (ES_pcap), color = factor(country), group = factor(country))) +
  #facet_wrap(facets = vars(cluster)) +
  geom_line() +
  geom_point() +
  geom_text(data = subset(panel_data, !duplicated(country, fromLast = TRUE)),
            aes(label = country), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02))  +
  
  labs(color = "Country", group = "Country",
       title = "Countries by GDP per  capita and ES per capita",
       y = "Energy Service per capita (vehichle km/ capita/ year)",
       x = "GDP per capita (US$)") +theme_bw() + theme(
         axis.title.y = element_text(size = 18, family = "ShellMedium"),
         axis.title.x = element_text(size = 18, family = "ShellMedium"),
         axis.text.y = element_text(size = 16, family = "ShellMedium"),
         axis.text.x = element_text(size = 16, family = "ShellMedium"),
         plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
         legend.position = "none", #c(1.05, 0.9),
         legend.text = element_text(size = 18, family = "ShellMedium"),
         legend.title = element_blank(),
         legend.background = element_rect(fill = "white", color = "black"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray", linetype = "dashed"),
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 18, family = "ShellMedium", hjust = 0.5),  # Center-align the title
         plot.caption = element_blank(),
         plot.subtitle = element_blank()
       )


ggsave(filename = here::here("plots", "pt_road_gdp_es.png"), plot = p1, width = 12, height = 12, dpi = 150)

p1 <- ggplot(data = panel_data, aes(x = log(GDP), y = log(ES_pcap), color = factor(country), group = factor(country))) +
  #facet_wrap(facets = vars(cluster)) +
  geom_line() +
  geom_point() +
  geom_text(data = subset(panel_data, !duplicated(country, fromLast = TRUE)),
            aes(label = country), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02))  +
  
  labs(color = "Country", group = "Country",
       title = "Countries by GDP per  capita and ES per capita",
       y = "Energy Service per capita (vehichle km/ capita/ year) (log)",
       x = "GDP per capita (US$) (log)") +theme_bw() + theme(
         axis.title.y = element_text(size = 18, family = "ShellMedium"),
         axis.title.x = element_text(size = 18, family = "ShellMedium"),
         axis.text.y = element_text(size = 16, family = "ShellMedium"),
         axis.text.x = element_text(size = 16, family = "ShellMedium"),
         plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
         legend.position = "none", #c(1.05, 0.9),
         legend.text = element_text(size = 18, family = "ShellMedium"),
         legend.title = element_blank(),
         legend.background = element_rect(fill = "white", color = "black"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray", linetype = "dashed"),
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 18, family = "ShellMedium", hjust = 0.5),  # Center-align the title
         plot.caption = element_blank(),
         plot.subtitle = element_blank()
       )


ggsave(filename = here::here("plots", "pt_road_gdp_es_log.png"), plot = p1, width = 12, height = 12, dpi = 150)

p1 <- ggplot(data = panel_data, aes(x = (gini), y = log(ES_pcap), color = factor(country), group = factor(country))) +
  #facet_wrap(facets = vars(cluster)) +
  geom_line() +
  geom_point() +
  geom_text(data = subset(panel_data, !duplicated(country, fromLast = TRUE)),
            aes(label = country), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02))  +
  
  labs(color = "Country", group = "Country",
       title = "Countries by ES per  capita and Gini coefficient",
       y = "Energy Service per capita (vehichle km/ capita/ year) (log)",
       x = "Gini coefficient") +theme_bw() + theme(
         axis.title.y = element_text(size = 18, family = "ShellMedium"),
         axis.title.x = element_text(size = 18, family = "ShellMedium"),
         axis.text.y = element_text(size = 16, family = "ShellMedium"),
         axis.text.x = element_text(size = 16, family = "ShellMedium"),
         plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
         legend.position = "none", #c(1.05, 0.9),
         legend.text = element_text(size = 18, family = "ShellMedium"),
         legend.title = element_blank(),
         legend.background = element_rect(fill = "white", color = "black"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray", linetype = "dashed"),
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 18, family = "ShellMedium", hjust = 0.5),  # Center-align the title
         plot.caption = element_blank(),
         plot.subtitle = element_blank()
       )


ggsave(filename = here::here("plots", "pt_road_es_log_gini.png"), plot = p1, width = 12, height = 12, dpi = 150)

p1 <- ggplot(data = panel_data, aes(x = (gini), y = log(GDP), color = factor(country), group = factor(country))) +
  #facet_wrap(facets = vars(cluster)) +
  geom_line() +
  geom_point() +
  geom_text(data = subset(panel_data, !duplicated(country, fromLast = TRUE)),
            aes(label = country), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02))  +
  
  labs(color = "Country", group = "Country",
       title = "Countries by GDP per  capita and Gini coefficient",
       y = "GDP per capita (vehichle km/ capita/ year) (log)",
       x = "Gini coefficient") +theme_bw() + theme(
         axis.title.y = element_text(size = 18, family = "ShellMedium"),
         axis.title.x = element_text(size = 18, family = "ShellMedium"),
         axis.text.y = element_text(size = 16, family = "ShellMedium"),
         axis.text.x = element_text(size = 16, family = "ShellMedium"),
         plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
         legend.position = "none", #c(1.05, 0.9),
         legend.text = element_text(size = 18, family = "ShellMedium"),
         legend.title = element_blank(),
         legend.background = element_rect(fill = "white", color = "black"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray", linetype = "dashed"),
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 18, family = "ShellMedium", hjust = 0.5),  # Center-align the title
         plot.caption = element_blank(),
         plot.subtitle = element_blank()
       )


ggsave(filename = here::here("plots", "pt_road_gdp_log_gini.png"), plot = p1, width = 12, height = 12, dpi = 150)



#### Discard GDP outliers - discard data where countries have GDP > 100000 before 1980
panel_data2 <- panel_data #%>%
  #filter(!(GDP > 100000 & year < 1986))

ggplotly(ggplot(panel_data2, aes(y = GDP, x = year, colour = country, group = country)) +
           geom_point() +
           geom_line() +
           #geom_smooth(method = "lm") +
           labs(title = "GDP over Time", y = "GDP", x = "Year") +
           theme_minimal())

ggplotly(ggplot(panel_data2, aes(y = log(ES_pcap), x = log(GDP), colour = country, group = country)) +
           geom_point() +
           geom_line() +
           #geom_smooth(method = "lm") +
           labs(title = "ES pcap vs GDP", x = "GDP", y = "ES pcap") +
           theme_minimal())

ggplotly(ggplot(panel_data2, aes(y = log(ES_pcap), x = log(gini), colour = country, group = country)) +
           geom_point() +
           geom_line() +
           #geom_smooth(method = "lm") +
           labs(title = "ES pcap vs GDP", x = "Gini", y = "ES pcap") +
           theme_minimal())

###### Countries to exclude
# # #"Rest of" countries

# # #Oil & gas countries -- Funny Money GDPcap does not reflect wealth - GDP almost solely as a function of oil price

# # Define lists of countries to exclude
# countries_to_exclude1 <- c("Iran", "Iraq", "Saudi Arabia", "Kuwait", "Qatar", "Oman", "Algeria",
#                            "Syria", "Sudan", "Ecuador", "Yemen", "Angola", "Nigeria", "Venezuela")
# 
# # # From old ladder estimation
# countries_to_exclude2 <- c("Iran", "Malaysia", "Algeria", "Syria", "Sudan", "Ecuador", "Yemen",
#                            "Angola", "Nigeria", "Venezuela")
# 
# # #Former USSR, Eastern Europe, North Korea
# ussr.countries <- c("Russia", "Uzbekistan", "Belarus", "Poland", "Romania", "Ukraine", "Bulgaria",
#                     "Azerbaijan", "Kazakhstan", "Turkmenistan", "North Korea")
# 
# baltic.states <- c("Estonia", "Latvia", "Lithuania", "Baltic States")

# 
# Data_HI_Phs2 <- Data_HI_Phs2 %>% 
#   dplyr::filter(!(Country %in% countries_to_exclude1)) %>%
#   dplyr::filter(!(Country %in% ussr.countries & Year < 2000)) %>% # Remove USSR states before 1999
#   dplyr::filter(!(Country %in% baltic.states)) %>%
#   arrange(Country, Sector, Year)
# 
# Data_HI_Phs2.maturity <- Data_HI_Phs2.maturity %>%
#   dplyr::filter(Country != "International marine bunkers" & !grepl("Rest of", Country, ignore.case = TRUE)) %>% 
#   dplyr::filter(!(Country %in% countries_to_exclude1)) %>%
#   dplyr::filter(!(Country %in% ussr.countries & Year < 2000)) %>% # Remove USSR states before 1999
#   dplyr::filter(!(Country %in% baltic.states)) %>%
#   arrange(Country, Sector, Year)

###########################################################################################

# Fit initial Gompertz using nls to data to get initial values
# Fit the model using nls
model1 <- nls(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b, c),
              data = panel_data2,
              #start = list(Asym = 1, b = 0.1, c = 0.1),
              control = list(maxiter = 1000))



# Step 1: Define the panel_select data frame
panel_select <- panel_data2 %>%
  mutate(GDP = log(GDP), ES_pcap = log(ES_pcap)) %>%
  select(id = country_id, year, GDP , Dbar, Ubar, Rising, Falling, ES_pcap) %>%
  ungroup() %>%
  group_by(id) %>%
  arrange(id, year) %>%
  mutate(lag_ES_pcap = dplyr::lag(ES_pcap, 1)) %>%
  ungroup() %>%
  filter(complete.cases(.)) #%>%
  #filter(id %in% c(1:20), !id %in% c(15,16))

summary(panel_select)

ggplotly(ggplot(panel_select, aes(y = (ES_pcap), x = (GDP), colour = factor(id), group = factor(id))) +
           geom_point() +
           geom_line() +
           #geom_smooth(method = "lm") +
           labs(title = "ES pcap vs GDP", x = "GDP", y = "ES pcap") +
           theme_minimal())


gamma_max_usa <- max(panel_select$ES_pcap) * 1.1


glimpse(panel_select)
# Step 3: Create the saemix.data object
saemix.data <- saemixData(name.data = panel_select, header = TRUE, sep = ",", na = NA,
                          name.group = c("id"), name.predictors = c("GDP", "Dbar", "Ubar", "Rising", "Falling", "lag_ES_pcap"),
                          name.response = c("ES_pcap"), #name.covariates = c("country"),
                          units = list(x = c("GDP", "Dbar", "Ubar", "Rising", "Falling", "lag_ES_pcap"), y = "ES_pcap"),
                          name.X = "GDP")

# Step 2: Define the model1gately function - aligned with saemix data
model1gately <- function(beta, id, x) {
  gamma_max <- gamma_max_usa
  lambda <- beta[id, 1]
  phi <- beta[id, 2]
  theta_r <- beta[id, 3]
  theta_f <- beta[id, 4]
  alpha <- beta[id, 5]
  delta <- beta[id, 6]
  
  
  gdp <- x[, 2]
  dbar <- x[, 3]
  ubar <- x[, 4]
  rising <- x[,5]
  falling <- x[, 6]
  lagged_ES_pcap <- x[, 7]
  
  ypred <- (gamma_max + lambda * dbar + phi * ubar) *
    (theta_r * rising + theta_f * falling) *
    exp(alpha * exp(delta * gdp)) + (1 - theta_r * rising - theta_f * falling) * lagged_ES_pcap
  
  return(ypred)
}

# Step 4: Create the matrix_6x6 matrix
matrix_6x6 <- matrix(0, nrow = 6, ncol = 6)
matrix_6x6[6, 6] <- 1

# Use Gately for initial guesses

# Step 5: Define the saemix.model object with consistent dimensions
saemix.model <- saemixModel(model = model1gately, modeltype = "structural",
                            description = "Gately model with first-order absorption",
                            psi0 = matrix(c(-1, -1, 0.1, 0.1, -3, -0.2), ncol = 6, byrow = TRUE,
                                          dimnames = list(NULL, c("lambda", "phi", "theta_r", "theta_f", "alpha", "delta"))), # Initial guesses
                            transform.par = c(1, 1, 1, 1, 1, 1),
                            covariate.model = matrix(c(0, 0, 0, 0, 0, 0), ncol = 6, byrow = TRUE),
                            fixed.estim = c(1, 1, 1, 1, 1, 1),
                            covariance.model = matrix_6x6,
                            omega.init = diag(6), # Ensure omega.init is a square matrix
                            error.model = "constant")

# Check the saemix.model object

saemix_fit1    <- saemix(saemix.model, saemix.data)
psi <- psi(saemix_fit1)
print(psi)

# Create smart sampling range

summary(model1)

# Define the ranges for each parameter
lambda_range <- seq(-0.5*10, 0, by = 0.0001)
phi_range <- seq(-0.1*10, 0, by = 0.0001)
alpha_range <- seq(-15*10, 0, by = 0.05)
theta_r_range <- seq(0, 1*10, by = 0.01)
theta_f_range <- seq(0, 1*10, by = 0.01)
delta_range <- seq(-1*10, 0, by = 0.01)


# Set the number of samples
n_samples <- 1000

# Initialize a data frame to store the samples
samples <- data.frame(lambda = numeric(n_samples), phi = numeric(n_samples),
                      theta_r = numeric(n_samples), theta_f = numeric(n_samples), alpha = numeric(n_samples), delta = numeric(n_samples))

# Draw n samples
set.seed(42) # For reproducibility
for (i in 1:n_samples) {
  samples$lambda[i] <- sample(lambda_range, 1)
  samples$phi[i] <- sample(phi_range, 1)
  samples$theta_r[i] <- sample(theta_r_range, 1)
  samples$theta_f[i] <- sample(theta_f_range, 1)
  samples$alpha[i] <- sample(alpha_range, 1)
  samples$delta[i] <- sample(delta_range, 1)
}

# Display the first few rows of the samples
head(samples)

samples_matrix <- as.matrix(samples)

dimnames(samples_matrix) <- NULL


saemix.model <- saemixModel(model = model1gately, modeltype = "structural",
                            description = "Gately model with first-order absorption",
                            psi0 = matrix(samples_matrix[1,], ncol = 6, byrow = TRUE,
                                          dimnames = list(NULL, c("lambda", "phi", "theta_r", "theta_f", "alpha", "delta"))), # Initial guesses
                            transform.par = c(1, 1, 1, 1, 1, 1),
                            #covariate.model = matrix(c(0, 0, 0, 0, 0, 0), ncol = 6, byrow = TRUE),
                            fixed.estim = c(1, 1, 1, 1, 1, 1),
                            covariance.model = matrix_6x6,
                            omega.init = diag(6), # Ensure omega.init is a square matrix
                            error.model = "constant")

# Check the saemix.model object

saemix_fit1    <- saemix(saemix.model, saemix.data)

# # Define the range and increment for psi0
# psi0_range <- seq(-100, 100, by = 0.01)
# 
# # Sample a subset of combinations (e.g., 1000 random combinations)
# set.seed(42) # For reproducibility
# sampled_combinations <- replicate(6, sample(psi0_range, 10000, replace = TRUE))

# Initialize a variable to store the first successful model fit
successful_model <- NULL

# Loop over the sampled combinations and attempt to fit the model
for (i in 1:nrow(samples_matrix)) {
  psi0 <- samples_matrix[i, ]
  
  saemix.model <- saemixModel(model = model1gately, modeltype = "structural",
                              description = "Gately model with first-order absorption",
                              psi0 = matrix(psi0, ncol = 6, byrow = TRUE,
                                            dimnames = list(NULL, c("lambda", "phi", "theta_r", "theta_f", "alpha", "delta"))),
                              transform.par = c(1, 1, 1, 1, 1, 1),
                              covariate.model = matrix(c(0, 0, 0, 0, 0, 0), ncol = 6, byrow = TRUE),
                              fixed.estim = c(1, 1, 1, 1, 1, 1),
                              covariance.model = matrix_6x6,
                              omega.init = diag(6), # Ensure omega.init is a square matrix
                              error.model = "constant")
  saemix_options <- list(map=TRUE, fim=TRUE, ll.is=FALSE, 
                         displayProgress=FALSE, save=FALSE, seed=632545)
  
  # Attempt to fit the model
  saemix_fit <- try(saemix(saemix.model, saemix.data, saemix_options), silent = TRUE)
  
    # Check if the model fit was successful
  if (!inherits(saemix_fit, "try-error")) {
    successful_model <- saemix_fit
    print("Model fit successful with psi0 values:")
    print(psi0)
    break
    
  }
}


# Check if a successful model was found
if (!is.null(successful_model)) {
  print("First successful model fit stored.")
} else {
  print("No successful model fit found.")
}

#################################################################################################

###########################################################################################

# Fit the Gaussian mixture model using flexmix
set.seed(42)  # For reproducibility

ggplot(panel_data, aes(y = log(ES_pcap), x = log(GDP), colour = country, group = country)) +
  geom_point() +
  #geom_smooth(method = "lm") +
  labs(title = "GDP vs Energy Service", x = "GDP", y = "Energy Service") +
  theme_minimal()

# Normalize data for clustering - normalize GDP_PPP_pcap and ES_pcap

# library(caret)
# 
cluster_data <- panel_data
# 
# # Standardize before clustering
# cluster_data$GDP <- scale(cluster_data$GDP)
# cluster_data$energy_service <- scale(cluster_data$energy_service)
# 
# summary(cluster_data)

# # Define the clustering model
lcgaMix <- stepFlexmix(.~.|country_id,
  model = FLXMRglmfix(log(ES_pcap) ~ log(GDP), #  log(ES_pcap) ~ log(GDP) + year,
                      varFix=FALSE),
  data = cluster_data,
  k = 1:10,
  nrep = 200,
  control = list(iter.max = 500, minprior = 0)
)
# # 
saveRDS(lcgaMix, file = here("data", "pt_road_cluster_lcga.rds"))


lcgaMix <- readRDS(file = here::here("data", "pt_road_cluster_lcga.rds"))
# Extract the BIC values from the 'lcgaMix' object
bic_values <- BIC(lcgaMix)

# Plot the BIC values as a fit statistic curve
plot(1:10, bic_values, type = "b", xlab = "Number of Clusters (K)", ylab = "BIC Value",
     main = "BIC Value vs. Number of Clusters")

# Add a horizontal line to indicate the minimum BIC value
abline(h = min(bic_values), col = "red")

# Add a legend for the horizontal line
legend("topright", legend = "Optimal K", col = "red", lty = 1, cex = 0.8)

# Elbow at K = 3

lcga5 <- getModel(lcgaMix, which = 3)

# Add clusters to data_set

cluster.data2 <- cluster_data 

length(clusters(lcga5))
nrow(cluster.data2)

cluster.data2$cluster <- clusters(lcga5)

# Plot countries by clusters

# p1 <- ggplotly(ggplot(data = cluster.data2, aes(x = ESPrice_Avg, y = ES_pcap, color = factor(Country), group = factor(cluster))) +
#                  facet_wrap(facets = vars(cluster)) +
#                  geom_point() +
#                  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
#                  scale_y_continuous(trans = "sqrt"))
# 
# p1
# 
# htmlwidgets::saveWidget(p1, file = here::here("Ladder estimation", "Sectors", "Plots", "2_NFM_HI_phase2_maturity_mixture_patterns.html"), selfcontained = TRUE)


p1 <- ggplot(data = cluster.data2, aes(x = log(GDP), y = log(ES_pcap), color = factor(country), group = factor(cluster))) +
  facet_wrap(facets = vars(cluster)) +
  geom_line() +
  geom_point() +
  geom_text(data = subset(cluster.data2, !duplicated(country, fromLast = TRUE)),
            aes(label = country), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02)) +
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  
  labs(color = "Country", group = "Country",
       title = "Clusters of countries by GDP per capita and ES per capita",
       y = "log(Energy Service per capita)",
       x = "log(GDP per capita)") 

p1

p1a <- p1 + theme_bw() + theme(
  axis.title.y = element_text(size = 18, family = "ShellMedium"),
  axis.title.x = element_text(size = 18, family = "ShellMedium"),
  axis.text.y = element_text(size = 16, family = "ShellMedium"),
  axis.text.x = element_text(size = 16, family = "ShellMedium"),
  plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
  legend.position = "none", #c(1.05, 0.9),
  legend.text = element_text(size = 18, family = "ShellMedium"),
  legend.title = element_blank(),
  legend.background = element_rect(fill = "white", color = "black"),
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "gray", linetype = "dashed"),
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 18, family = "ShellMedium", hjust = 0.5),  # Center-align the title
  plot.caption = element_blank(),
  plot.subtitle = element_blank()
)

p1a

ggsave(filename = here::here("plots", "pt_road_mixture_k3_patterns.png"), plot = p1a, width = 12, height = 12, dpi = 150)
  
p2 <- ggplotly(p1 + theme(
  axis.title.y = element_text(size = 18, family = "ShellMedium"),
  axis.title.x = element_text(size = 18, family = "ShellMedium"),
  axis.text.y = element_text(size = 16, family = "ShellMedium"),
  axis.text.x = element_text(size = 16, family = "ShellMedium"),
  plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
  legend.position = c(1.05, 0.9),
  legend.text = element_text(size = 18, family = "ShellMedium"),
  legend.title = element_blank(),
  legend.background = element_rect(fill = "white", color = "black"),
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "gray", linetype = "dashed"),
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 18, family = "ShellMedium", hjust = 0.5),  # Center-align the title
  plot.caption = element_blank(),
  plot.subtitle = element_blank()
))

p2 

htmlwidgets::saveWidget(p2, file = here::here("plots", "pt_road_mixture_patterns.html"), selfcontained = TRUE)


###############################
result <- cluster.data2

result <- dummy_cols(result, select_columns = "cluster")

s.curve.data <- result %>%
  arrange(country, year, cluster) %>%
  ungroup()
# select(-starts_with("Pattern_Phase2_")) %>%
# mutate(Pattern_Phase2 = case_when(Country == "USA" | Country == "Australia" ~ 1, 
# Country == "Austria" ~ 2, TRUE ~ Pattern_Phase2)) %>%
# dummy_cols(select_columns = "Pattern_Phase2", remove_selected_columns = FALSE)

table(s.curve.data$cluster)

# S-curve estimation - Gompertz curve

Gompertz <- function(x, gamma, alpha, beta) {
  result <- gamma * exp(alpha * exp(beta * (x)))
  return(result)
}


# Scurve <- function(x, lower_asym, upper_asym, steepness, midpoint) {
#   result <- lower_asym + (upper_asym - lower_asym) / (1 + exp(-steepness * (x - midpoint)))
#   return(result)
# }

##### S-curve:
### Self-starting functions: https://www.statforbiology.com/2020/stat_nls_usefulfunctions/#modified-gompertz-function

# Specify the maximum number of iterations you want
max_iterations <- 1000

# Specify the tolerance value for convergence
tolerance_value <- 1e-6  # You can adjust this value as needed

# Specify the step factor for the optimization algorithm
step_factor <- 0.1  # You can adjust this value as needed

minfactor <- 1e-6

# Create a control list with the desired parameters
control_list <- list(maxiter = max_iterations, 
                     tol = tolerance_value, 
                     minFactor = minfactor,
                     factor = step_factor, algorithm = "lm")

s.curve.data <- s.curve.data %>% ungroup()

getMeanFunctions()

model <- nls(log(ES_pcap) ~ NLS.E4(log(GDP), b, c, d, e) , data = s.curve.data %>% filter(cluster == 1), 
             control = control_list, na.action = na.omit)

model <- drm(log(ES_pcap) ~log(GDP), fct = W1.4(), data  = s.curve.data %>% filter(cluster == 3), 
             na.action = na.omit)

summary(model) # check standard errors and significance as well as overall fit

plot(model, log ="", main = "Weibull 4-parameters (Cluster 3)")
# Use the control argument to pass the control list to nlsList
(m <- nls(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b2, b3) , data = s.curve.data, 
              control = control_list, na.action = na.omit))


#############################
### NLME model for random effects
data <- data.frame(
  country_id = rep(1:3, each = 5),
  ES_pcap_it = c(2.3, 2.8, 3.6, 4.5, 5.1, 2.1, 2.6, 3.4, 4.2, 4.8, 2.5, 3.0, 3.8, 4.6, 5.3),
  Gini_it = c(0.3, 0.32, 0.34, 0.36, 0.38, 0.31, 0.33, 0.35, 0.37, 0.39, 0.29, 0.31, 0.33, 0.35, 0.37),
  GDP_it = c(1.5, 1.6, 1.7, 1.8, 1.9, 1.4, 1.5, 1.6, 1.7, 1.8, 1.3, 1.4, 1.5, 1.6, 1.7)
)

# Define the model function
model_function <- function(Gini_it, GDP_it, gamma, alpha0, alpha1, beta) {
  gamma * exp((alpha0 + alpha1 * Gini_it) * exp(beta * GDP_it))
}
# Get initial guesses for parameters

(m <- nls(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b2, b3) , data = s.curve.data, 
          control = control_list, na.action = na.omit))

coefficients <- coef(m)

asym_start <- unlist(coefficients[1])

fit <- nlme(
  log(ES_pcap) ~ gamma * exp((alpha0 + alpha1 * gini) * exp(beta * log(GDP))),
  data = s.curve.data,
  fixed = gamma + alpha0 + alpha1 ~ 1,
  random = beta ~ 1 | country_id,
  na.action = na.omit,
  start = c(gamma = asym_start, alpha0 = 15.0892675, alpha1 = 0.6817013)
)

# Fit the model using nlme
fit <- nlme(
  log(ES_pcap) ~ model_function(gini, log(GDP), gamma, alpha0, alpha1, beta),
  data = s.curve.data,
  fixed = gamma + alpha0 + alpha1 ~ 1,
  random = beta ~ 1 | country_id,
  na.action = na.omit,
  start = c(gamma = 1, alpha0 = 0.1, alpha1 = 0.1)
)

# Summary of the model
summary(fit)



##############################

# Define the model function
model_function <- function(country_id, ES_pcap_it, Gini_it, GDP_it, gamma, alpha0, alpha1, beta) {
  gamma * exp((alpha0 + alpha1 * Gini_it) * exp(beta[country_id] * GDP_it))
}

# Initial parameter guesses
start_params <- list(gamma = 1, alpha0 = 0.1, alpha1 = 0.1)

# Fit the model
fit <- nls(log(ES_pcap) ~ model_function(country_id, ES_pcap, gini, log(GDP), gamma, alpha0, alpha1, beta),
           data = s.curve.data, start = start_params)

# Summary of the model
summary(fit)

(m <- nls(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b2, b3) , data = s.curve.data %>% filter(cluster == 1), 
          control = control_list, na.action = na.omit))
(m <- nls(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b2, b3) , data = s.curve.data %>% filter(cluster == 2), 
          control = control_list, na.action = na.omit))
(m <- nls(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b2, b3) , data = s.curve.data %>% filter(cluster == 3), 
          control = control_list, na.action = na.omit))

(m <- nlsList(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b2, b3) | cluster, data = s.curve.data, 
              control = control_list, na.action = na.omit))

# Assuming you have fitted the models using nlsList
control_list <- list(maxiter = 1000, tol = 1e-05, minFactor = 1e-8, factor = 0.1, algorithm = "port")

s.curve.data <- s.curve.data %>% ungroup()

m <- nlsList(log(ES_pcap) ~ SSgompertz(log(GDP), Asym, b2, b3) | cluster, 
             data = s.curve.data, 
             control = control_list, 
             na.action = na.omit)

# Extract fitted values
fitted_values <- fitted(m)

# Combine fitted values with original data
s.curve.data <- s.curve.data %>%
  mutate(fitted_ES_pcap = exp(fitted_values))

# Plot the fitted Gompertz function per cluster
ggplot(s.curve.data, aes(x = log(GDP), y = log(ES_pcap), color = as.factor(cluster))) +
  geom_point() +
  geom_line(aes(y = log(fitted_ES_pcap)), size = 1) +
  facet_wrap(~ cluster) +
  labs(title = "Fitted Gompertz Function per Cluster",
       x = "Log(GDP)",
       y = "Log(ES_pcap)",
       color = "Cluster") +
  theme_minimal()


(m <- nlsList(ES_pcap ~ SSlogis(GDP_pcap, Asym, xmid , scal) | cluster, data = s.curve.data, control = control_list))

(m <- nlsList(ES_pcap ~ SSlogis(GDP_pcap, Asym, xmid = m1.median.gdp, scal) | cluster, data = s.curve.data, control = control_list))


# Get clusters for Scenarios

df.patterns <- (s.curve.data) %>%
  dplyr::select(country_id, cluster) %>%
  ungroup() %>%
  group_by(country_id, cluster) %>%
  dplyr::slice(1)

complete.data <- data.frame(country_id = 1:100)

# Left join the complete data with the original data

all.countries <- complete.data %>%
  left_join(df.patterns, by = "country_id")

Data.to.excel <- all.countries %>%
  ungroup() %>% arrange(country_id) %>% 
  dplyr::select(cluster) %>%
  mutate_all(~if_else(is.na(.), "", as.character(.))) # Change NA to blank for Excel

# Convert the data frame to a tab-separated string
writeClipboard(capture.output(write.table(Data.to.excel, sep = "\t", quote = FALSE, 
                                          row.names = FALSE, col.names = FALSE)))

# Extract the first column of coefficients from 'all.coeffs' object
all.coeffs <- coef(m)
asym <- all.coeffs[, 1]
midpoint.x <- all.coeffs[, 2]
steepness <- 1/all.coeffs[, 3]

# Write the first column to the clipboard
write.table(asym, file = "clipboard", sep = "\t", col.names = FALSE, row.names = FALSE)
write.table(midpoint.x, file = "clipboard", sep = "\t", col.names = FALSE, row.names = FALSE)
write.table(steepness, file = "clipboard", sep = "\t", col.names = FALSE, row.names = FALSE)

























########################################################################################################################
# Randomly select 10 numbers from 1 to 79
set.seed(123) # For reproducibility
random_numbers <- sample(1:79, 25)
# Fit the model using nls to get initial estimates
panel_data1 <- panel_data %>%
  filter(country_id %in% random_numbers) %>%
  select(country_id, country, GDP, gini, Dbar, Ubar, energy_service, lag_energy_service) %>%
  filter(complete.cases(.)) %>%
  ungroup()

#### Read this for estimation
#### https://encyclopedia.pub/entry/626
#### https://rpubs.com/abbyhudak/nonlinreg

ggplot(panel_data1, aes(y = log(energy_service), x = log(GDP), colour = country, group = country)) +
  geom_point() +
  #geom_smooth(method = "lm") +
  labs(title = "GDP vs Energy Service", x = "GDP", y = "Energy Service") +
  theme_minimal()

###
summary(panel_data1)
panel_data1_df <- as.data.frame(panel_data1)
sum(is.infinite(panel_data1_df))
sum(is.na(panel_data1))
sum(is.infinite(panel_data1))

sapply(panel_data1, function(column) sum(is.infinite(column)))

panel_data1$log_GDP <- scale(log(panel_data1$GDP))
panel_data1$log_energy_service <- scale(log(panel_data1$energy_service))
model1 <- nls(log_energy_service ~ SSgompertz(log_GDP, Asym, b2, b3),
              data = panel_data1)

model1 <- nls(log(energy_service) ~ SSgompertz(log(GDP), Asym, b2, b3),
              data = panel_data1)

summary(model1) # Watch standard errors and significance


# Fit the model using nls with the self-starting function
model1 <- nls(energy_service ~ SSgompertz(GDP, gamma, alpha, beta),
              data = panel_data1,
              na.action = na.omit)

summary(model1) # Watch standard errors and significance

# Define the Gompertz function
gompertz_function <- function(x, gamma, alpha, beta) {
  gamma * exp(alpha * exp(beta * log(x)))
}

# Generate data points
x_values <- seq(1, 100000, by = 50)
y_values <- gompertz_function(x_values, gamma = 6062, alpha = -3.424, beta = -1)

# Create a data frame for plotting
data <- data.frame(x = x_values, y = y_values)

# Plot the Gompertz function
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  labs(title = "Gompertz Function",
       x = "X",
       y = "Y") +
  theme_minimal()








# Define the self-starting Gompertz function with '...' argument
SSgompertz <- selfStart(
  function(GDP, gamma, alpha, beta, ...) {
    gamma * exp(alpha * exp(beta * log(GDP)))
  },
  initial = function(mCall, LHS, data, ...) {
    gamma <- max(data$energy_service, na.rm = TRUE)
    alpha <- -1
    beta <- -2
    setNames(c(gamma, alpha, beta), c("gamma", "alpha", "beta"))
  },
  parameters = c("gamma", "alpha", "beta")
)

# Fit the model using nlsLM with the self-starting function
nls_model <- nlsLM(
  energy_service ~ SSgompertz(GDP, gamma, alpha, beta),
  data = panel_data1,
  control = nls.lm.control(maxiter = 1000, maxfev = 10000),
  na.action = na.omit
)

summary(nls_model)

# Simple NLS
start_params <- c(gamma = 5000, alpha = -1, beta = -2)
nls_model <- nlsLM(
  energy_service ~ gamma * (exp(alpha * exp(beta * log(GDP)))),
  data = panel_data1,
  start = start_params,
  na.action = na.omit
)



model1 <- nls(
  energy_service ~ gamma * (exp(alpha * exp(beta * log(GDP)))),
  data = panel_data1,
  start = start_params,
  na.action = na.omit
)

model1 <- nlme(
  energy_service ~ gamma * (exp(alpha * exp(beta * log(GDP)))),
  data = panel_data1,
  fixed = gamma + alpha ~ 1,
  random = beta ~ 1 | country_id,
  start = start_params,
  na.action = na.omit
)




nls_model <- nls(
  energy_service ~ (gamma_max + lambda * Dbar + phi * Ubar) * (exp((alpha0 + alpha1 * gini) * exp(beta_i * log(GDP)))) +
    (1 - lambda * Dbar - phi * Ubar) * lag_energy_service,
  data = panel_data1,
  start = list(gamma_max = 5000, lambda = 1, phi = 1, theta_R = 1, theta_F = 1, alpha0 = 1, alpha1 = 1, beta_i = 1),
  na.action = na.omit
)

# Fit the model using nlsLM to get initial estimates
nls_model <- nlsLM(
  energy_service ~ (gamma_max + lambda * Dbar + phi * Ubar) * (exp((alpha0 + alpha1 * gini) * exp(beta_i * log(GDP)))) +
    (1 - lambda * Dbar - phi * Ubar) * lag_energy_service,
  data = panel_data1,
  start = list(gamma_max = 5000, lambda = 1, phi = 1, theta_R = 1, theta_F = 1, alpha0 = 1, alpha1 = 1, beta_i = 1)
)

# Extract initial parameter estimates from nls
start_params <- coef(nls_model)

# Fit the model using nlme with initial estimates from nls
model1 <- nlme(
  energy_service ~ (gamma_max + lambda * Dbar + phi * Ubar) * (exp((alpha0 + alpha1 * gini) * exp(beta_i * log(GDP)))) +
    (1 - lambda * Dbar - phi * Ubar) * Y_it_pred_lag,
  data = panel_data,
  fixed = gamma_max + lambda + phi + theta_R + theta_F + alpha0 + alpha1 ~ 1,
  random = beta_i ~ 1 | country,
  start = start_params,
  na.action = na.omit
)

summary(model1)






# Initial parameter estimates
n.countries <- length(unique(panel_data$country))
start_params <- c(gamma_max = 5000, lambda = 1, phi = 1, theta_R = 1, theta_F = 1, alpha0 = 1, alpha1 = 1)

# Fit the model
model1 <- nlme(
  energy_service ~ (gamma_max + lambda * Dbar + phi * Ubar) * (exp ((alpha0 + alpha1 * gini) * exp(beta_i * log(GDP)))) +
    (1 - lambda * Dbar - phi * Ubar) * lag(energy_service), 
  data = panel_data,
fixed = gamma_max + lambda + phi + theta_R + theta_F + alpha0 + alpha1 ~ 1,
random = beta_i ~ 1 | country,
start = start_params,
na.action = na.omit
)

summary(model1)















gompertz_model <- function(params, data) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha0 <- params[6]
  alpha1 <- params[7]
  beta_i <- params[8:length(params)]
  
  # Calculate the Gompertz model
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  alpha <- alpha0 + alpha1 * data$gini
  exp_term <- exp(alpha * exp(beta_i * log(data$GDP)))
  
  Y_it_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - theta_R * data$Rising - theta_F * data$Falling) * data$Y_it_pred_lag
  
  return(Y_it_pred)
}

# Example data
panel_data <- data.frame(
  country = factor(rep(1:10, each = 10)),
  GDP = runif(100, 1, 100),
  gini = runif(100, 0.2, 0.5),
  Dbar = runif(100, 1, 10),
  Ubar = runif(100, 1, 10),
  Rising = runif(100, 0, 1),
  Falling = runif(100, 0, 1),
  energy_service = runif(100, 1, 100)
)

# Add lagged values of Y_it_pred
panel_data <- panel_data %>%
  group_by(country) %>%
  mutate(Y_it_pred_lag = lag(energy_service))

# Initial parameter estimates
start_params <- c(gamma_max = 1, lambda = 1, phi = 1, theta_R = 1, theta_F = 1, alpha0 = 1, alpha1 = 1, beta_i = rep(1, 10))

# Fit the model
model1 <- nlme(
  energy_service ~ (gamma_max + lambda * Dbar + phi * Ubar) * (exp ((alpha0 + alpha1 * gini) * exp(beta_i * log(data$GDP)))), 
  panel_data),
  data = panel_data,
  fixed = gamma_max + lambda + phi + theta_R + theta_F + alpha0 + alpha1 ~ 1,
  random = beta_i ~ 1 | country,
  start = start_params
)

summary(model1)

















gompertz_model <- function(params, data, Y_it_pred_lag) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha0 <- params[6]
  alpha1 <- params[7]
  beta_i <- params[8:length(params)]
  
  # Calculate the Gompertz model
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  alpha <- alpha0 + alpha1 * data$gini
  exp_term <- exp(alpha * exp(beta_i * log(data$GDP)))
  
  Y_it_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - theta_R * data$Rising - theta_F * data$Falling) * Y_it_pred_lag
  
  return(Y_it_pred)
}

start_params <- c(gamma_max = 1, lambda = 1, phi = 1, Theta_R = 1, Theta_F = 1, alpha0 = 1, alpha1 = 1)


model1 <- nlme(
  energy_service ~ gompertz_model(params, panel_data),
  data = panel_data,
  fixed = gamma_max + lambda + phi + theta_R + theta_F + alpha0 + alpha1 ~ 1,
  random = beta_i ~ 1 | country,
  start = list(params = start_params)
)


# Specify nonlinear model with mixed effects (random effects for countries)



gompertz_model <- function(params, data, Y_it_pred_lag) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha0 <- params[6]
  alpha1 <- params[7]
  beta_i <- params[8:length(params)]
  
  # Calculate the Gompertz model
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  alpha <- alpha0 + alpha1 * data$gini
  exp_term <- exp(alpha * exp(beta_i * log(data$GDP)))
  
  Y_it_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - theta_R * data$Rising - theta_F * data$Falling) * Y_it_pred_lag
  
  return(Y_it_pred)
}

gompertz_model <- function(params, data) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha0 <- params[6]
  alpha1 <- params[7]
  beta_i <- params[8:length(params)]

  # Calculate the Gompertz model
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  alpha <- alpha0 + alpha1 * data$gini
  exp_term <- exp(alpha * exp(beta_i * log(data$GDP)))
  
  Y_it_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - theta_R * data$Rising - theta_F * data$Falling) * lag(data$Y_it_pred)
  
  return(Y_it_pred)

}




########################################################################################################################
# Define the nonlinear model function
gompertz_model <- function(params, data) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  Theta_R <- params[4]
  Theta_F <- params[5]
  alpha0 <- params[6]
  alpha1 <- params[7]
  beta_i <- params[8]
  
  Dbar <- 1 # Example value, replace with actual data
  Ubar <- 1 # Example value, replace with actual data
  R_it <- 1 # Example value, replace with actual data
  F <- 1 # Example value, replace with actual data
  
  alpha <- alpha0 + alpha1 * Gini_Coefficient
  
  (gamma_max + lambda * Dbar + phi * Ubar) * (Theta_R * R_it + Theta_F * F) * exp(alpha * exp(beta_i * GDP_it))
}

# Example data
data <- data.frame(
  country = factor(rep(1:10, each = 10)),
  GDP_it = runif(100, 1, 100),
  Gini_Coefficient = runif(100, 0.2, 0.5),
  Y_it = runif(100, 1, 100)
)

# Initial parameter estimates
start_params <- c(gamma_max = 1, lambda = 1, phi = 1, Theta_R = 1, Theta_F = 1, alpha0 = 1, alpha1 = 1)

# Fit the model
fit <- nlme(
  Y_it ~ gompertz_model(c(gamma_max, lambda, phi, Theta_R, Theta_F, alpha0, alpha1, beta_i), GDP_it, Gini_Coefficient),
  data = data,
  fixed = gamma_max + lambda + phi + Theta_R + Theta_F + alpha0 + alpha1 ~ 1,
  random = beta_i ~ 1 | country,
  start = start_params
)

summary(fit)
########################################################################################################################


# Define the Gompertz model function
gompertz_model <- function(params, data) {
  gamma_max <- 5000
  lambda <- params[1]
  phi <- params[2]
  alpha <- params[3]
  beta <- params[4:(length(params)-2)]
  theta_R <- params[length(params)-1]
  theta_F <- params[length(params)]
  
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  exp_term <- exp(alpha * exp(beta[as.numeric(as.factor(data$country))] * data$GDP))
  energy_service_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - (theta_R * data$Rising + theta_F * data$Falling)) * data$lag_energy_service
  
  return(energy_service_pred)
}

# Initial parameter values
initial_params <- c(-0.000388, -0.007765, -5.897, rep(-0.15, length(unique(panel_data$country))), 0.095, 0.084)

# Fit the model with cleaned data
fit <- nls(
  energy_service ~ gompertz_model(params, panel_data),
  data = panel_data,
  start = list(params = initial_params)
)

# View the summary of the model fit
summary(fit)






######################################################################################

# Define the Gompertz model function
gompertz_model <- function(params, data) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  alpha <- params[4]
  beta <- params[5:length(params)]
  theta_R <- params[length(params) - 1]
  theta_F <- params[length(params)]
  
  saturation_level_it <- gamma_max + lambda * data$Dbar + phi * data$Ubar
  exp_term <- exp(alpha * exp(beta[as.numeric(as.factor(data$country))] * data$GDP))
  energy_service_pred <- saturation_level_it * (theta_R * data$Rising + theta_F * data$Falling) * exp_term + 
    (1 - (theta_R * data$Rising + theta_F * data$Falling)) * data$lag_energy_service
  
  return(energy_service_pred)
}

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, -5.897, rep(-0.15, length(unique(panel_data$country))), 0.095, 0.084)

# Fit the model
fit <- nls(
  energy_service ~ gompertz_model(params, panel_data),
  data = panel_data,
  start = list(params = initial_params)
)
####
sum(is.na(panel_data))
sum(is.infinite(panel_data))

# Drop rows with NA values in density, urbanization, energy_service, GDP, lag_GDP_PPP_pcap, lag_energy_service, Rising, Falling, Ubar, Dbar
panel_data1 <- panel_data %>%
  filter(!is.na(density) & !is.na(urbanization) & !is.na(energy_service) & 
           !is.na(GDP) & !is.na(lag_GDP_PPP_pcap) & !is.na(lag_energy_service) &
           !is.na(Rising) & !is.na(Falling) & !is.na(Ubar) & !is.na(Dbar))


fit <- nls(
  energy_service ~ gompertz_model(params, panel_data1),
  data = panel_data,
  start = list(params = initial_params)
)
# View the summary of the model fit
summary(fit)

# Example structure of your panel dataset
# panel_data <- data.frame(
#   country = rep(c("USA", "Canada", "Mexico", "Germany"), each = 20),
#   year = rep(2000:2019, times = 4),
#   GDP = rnorm(80, mean = 50000, sd = 10000),
#   density = rnorm(80, mean = 100, sd = 20),
#   urbanization = runif(80, min = 50, max = 90),
#   vehicle_ownership = rnorm(80, mean = 200, sd = 50)
# )

# View the first few rows of the dataset
head(panel_data)

gompertz_model <- function(params, data) {
  gamma_max <- params[1]
  lambda <- params[2]
  phi <- params[3]
  alpha <- params[4]
  beta <- params[5:length(params)]
  
  data <- data %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(
      #lag_vehicle_ownership = lag(vehicle_ownership),
      R_it = ifelse(GDP > lag(GDP), 1, 0),
      F_it = ifelse(GDP < lag(GDP), 1, 0)
    ) %>%
    ungroup()
  
  D_it <- data$density
  U_it <- data$urbanization
  D_USA <- mean(data$density[data$country == "USA"])
  U_USA <- mean(data$urbanization[data$country == "USA"])
  
  D_bar_it <- ifelse(D_it > D_USA, D_it - D_USA, 0)
  U_bar_it <- ifelse(U_it > U_USA, U_it - U_USA, 0)
  
  saturation_level_it <- gamma_max + lambda * D_bar_it + phi * U_bar_it
  
  exp_term <- exp(alpha * exp(beta[as.numeric(as.factor(data$country))] * data$GDP))
  vehicle_ownership_pred <- saturation_level_it * (params[6] * data$R_it + params[7] * data$F_it) * exp_term + (1 - (params[6] * data$R_it + params[7] * data$F_it)) * data$lag_vehicle_ownership
  
  return(vehicle_ownership_pred)
}

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, -5.897, rep(-0.15, length(unique(panel_data$country))), 0.095, 0.084)

# Fit the model
fit <- nls(
  vehicle_ownership ~ gompertz_model(params, panel_data),
  data = panel_data,
  start = list(params = initial_params)
)

# View the summary of the model
summary(fit)

# Extract estimated parameters
estimated_params <- coef(fit)

# View the estimated parameters
print(estimated_params)

############################################################################################################



# Define parameters of Gompertz function
fraction_saturation_higher <- 1.1
country_id_max_saturation <- 1
gamma_max <- fraction_saturation_higher * max(all.data.gompertz$energy_service[all.data.gompertz$country_id == country_id_max_saturation], na.rm = TRUE) 

### data frame in following format:
# country, year, density, GDP, lag(GDP), ES, lag(ES), urbanization

set.seed(123) # For reproducibility

# Define the Modified Gompertz according to Gately et. al (2006)

model_function <- function(params, data) {
  gamma_max_USA <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha <- params[6]
  beta_i <- params[7:length(params)]
  
  # Group by country and arrange by year
  data <- data %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(
      R_it = ifelse(GDP > lag(GDP), 1, 0),
      F_it = ifelse(GDP < lag(GDP), 1, 0),
      V_i_t_minus_1 = lag(V_it)
    )
  
  # Extract density values
  D_it <- data$density
  D_USA_t <- data$density[data$country == "USA" & data$year == data$year]
  
  # Extract urbanization values (assuming U_it and U_USA_t are similarly defined)
  U_it <- data$urbanization
  U_USA_t <- data$urbanization[data$country == "USA" & data$year == data$year]
  
  # Define Ubar_it and Dbar_it for country i at time t
  D_bar_it <- ifelse(D_it > D_USA_t, D_it - D_USA_t, 0)
  U_bar_it <- ifelse(U_it > U_USA_t, U_it - U_USA_t, 0)
  
  # Saturation levels defined by the USA
  saturation_level_it <- gamma_max_USA + lambda * D_bar_it + phi * U_bar_it
  
  # Define the exponential term
  exp_term <- exp(alpha * exp(beta_i[as.numeric(as.factor(data$country))] * data$GDP))
  V_it <- saturation_level_it * (theta_R * data$R_it + theta_F * data$F_it) * exp_term + (1 - (theta_R * data$R_it + theta_F * data$F_it)) * data$V_i_t_minus_1
  
  return(V_it)
}

#######################################################################





#######################################################################

# Define the objective function (sum of squared residuals)
objective_function <- function(params, data) {
  V_it_pred <- model_function(params, data)
  SSR <- sum((data$V_it - V_it_pred)^2)
  return(SSR)
}

# Example data frame
data <- data.frame(
  country = rep(c("USA", "CAN", "MEX"), each = 10),
  year = rep(1960:1969, times = 3),
  density = runif(30, 0, 100),
  GDP = runif(30, 1000, 50000),
  #GDP_i_t_minus_1 = runif(30, 1000, 50000),
  V_i_t_minus_1 = runif(30, 0, 1000),
  V_it = runif(30, 0, 1000),
  R_it = runif(30, 0, 1),
  F_it = runif(30, 0, 1),
  urbanization = runif(30, 0, 100),
  D_USA_t = runif(30, 0, 100),
  U_USA_t = runif(30, 0, 100)
)

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, 0.095, 0.084, -5.897, rep(-0.20, length(unique(data$country))))

# Optimize the parameters using iterative least squares
result <- optim(par = initial_params, fn = objective_function, data = data, method = "BFGS")

# Estimated parameters
estimated_params <- result$par
print(estimated_params)


#########################################################################################################################
# Define the model function
model_function <- function(params, data, q = 1.1) {
  # Extract parameters
  gamma_max_USA <- params[1]
  lambda <- params[2]
  phi <- params[3]
  theta_R <- params[4]
  theta_F <- params[5]
  alpha <- params[6]
  beta_i <- params[7:length(params)] # Beta_i for each country
  
  # Calculate gamma_max_USA based on the maximum V_it value for the USA
  gamma_max_USA <- max(data %>% filter(country == "USA") %>% pull(V_it)) * q
  
  # Ensure the data is in the correct format
  data <- data %>%
    mutate(D_it = ifelse(D_it > 0, D_it, 0))
  
  # Define Ubar_it and Dbar_it for country i at time t
  data <- data %>%
    mutate(
      D_bar_it = ifelse(D_it > D_USA_t, D_it - D_USA_t, 0),
      U_bar_it = ifelse(U_it > U_USA_t, U_it - U_USA_t, 0)
    )
  
  # Saturation levels defined by the USA
  data <- data %>%
    mutate(saturation_level_it = gamma_max_USA + lambda * D_bar_it + phi * U_bar_it)
  
  # Defining different responses to falling and rising incomes
  data <- data %>%
    mutate(
      R_it = ifelse(GDP_it > GDP_i_t_minus_1, 1, 0),
      F_it = ifelse(GDP_it < GDP_i_t_minus_1, 1, 0),
      theta = theta_R * R_it + theta_F * F_it
    )
  
  # Define the exponential term
  data <- data %>%
    mutate(exp_term = exp(alpha + beta_i[as.numeric(as.factor(country))] * log(GDP_it)))
  
  # Calculate V_it based on the modified Gompertz curve
  data <- data %>%
    mutate(V_it_pred = saturation_level_it * theta * exp_term + (1 - theta) * V_i_t_minus_1)
  
  return(data$V_it_pred)
}

# Define the objective function (sum of squared residuals)
objective_function <- function(params, data, q = 1.1) {
  V_it_pred <- model_function(params, data, q)
  SSR <- sum((data$V_it - V_it_pred)^2)
  return(SSR)
}

# Example data frame
data <- data.frame(
  country = rep(c("USA", "CAN", "MEX"), each = 10),
  year = rep(1960:1969, times = 3),
  D_it = runif(30, 0, 100),
  GDP_it = runif(30, 1000, 50000),
  GDP_i_t_minus_1 = runif(30, 1000, 50000),
  V_i_t_minus_1 = runif(30, 0, 1000),
  V_it = runif(30, 0, 1000),
  U_it = runif(30, 0, 100),
  D_USA_t = runif(30, 0, 100),
  U_USA_t = runif(30, 0, 100)
)

# Initial parameter values
initial_params <- c(852, -0.000388, -0.007765, 0.095, 0.084, -5.897, rep(-0.20, length(unique(data$country))))

# Optimize the parameters using iterative least squares
result <- optim(par = initial_params, fn = objective_function, data = data, method = "BFGS")

# Estimated parameters
estimated_params <- result$par
print(estimated_params)

# Plot the results
data$V_it_pred <- model_function(estimated_params, data)

library(ggplot2)
ggplot(data, aes(x = year)) +
  geom_line(aes(y = V_it, color = "Observed")) +
  geom_line(aes(y = V_it_pred, color = "Predicted")) +
  facet_wrap(~ country) +
  labs(title = "Observed vs Predicted V_it", y = "V_it") +
  theme_minimal()
