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
              "DBI","odbc","RODBC",
              "WDI",
              "openxlsx",
              "plotly", "conflicted",
              "zoo",
              "mgcv",
              "nlme") #
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
windows()

conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#####################################################################################################################
####################################################################################################

### Functions  
# Gapfill function: Use CAGR to fill in gaps between non-missing values
gapfill <- function(data, column_name) {
  
  # Extract the column of interest (e.g., Value)
  x <- data[[column_name]]
  
  # Scan for the first and last data point. Any "NA" in between is a gap.
  dataend = -1
  datastart = 0
  beginning = 1
  for(i in 1:length(x)) {
    if (!is.na(x[i]) & beginning == 1) { datastart = i }
    if (!is.na(x[i])) { dataend = i; beginning = 0 }
  }
  
  # This for-loop iterates through the series and scans for gaps.
  # If suddenly data becomes "NA", then the gap counter "gaps" is increased by one.    
  # Moreover, the variable "ingap" is set to 1. For every next "NA", the gap length 
  # counter "gaplength" is increased by 1.
  # If it encounters data while "ingap" = 1, then it knows the gap is ended. The gap  
  # length is stored in "gappattern" together with the value of the gap counter, and 
  # "ingap" is set to 0 again.
  ingap = 0
  gaps = 0
  gaplength = 0
  gappattern = c()
  if (dataend > datastart) {
    for (i in datastart:dataend) {
      if (ingap == 0 & is.na(x[i])) {
        ingap = 1
        gaps = gaps + 1
        gappattern = rbind(gappattern, c(i, 0))
        gaplength = gaplength + 1
      }
      if (ingap == 1 & is.na(x[i])) {
        gaplength = gaplength + 1
      }
      if (ingap == 1 & !is.na(x[i])) {
        ingap = 0
        gappattern[gaps, 2] = i - 1
        gaplength = 0         
      }        
    }
  }
  
  # This loop iterates through all the gaps that have been found. It starts by 
  # calculating the CAGR using the value to the left and right of the gap. 
  # Then it fills these up.
  if (length(gappattern) > 0) {
    for (i in 1:nrow(gappattern)) {
      gapstartvalue = x[gappattern[i, 1] - 1]
      gapendvalue = x[gappattern[i, 2] + 1]
      gapsize = (gappattern[i, 2] + 1) - (gappattern[i, 1] - 1)
      cagr = (gapendvalue / gapstartvalue)^(1 / gapsize)
      for (j in gappattern[i, 1]:gappattern[i, 2]) {
        x[j] = x[j - 1] * cagr
      }
    }
  }
  
  # Assign the modified column back to the data frame
  data[[column_name]] <- x
  
  return(data)
}

growth.rate <- function(df, value_col) {
  df %>%
    mutate(
      growth_rate_IGU.real = (.[[value_col]] - lag(.[[value_col]], 1)) / lag(.[[value_col]], 1) * 100
    )
}

growth.rate <- function(df, value_col) {
  new_col_name <- paste0("growth_rate_", value_col)
  df %>%
    mutate(
      !!new_col_name := (.[[value_col]] - lag(.[[value_col]], 1)) / lag(.[[value_col]], 1) * 100
    )
}

# # Function to calculate forward and backward extrapolation
calculate_extrapolation <- function(value, growth) {
  Reduce(
    function(prevval, ind) coalesce(value[ind], prevval * growth[ind]),
    row_number(), init = NA, accumulate = TRUE
  )[-1]
}

####################################################################################################
# Establish the database connection
# https://solutions.posit.co/connections/db/databases/microsoft-sql-server/
# # FROM HERE
# # Define the database connection parameters
sort(unique(odbcListDrivers()[[1]])) # Check for SQL drivers and names which are then input below
driver <- "ODBC Driver 18 for SQL Server"  # Update with the appropriate driver name
# # Store as environmental variables to enhance security, e.g. Sys.setenv(DB_SERVER = "ABCD")
server <- Sys.getenv("DB_SERVER")  # Update with the server name
database <- Sys.getenv("DB_DATABASE")  # Update with the database name
uid <- Sys.getenv("DB_UID")
# 
conn <- DBI::dbConnect(odbc::odbc(),
                       Driver = driver,
                       Server = server,
                       Database = database,
                       Authentication = "ActiveDirectoryInteractive",
                       UID = uid,
                       autocommit = TRUE) # Azure MFA authentication
# 
# # Define a function to simplify querying the database
queryDB <- function(query) {
  dbGetQuery(conn, query)
}

# Query to get table names
table_names_query <- "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'"
table_names <- queryDB(table_names_query)

# View the table names
print(table_names)
#####################################################################################################################
### Get data

#---LOAD DATA---------------------------
run_id <- 197         # Isolate run interested in - check run's table
max_year <- 2100 #2024  # Get historic data only
max_sector_id <- toString(c(14))  # Focus on PT - Road
run_id <- paste0(run_id)  # Convert run_id to string for SQL query
# String so that SQL can understand it

# Country name data
countries.query <- paste0(
  "SELECT [country_id], [country_name]",
  " FROM [WEMv3_1].[countries]"
)
countries.data <- queryDB(countries.query)
 
# # Energy Service data: Units as per WEM
enerserv.query <- paste0(
  "SELECT [run_id], [sector_id], [carrier_id], [country_id], [year], [energy_service]",
  " FROM [WEMv3_1].[energy_service]",
  " WHERE [year] <= ", max_year,
  " AND [run_id] = ", run_id, #(SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])",
  " AND [sector_id] IN (", max_sector_id, ")"
)
enerserv.data <- queryDB(enerserv.query) 

# Sum over all carriers
enerserv.data <- enerserv.data %>%
  group_by(country_id, year, sector_id, run_id) %>%
  summarise(energy_service = sum(energy_service)) %>%
  ungroup() %>%
  as_tibble()

# # Energy demand data (TFC1): Units as per WEM (Sector by carrier)/ TFC2 Carrier by source
tfc.enerdem.query <- paste0(
  "SELECT [run_id], [data_type], [sector_id], [carrier_id], [country_id], [year], [demand]",
  " FROM [WEMv3_1].[energy_demand]",
  " WHERE [year] <= ", max_year,
  " AND [run_id] = ", run_id, #(SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])", # Ensure consistency of run_id selected
  " AND [data_type] = 'TFC1'"#,
  #" AND [sector_id] IN (", max_sector_id, ")"
)
tfc.enerdem.data <- queryDB(tfc.enerdem.query)

summary(tfc.enerdem.data)

table(tfc.enerdem.data$data_type)
# 
# # Population and GDP data
gdp.pop.query <- paste0(
  "SELECT [run_id], [category], [data_type], [country_id], [year], [value]",
  " FROM [WEMv3_1].[other_data]",
  " WHERE [year] <= ", max_year,
  " AND [run_id] = ", run_id, #(SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])",
  " AND ([data_type] = 'GDP_PPP' OR [data_type] = 'Population')"#,
  #" AND [run_id] = ", run_id
)
gdp.pop.data <- queryDB(gdp.pop.query)

# 
table(unique(gdp.pop.data$data_type))
# # Energy carrier prices including taxes: USD/GJ
# price.query <- paste0(
#   "SELECT [run_id], [data_type], [sector_id], [carrier_id], [source_id], [country_id], [year], [price]",
#   " FROM [WEMv3_1].[prices]",
#   " WHERE [year] <= ", max_year,
#   " AND [run_id] = (SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])",
#   #" AND [run_id] = ", run_id,
#   " AND [sector_id] IN (", max_sector_id, ")"
# )
# price.data <- queryDB(price.query)
# 
# #table(price.data$data_type)
# 
# # Natural resources data
# # supply.source.query <- paste0(
# #   "SELECT [run_id], [data_type], [source_id], [country_id], [year], [value]",
# #   " FROM [WEMv3_1].[supply]",
# #   " WHERE [year] <= ", max_year,
# #   #" AND [run_id] = ", run_id,
# #   " AND [source_id] = 11" # Traditional biomass
# # )
# # supply.source <- queryDB(supply.source.query)
# #
# # # Natural resources hydro data
# # # NOTE: IS THIS THE CORRECT TABLE? ### ALSO possible to use TFC2
# # # combined demand: TFC1 cross-table: Sectors x carriers --- carriers to sectors: TFC2 flow from sources to carriers: No direct link
# # # Dimensionality 18 sources x 10 carriers (TFC1), 10 carriers x 19 sectors (TFC2)
# # # combined: 18 sources x 10 carriers x 19 sectors [who buys PTL aviation]
# # # TFC2 slightly faster - just consider
# # supply.source.hydro.query <- paste0(
# #   "SELECT [run_id], [sector_id], [carrier_id], [source_id], [country_id], [year], [demand]",
# #   " FROM [dbo].[combined_energy_demand]",
# #   " WHERE [year] <= ", max_year,
# #   " AND [run_id] = ", run_id,
# #   " AND [source_id] = 5",
# #   " AND [carrier_id] = 4"
# # )
# # supply.source.hydro <- queryDB(supply.source.hydro.query)
# 
# # Close the database connection
dbDisconnect(conn)

#####################################################################################################################
### Import country names and mappings
#Specify the path to your Excel file
# excel_file <- here::here("data", "Shell WEM - A Settings v3.4.1.xlsx")
# 
# # Create a vector of named regions
# named_regions <- c("A_WEM_Countries_Mapped",
#                    "A_IEA_Countries_Mapped",
#                    "A_WEM_Top_100_Classification_Mapped",
#                    "A_WEM_Regions_Mapped",
#                    "A_ISO_3_letter_code_Mapped")
# 
# # Read each named region into a list of data frames
# country_mappings_list <- lapply(named_regions, function(region) {
#   # Read data without column names
#   df <- read.xlsx(excel_file, namedRegion = region, skipEmptyRows = FALSE, colNames = FALSE)
# 
#   # Assuming each region results in one main data column
#   # Set the name of the column to the region name
#   # If there are multiple columns, consider appending index or specific identifiers
#   colnames(df) <- region
# 
#   return(df)
# })

#### Obtaining WEM urbanization data and LR prices

excel_file1 <- here::here("data", "Shell WEM - B Historic Data v3.4.1.xlsx")
excel_file2 <- here::here("data", "Shell WEM - C Scenario Inputs v3.4.1.xlsx")
excel_file3 <- here::here("data", "Gini v2.xlsx")
# Create a vector of named regions
named_regions1 <- c("B_Historic_urbanisation_by_year")
named_regions2 <- c("C_Projections_urbanisation_Set_1")

# Define named regions and corresponding case labels
named_regions <- c("B_Gini_set_1", "B_Gini_set_2", "B_Gini_set_3")
case_labels <- c("gini_case1", "gini_case2", "gini_case3")

# Function to read and transform each sheet
read_gini_case <- function(region, case_label) {
  df <- read.xlsx(excel_file3, namedRegion = region, skipEmptyRows = FALSE, colNames = FALSE) * 100
  colnames(df) <- 1960:2100
  
  df %>%
    mutate(country_id = row_number()) %>%
    pivot_longer(-country_id, names_to = "year", values_to = case_label) %>%
    mutate(year = as.numeric(year))
}

# Read and process all cases
gini_dfs <- map2(named_regions, case_labels, read_gini_case)

# Merge all data frames on country_id and year
gini_combined <- reduce(gini_dfs, full_join, by = c("country_id", "year"))

# View result
print(head(gini_combined))

# Plot the Gini coefficient for each case (facetted by case) and coloured by country_id
# ggplot(gini_combined, aes(x = year)) +
#   geom_line(aes(y = gini_case1, color = as.factor(country_id)), size = 0.5) +
#   geom_line(aes(y = gini_case2, color = as.factor(country_id)), size = 0.5, linetype = "dashed") +
#   geom_line(aes(y = gini_case3, color = as.factor(country_id)), size = 0.5, linetype = "dotted") +
#   labs(title = "Gini Coefficient Over Time by Case",
#        x = "Year",
#        y = "Gini Coefficient",
#        color = "Country ID") +
#   theme_minimal() +
#   facet_wrap(~ country_id, scales = "free_y") +
#   theme(legend.position = "none")


####################################################################################

# Read each named region into a list of data frames
urban_mappings_list <- lapply(named_regions1, function(region) {
  # Read data without column names
  df <- read.xlsx(excel_file1, namedRegion = region, skipEmptyRows = FALSE, colNames = FALSE) * 100 # To percentages
  
  # Set column names to the year
  colnames(df) <- 1960:2030
  
  # Add a column for country_id
  df <- df %>%
    mutate(country_id = row_number()) %>%
    select(country_id, everything())
  # Pivot keeping country_id as column make make year columns rows and call the value Urban_perc
  df <- df %>%
    pivot_longer(-country_id, names_to = "year", values_to = "wem_urbanization_perc") %>%
    mutate(year = as.numeric(year)) %>%
    arrange(country_id, year) %>%
    as_tibble()
  
  return(df)
})

urban_mappings_future_list <- lapply(named_regions2, function(region) {
  # Read data without column names
  df <- read.xlsx(excel_file2, namedRegion = region, skipEmptyRows = FALSE, colNames = FALSE) * 100 # To percentages
  
  # Set column names to the year
  colnames(df) <- 2007:2100
  # Add a column for country_id
  df <- df %>%
    mutate(country_id = row_number()) %>%
    select(country_id, everything())
  # Pivot keeping country_id as column make make year columns rows and call the value Urban_perc
  df <- df %>%
    pivot_longer(-country_id, names_to = "year", values_to = "wem_urbanization_perc") %>%
    mutate(year = as.numeric(year)) %>%
    arrange(country_id, year) %>%
    as_tibble() %>%
    filter(year > 2030)
  return(df)
})

# Stack two lists
urban_mappings_list_all <- bind_rows(urban_mappings_list, urban_mappings_future_list) %>%
  # Remove duplicates
  distinct() %>%
  arrange(country_id, year) %>%
  as_tibble()

 
# # Combine all data frames into one by columns
# all.country.mappings <- as_tibble(bind_cols(country_mappings_list) %>%
#   mutate(iso3c = toupper(A_ISO_3_letter_code_Mapped)) %>%
#   select(-A_ISO_3_letter_code_Mapped))
# 
# saveRDS(all.country.mappings, here::here("data", "all_country_mappings.rds"))

all.country.mappings <- readRDS(here::here("data", "all_country_mappings.rds"))

# land_mappings_list <- lapply("A_Land_Area", function(region) {
#   # Read data without column names
#   df <- read.xlsx(excel_file, namedRegion = region, skipEmptyRows = FALSE, colNames = FALSE)
#   
#   # Assuming each region results in one main data column
#   # Set the name of the column to the region name
#   # If there are multiple columns, consider appending index or specific identifiers
#   colnames(df) <- region
#   
#   return(df)
# })
# 
# land_mappings_list <- bind_rows(land_mappings_list) %>%
#   # Add row numbers as a new column and call country_id
#   mutate(country_id = row_number())
# 
# saveRDS(land_mappings_list, here::here("data", "land_mappings.rds"))

land_mappings <- readRDS(here::here("data", "land_mappings.rds"))

#####################################################################################################################
### Focus on actual countries of WEM 100
# Merge all.country.mappings with countries.data
countries.data.iso <- countries.data %>%
  left_join(all.country.mappings, by = c("country_name" = "A_WEM_Countries_Mapped")) %>%
  select(country_id, country_name, iso3c, wem_regions = A_WEM_Regions_Mapped) %>%
  mutate(country_name = ifelse(is.na(country_name), country_id, country_name)) %>%
  #filter(complete.cases(.)) %>% # Activate this if want to only focus on actual countries with ISO
  as_tibble()

# Pull unique values in iso3c column from countries.data.iso
unique_iso3c <- unique(countries.data.iso$iso3c)

#####################################################################################################################
### Get other variables from WDI
# Gini coefficient: SI.POV.GINI
# Population density: EN.POP.DNST
# Urbanization rate: SP.URB.TOTL.IN.ZS

# Define the indicators
indicators <- c("SI.POV.GINI", "EN.POP.DNST", "SP.URB.TOTL.IN.ZS")

# Download the data and only for iso3 in unique_iso3c
# Filter for the countries of interest
data <- WDI(indicator = indicators, start = 1960, end = 2024, extra = TRUE) %>%
  filter(iso3c %in% unique_iso3c) %>% # arrange by country and year
  arrange(country, year) %>%
  select(country, iso3c, year, SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, income) 
# View the first few rows of the data
head(data)

ggplot(data, aes(x=year, y = SI.POV.GINI, color = country, group = country)) +
  geom_line() +
  labs(title = "Gini Coefficient Over Time", x = "Year", y = "Gini Coefficient") +
  theme_bw() +
  theme(legend.position = "none") 

#### Join with countries.data.iso
wdi.data <- data %>%
  left_join(countries.data.iso, by = c("iso3c" = "iso3c")) %>%
  select(country_id, country_name, wem_regions, iso3c, year, SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, income) %>%
  mutate(SI.POV.GINI = as.numeric(SI.POV.GINI),
         EN.POP.DNST = as.numeric(EN.POP.DNST),
         SP.URB.TOTL.IN.ZS = as.numeric(SP.URB.TOTL.IN.ZS)) %>%
  #filter(complete.cases(.)) %>%
  arrange(country_name, year) %>%
  as_tibble()

#######

head(tfc.enerdem.data)
tfc.enerdem.data1 <- tfc.enerdem.data %>%
  # Group by country_id and year and then calculate new variable called prop TFC
  # which is the value of the sum of demand where sector_id = 14 (PT) divided by the sum of demand over all sector_id
  group_by(country_id, year) %>%
  summarise(prop_TFC_PTR = sum(demand[sector_id == 14], na.rm = TRUE) / sum(demand, na.rm = TRUE) *100 ) %>%
  ungroup() %>%
  as_tibble() 

# Plot line plot of the various prop_TFC by country_id over year
# ggplot(tfc.enerdem.data1, aes(x = year, y = prop_TFC, color = as.factor(country_id))) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Proportion of TFC by Country that is Passenger Transport Road", x = "Year", y = "Proportion of TFC") +
#   theme_bw() +
#   theme(legend.position = "none")

#### Merge with energy service data
all.data <- wdi.data %>%
  right_join(enerserv.data, by = c("country_id" = "country_id", "year" = "year")) %>%
  #left_join(enerserv.data, by = c("country_id" = "country_id", "year" = "year")) %>%
  select(country_id, country_name, wem_regions, iso3c, year, SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, energy_service) %>%
  mutate(energy_service = as.numeric(energy_service * 1e06)) %>% # Original units
  arrange(country_name, year) %>%
  as_tibble()

### Transform gdp from long to wide format
gdp.pop.data.wider <- gdp.pop.data %>%
  #filter(data_type == "GDP_PPP") %>%
  select(country_id, year, value, data_type) %>%
  mutate(value = as.numeric(value)) %>%

  #arrange(country_id, year) %>%
  pivot_wider(names_from = data_type, values_from = value) %>%
  mutate(GDP_PPP = GDP_PPP * 1e09,
         Population = Population * 1e03) %>%
  mutate(GDP_PPP_pcap = (GDP_PPP) / (Population)) %>%
  as_tibble()

###################################################################################
# Define a helper function to get the mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

### Merge with gdp.pop.data
all.data1 <- gdp.pop.data.wider %>%
  #left_join(all.data, by = c("country_id" = "country_id", "year" = "year")) %>%#all.data %>%
  full_join(all.data, by = c("country_id" = "country_id", "year" = "year")) %>%
  mutate(ES_pcap = energy_service / (Population)) %>%
  mutate(Population = Population * 1e03) %>%
  select(country_id, country_name, wem_regions, Population, iso3c, year, SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, ES_pcap, energy_service, GDP_PPP_pcap, GDP_PPP) %>%
  #mutate(value = as.numeric(value)) %>%
  group_by(country_id) %>%
  mutate(
    country_name = ifelse(is.na(country_name), get_mode(country_name), country_name),
    iso3c = ifelse(is.na(iso3c), get_mode(iso3c), iso3c),
    wem_regions = ifelse(is.na(wem_regions), get_mode(wem_regions), wem_regions),
    #
  ) %>%
  ungroup() %>%
  arrange(country_name, year) %>%
  left_join(tfc.enerdem.data1, by = c("country_id" = "country_id", "year" = "year")) %>%
  # Drop all rows where country_name is missing
  #filter(!is.na(country_name)) %>%
  # Dummies for missing values
  mutate(gini_missing = ifelse(is.na(SI.POV.GINI), 1, 0),
         density_missing = ifelse(is.na(EN.POP.DNST), 1, 0),
         urbanization_missing = ifelse(is.na(SP.URB.TOTL.IN.ZS), 1, 0)) %>%
  as_tibble()


#############################################################################################
## Data wrangling for right format for Gompertz curve function

# Use WEM land area and population data to calculate population density (future too)
all.data1 <- all.data1 %>%
  # Merge with land_mappings on country_id
  left_join(land_mappings, by = c("country_id" = "country_id")) %>%
  # group by country_id and check if EN.POP.DNST is missing, if so calculate by taking A_Land_Area divided by Population
  group_by(country_id) %>%
  mutate(EN.POP.DNST = (Population / A_Land_Area ) * 1e-03) # Rather be consistent with WEM


# Linearly interpolate GINI and urbanization percentage using CAGR
vars_to_fill <- c(#"GDP_PPP_pcap", "ES_pcap", "energy_service", "GDP_PPP", 
                  "SI.POV.GINI", "SP.URB.TOTL.IN.ZS")

all.data.gompertz1 <- reduce(
  vars_to_fill,
  function(df, var) {
    df %>%
      group_by(country_id) %>%
      group_modify(~ gapfill(.x, var)) %>%
      ungroup()
  },
  .init = all.data1
)

all.data.gompertz1 <- all.data.gompertz1 %>% ungroup()

# Plot GINI and urbanization percentage using CAGR
ggplot(all.data.gompertz1, aes(x=year, y = SI.POV.GINI, color = country_name, group = country_name)) +
  geom_line() +
  labs(title = "Gini Coefficient Over Time", x = "Year", y = "Gini Coefficient") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(all.data.gompertz1, aes(x=year, y = SP.URB.TOTL.IN.ZS, color = country_name, group = country_name)) +
  geom_line() +
  labs(title = "Urbanization Percentage Over Time", x = "Year", y = "Urbanization Percentage") +
  theme_bw() +
  theme(legend.position = "none")

   
# Log-quadratic Kuznets form - economic theory: gini = b0 + b1 * log(GDP_PPP_pcap) + b2 * (log(GDP_PPP_pcap))^2
p_order <- 3
n_points <- p_order + 1

# Custom function to compute adaptive moving average - ideal max_k
adaptive_rollmean <- function(x, max_k = 3) {
  result <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    for (k in max_k:1) {
      if (i >= k && sum(!is.na(x[(i - k + 1):i])) == k) {
        result[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
        break
      }
    }
  }
  return(result)
}

# Apply it to your data
all.data.gompertz_ma <- all.data.gompertz1 %>%
  ungroup() %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(Gini_growth = (SI.POV.GINI - lag(SI.POV.GINI)) / lag(SI.POV.GINI) * 100) %>%
  mutate(Gini_growth_MA = adaptive_rollmean(Gini_growth, max_k = 10)) %>%
  ungroup()


# # Step 1: Compute growth rates and moving averages
# all.data.gompertz <- all.data.gompertz1 %>%
#   group_by(country_id) %>%
#   arrange(country_id, year) %>%
#   mutate(
#     #GDP_growth = (GDP_PPP_pcap - lag(GDP_PPP_pcap)) / lag(GDP_PPP_pcap) * 100,
#     Gini_growth = (SI.POV.GINI - lag(SI.POV.GINI)) / lag(SI.POV.GINI) * 100,
#     #GDP_growth_MA = rollmean(GDP_growth, k = 3, fill = NA, align = "right"),
#     Gini_growth_MA = rollmean(Gini_growth, k = 10, fill = NA, align = "right")
#   )   %>%
#   ungroup()


projected_gini <- all.data.gompertz_ma %>%
  group_by(country_id) %>%
  arrange(year) %>%
  group_split() %>%
  purrr::map_dfr(function(df) {
    # Check if there are any non-missing Gini values
    non_missing_indices <- which(!is.na(df$SI.POV.GINI))
    if (length(non_missing_indices) == 0) return(df)
    
    last_known_index <- max(non_missing_indices)
    if (last_known_index == nrow(df)) return(df)
    
    # Get the last known Gini value and the last available MA growth rate
    last_gini <- df$SI.POV.GINI[last_known_index]
    valid_growths <- df$Gini_growth_MA[1:last_known_index]
    last_growth <- tail(na.omit(valid_growths), 1)
    
    if (length(last_growth) == 0 || is.na(last_gini)) return(df)
    
    # Forward project Gini using the last known MA growth rate
    for (i in (last_known_index + 1):nrow(df)) {
      last_gini <- last_gini * (1 + last_growth / 100)
      df$SI.POV.GINI[i] <- last_gini
    }
    
    df
  })

# Plot Gini 
ggplot(projected_gini, aes(x=year, y = SI.POV.GINI, color = country_name, group = country_name)) +
  geom_line() +
  labs(title = "Projected Gini Coefficient Over Time", x = "Year", y = "Gini Coefficient") +
  theme_bw() +
  theme(legend.position = "none")


########################################
# Define the nonlinear model function
# logistic_model <- deriv(
#   ~ Asym / (1 + exp((xmid - GDP_PPP_pcap) / scal)),
#   namevec = c("Asym", "xmid", "scal"),
#   function.arg = c("GDP_PPP_pcap", "Asym", "xmid", "scal")
# )
# 
# richards_curve <- deriv(
#   ~ Asym / (1 + exp(-k * (log(GDP_PPP_pcap) - x0)))^v,
#   namevec = c("Asym", "k", "x0", "v"),
#   function.arg = c("GDP_PPP_pcap", "Asym","k", "x0", "v")
# )
# 
# ggplot(all.data.gompertz1, aes(x= GDP_PPP_pcap, y = SI.POV.GINI, color = country_name, group = country_name)) +
#   geom_line() +
#   labs(title = "Projected Gini Coefficient Over Time", x = "Year", y = "Gini Coefficient") +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# richards_curve <- deriv(
#   ~ Asym / (1 + exp(-k * (log(GDP_PPP_pcap) - x0)))^v,
#   namevec = c("Asym", "k", "x0", "v"),
#   function.arg = c("GDP_PPP_pcap", "Asym","k", "x0", "v")
# )
# 
# logistic_model <- deriv(
#   ~ Asym / (1 + exp((xmid - GDP_PPP_pcap) / scal)),
#   namevec = c("Asym", "xmid", "scal"),
#   function.arg = c("GDP_PPP_pcap", "Asym", "xmid", "scal")
# )
# 
# 
# 
# fit <- nlme(
#   SI.POV.GINI ~ logistic_model(GDP_PPP_pcap, Asym, xmid, scal),
#   data = all.data.gompertz1,
#   fixed = Asym + xmid + scal ~ 1,
#   #random = Asym + xmid + scal ~ 1 | country_name, # Allows for Asym, xmid and scal to vary by country
#   random = Asym ~ 1 | country_name,
#   start = c(Asym = 5000, xmid = 10000, scal = 1000),
#   na.action = na.omit,
#   control = nlmeControl(pnlsTol = 0.1, maxIter = 100)
# )
# 
# fit <- nlme(
#   SI.POV.GINI ~ richards_curve(GDP_PPP_pcap, Asym, k, x0, v),
#   data = all.data.gompertz1,
#   fixed = Asym + k + x0 + v ~ 1,
#   #random = Asym + xmid + scal ~ 1 | country_name, # Allows for Asym, xmid and scal to vary by country
#   random = Asym ~ 1 | country_name,
#   start = c(Asym = 5000, k = 0.01, x0 = 10000, v = 1),
#   na.action = na.omit,
#   control = nlmeControl(pnlsTol = 0.1, maxIter = 100)
# )


all.data.gompertz.reg <- all.data.gompertz1 %>%
  # Scale Gini to between 0 and 1 to satisfy logit transformation
  mutate(
    gini_scaled = SI.POV.GINI / 100,
    logit_Gini = log(gini_scaled / (1 - gini_scaled)),
    logit_Gini = ifelse(is.infinite(logit_Gini) | is.nan(logit_Gini), NA, logit_Gini),
    Sigmoid_GDP = log(GDP_PPP_pcap/ (1 + GDP_PPP_pcap)),
    Sigmoid_GDP = ifelse(is.infinite(Sigmoid_GDP) | is.nan(Sigmoid_GDP), NA, Sigmoid_GDP),
    urban_scaled = SP.URB.TOTL.IN.ZS / 100,
    logit_Urban = log(urban_scaled / (1 - urban_scaled)),
    logit_Urban = ifelse(is.infinite(logit_Urban) | is.nan(logit_Urban), NA, logit_Urban)
  ) %>%
  
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(lag_logit_Gini = lag(logit_Gini)) %>%
  ungroup() %>%
  mutate(Year0 = year - 1960)

ggplot(all.data.gompertz.reg, aes(x=Sigmoid_GDP, y = logit_Gini, color = country_name, group = country_name)) +
  geom_line() +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  #labs(title = "Gini Coefficient Over Time", x = "GDP PPP per Capita", y = "Logit Gini Coefficient") +
  theme_bw() +
  theme(legend.position = "none")

# Log-quadratic Kuznets form - economic theory: gini = b0 + b1 * log(GDP_PPP_pcap) + b2 * (log(GDP_PPP_pcap))^2
# 
# 
# all.data.gompertz.reg1 <- all.data.gompertz.reg %>%
#  nest(data = -country_id) %>%
#   mutate(
#     has_enough_data = map_lgl(data, ~ sum(!is.na(.x$logit_Gini) & !is.na(.x$Sigmoid_GDP)) >= n_points),
#     model = map2(data, has_enough_data, ~ if (.y) {
#       #gam(logit_Gini ~ s(GDP_PPP_pcap, k = p_order) , data = .x, na.action = na.exclude) #+  s(year, k = p_order), data = .x, na.action = na.exclude)
#       # This GAM
#       #gam(logit_Gini ~ s(Sigmoid_GDP, k = p_order) , data = .x, na.action = na.exclude)
#       
#       #lm(logit_Gini ~ poly(GDP_PPP_pcap, 3, raw = TRUE), data = .x, na.action = na.exclude)
#       lm(logit_Gini ~ Sigmoid_GDP + yeart, data = .x, na.action = na.exclude)
#       #lm(logit_Gini ~ poly(log(GDP_PPP_pcap), 2) , data = .x, na.action = na.exclude)
#     } else {
#       NULL
#     }),
# 
#     data = map2(data, model, ~ {
#       if (!is.null(.y)) {
#         pred_logit <- predict(.y, newdata = .x)
#         pred_gini <- exp(pred_logit) / (1 + exp(pred_logit))
#         pred_gini_clamped <- pmin(pmax(pred_gini, 0), 1) * 100
#         
#         .x$gini_filled <- dplyr::if_else(
#           is.na(.x$SI.POV.GINI) & .x$year > 2010,
#           pred_gini_clamped,
#           .x$SI.POV.GINI
#         )
#       } else {
#         .x$gini_filled <- .x$SI.POV.GINI
#       }
#       .x
#     })
#     
#   ) %>%
#   select(country_id, data) %>%
#   unnest(data)

# View(all.data.gompertz.reg1 %>% filter(country_name == "Russia"))

# This is the pipeline I have focused on
p_order <- 10
n_points <- 3 #p_order + 1

# Create dummy column for when year = 2023
all.data.gompertz.reg <- all.data.gompertz.reg %>%
  #filter(country_name != "United Arab Emirates") %>%
  mutate(year_2023 = ifelse(year == 2023, 1, 0)) %>%
  mutate(
    GDP_low = Sigmoid_GDP * (GDP_PPP_pcap < 20000),
    GDP_high = Sigmoid_GDP * (GDP_PPP_pcap >= 20000)
  )

# # Nesting per country
all.data.gompertz.reg1 <- all.data.gompertz.reg %>%
  nest(data = -country_id) %>%
  mutate(
    has_gini_data = map_lgl(data, ~ sum(!is.na(.x$logit_Gini) & !is.na(.x$Sigmoid_GDP) ) >= n_points),
    has_urb_data = map_lgl(data, ~ sum(!is.na(.x$logit_Urban) & !is.na(.x$Sigmoid_GDP)) >= n_points),

    gini_model = map2(data, has_gini_data, ~ if (.y) {
      #gam(logit_Gini ~ s(Sigmoid_GDP, k = p_order), data = .x, na.action = na.exclude)
      lm(logit_Gini ~ Sigmoid_GDP + year, data = .x, na.action = na.exclude)
      #lm(logit_Gini ~ GDP_low + GDP_high + year, data = .x, na.action = na.exclude) # To control for different regimes of effect of inequality
    } else {
      NULL
    }),

    urb_model = map2(data, has_urb_data, ~ if (.y) {
      #gam(logit_Urban ~ s(year, k = p_order), data = .x, na.action = na.exclude)
      lm(logit_Urban ~ year, data = .x, na.action = na.exclude) #Sigmoid_GDP +
    } else {
      NULL
    }),

    data = pmap(list(data, gini_model, urb_model), function(df, g_model, u_model) {
      if (!is.null(g_model)) {
        pred_logit <- predict(g_model, newdata = df)
        pred_gini <- exp(pred_logit) / (1 + exp(pred_logit))
        pred_gini_clamped <- pmin(pmax(pred_gini, 0), 1) * 100
        df$gini_filled <- dplyr::if_else(
          is.na(df$SI.POV.GINI) & df$year > 2000,
          pred_gini_clamped,
          df$SI.POV.GINI
        )
      } else {
        df$gini_filled <- df$SI.POV.GINI
      }

      if (!is.null(u_model)) {
        pred_urb_logit <- predict(u_model, newdata = df)
        pred_urb <- exp(pred_urb_logit) / (1 + exp(pred_urb_logit))
        pred_urb_clamped <- pmin(pmax(pred_urb, 0), 1) * 100
        df$urb_filled <- dplyr::if_else(
          is.na(df$SP.URB.TOTL.IN.ZS) & df$year > 2000,
          pred_urb_clamped,
          df$SP.URB.TOTL.IN.ZS
        )
      } else {
        df$urb_filled <- df$SP.URB.TOTL.IN.ZS
      }

      df
    })
  ) %>%
  select(country_id, data) %>%
  unnest(data)

# ###############################################################################
# library(dplyr)
# library(nlme)
# 
# # Prepare the data
# all.data.gompertz.reg <- all.data.gompertz.reg %>%
#   filter(country_name != "United Arab Emirates") %>%
#   mutate(
#     year_2023 = ifelse(year == 2023, 1, 0),
#     GDP_low = log(GDP_PPP_pcap) * (GDP_PPP_pcap < 5000), #Sigmoid_GDP * (GDP_PPP_pcap < 2000),
#     GDP_high = log(GDP_PPP_pcap) * (GDP_PPP_pcap >=  5000)#Sigmoid_GDP * (GDP_PPP_pcap >= 20000)
#   )
# 
# # Filter for valid observations
# gini_data <- all.data.gompertz.reg %>%
#   filter(!is.na(logit_Gini), !is.na(Sigmoid_GDP))
# 
# urb_data <- all.data.gompertz.reg %>%
#   filter(!is.na(logit_Urban), !is.na(Sigmoid_GDP))
# 
# # Fit mixed-effects models
# library(splines)
# gini_model <- tryCatch({
#   if (nrow(gini_data) >= n_points) {
#     nlme::lme(
#       logit_Gini ~ GDP_low + GDP_high,
#       #logit_Gini ~ ns(log(GDP_PPP_pcap), df = 3),
#       #logit_Gini ~ ns(log(GDP_PPP_pcap), df = 3) + year,
#       #logit_Gini ~ log(GDP_PPP_pcap) + I(log(GDP_PPP_pcap)^2) + year,
#       #random =  ~ 1 | country_id,
#       random =  ~ year | country_id,
#       data = gini_data,
#       na.action = na.exclude,
#       control = lmeControl(opt = "optim")
#     )
#   } else {
#     NULL
#   }
# }, error = function(e) NULL)
# 
# # Kuznets Hypothesis
# # gini_model <- tryCatch({
# #   if (nrow(gini_data) >= n_points) {
# #     gini_data <- gini_data %>%
# #       mutate(
# #         GDP = log(GDP_PPP_pcap),
# #         GDP_sq = GDP^2
# #       )
# # 
# #     nlme::lme(
# #       logit_Gini ~ GDP + GDP_sq,
# #       random =  ~1 | country_id,
# #       data = gini_data,
# #       na.action = na.exclude,
# #       control = lmeControl(opt = "optim")
# #     )
# #   } else {
# #     NULL
# #   }
# # }, error = function(e) NULL)
# 
# 
# summary(gini_model)
# # 
# urb_model <- tryCatch({
#   if (nrow(urb_data) >= n_points) {
#     nlme::lme(
#       logit_Urban ~ year,
#       random = ~1 | country_id,
#       data = urb_data,
#       na.action = na.exclude,
#       control = lmeControl(opt = "optim")
#     )
#   } else {
#     NULL
#   }
# }, error = function(e) NULL)
# 
# # Prepare prediction data
# pred_data_gini <- all.data.gompertz.reg %>%
#   filter(!is.na(GDP_low), !is.na(GDP_high), !is.na(year), !is.na(country_id))
# 
# pred_data_urb <- all.data.gompertz.reg %>%
#   filter(!is.na(year), !is.na(country_id))
# 
# # Predict Gini
# if (!is.null(gini_model)) {
#   pred_logit <- predict(gini_model, newdata = pred_data_gini, level = 1)
#   pred_gini <- exp(pred_logit) / (1 + exp(pred_logit))
#   pred_gini_clamped <- pmin(pmax(pred_gini, 0), 1) * 100
# 
#   pred_df_gini <- pred_data_gini %>%
#     mutate(pred_gini_clamped = pred_gini_clamped)
# } else {
#   pred_df_gini <- NULL
# }
# 
# # Predict Urban
# if (!is.null(urb_model)) {
#   pred_urb_logit <- predict(urb_model, newdata = pred_data_urb, level = 1)
#   pred_urb <- exp(pred_urb_logit) / (1 + exp(pred_urb_logit))
#   pred_urb_clamped <- pmin(pmax(pred_urb, 0), 1) * 100
# 
#   pred_df_urb <- pred_data_urb %>%
#     mutate(pred_urb_clamped = pred_urb_clamped)
# } else {
#   pred_df_urb <- NULL
# }
# # 
# # # Join predictions back to full dataset and fill missing values
# all.data.gompertz.reg1 <- all.data.gompertz.reg %>%
#   left_join(pred_df_gini %>% select(country_id, year, pred_gini_clamped), by = c("country_id", "year")) %>%
#   left_join(pred_df_urb %>% select(country_id, year, pred_urb_clamped), by = c("country_id", "year")) %>%
#   mutate(
#     gini_filled = if_else(is.na(SI.POV.GINI) & year > 2000, pred_gini_clamped, SI.POV.GINI),
#     urb_filled = if_else(is.na(SP.URB.TOTL.IN.ZS) & year > 2000, pred_urb_clamped, SP.URB.TOTL.IN.ZS)
#   ) %>%
#   select(-pred_gini_clamped, -pred_urb_clamped)

# #########################################
# NLME


# Treat as Pooled series: No individual variation and looks flat
# # Check if there is enough data for modeling
# has_gini_data <- sum(!is.na(all.data.gompertz.reg$logit_Gini) & !is.na(all.data.gompertz.reg$Sigmoid_GDP)) >= n_points
# has_urb_data <- sum(!is.na(all.data.gompertz.reg$logit_Urban) & !is.na(all.data.gompertz.reg$Sigmoid_GDP)) >= n_points
# 
# # Fit models if applicable
# gini_model <- if (has_gini_data) {
#   lm(logit_Gini ~ GDP_low + GDP_high + year, data = all.data.gompertz.reg, na.action = na.exclude)
# } else {
#   NULL
# }
# 
# urb_model <- if (has_urb_data) {
#   lm(logit_Urban ~ year, data = all.data.gompertz.reg, na.action = na.exclude)
# } else {
#   NULL
# }
# 
# # Predict and fill missing values
# all.data.gompertz.reg1 <- all.data.gompertz.reg %>%
#   mutate(
#     gini_filled = if (!is.null(gini_model)) {
#       pred_logit = predict(gini_model, newdata = .)
#       pred_gini = exp(pred_logit) / (1 + exp(pred_logit))
#       pred_gini_clamped = pmin(pmax(pred_gini, 0), 1) * 100
#       dplyr::if_else(is.na(SI.POV.GINI) & year > 2000, pred_gini_clamped, SI.POV.GINI)
#     } else {
#       SI.POV.GINI
#     },
#     urb_filled = if (!is.null(urb_model)) {
#       pred_urb_logit = predict(urb_model, newdata = .)
#       pred_urb = exp(pred_urb_logit) / (1 + exp(pred_urb_logit))
#       pred_urb_clamped = pmin(pmax(pred_urb, 0), 1) * 100
#       dplyr::if_else(is.na(SP.URB.TOTL.IN.ZS) & year > 2000, pred_urb_clamped, SP.URB.TOTL.IN.ZS)
#     } else {
#       SP.URB.TOTL.IN.ZS
#     }
#   )
# 
### Plots

create_theme1 <- function(rel.size = 1) {
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

#View(all.data.gompertz.reg1 %>% filter(country_name == "Singapore"))  

shell.brand.palette <- readRDS(here::here("data", "shell_brand_palette.rds"))

plot_data <- shell.brand.palette %>%
  mutate(Hex = paste0("#", Hex)) %>%
  mutate(country_id = row_number()) %>% 
  left_join(all.data.gompertz.reg1 , by = "country_id") #%>%
  #distinct()

p1 <- ggplot(plot_data, aes(x = year, y = gini_filled, colour = Hex)) +
  geom_line(aes(group = country_name), linetype = "dashed") +
  geom_line(aes(y = SI.POV.GINI, group = country_name), linetype = "solid")  +
  theme_bw() + create_theme1(2) +
  # geom_text(data = subset(plot_data, !duplicated(country_name, fromLast = TRUE)),
  #           aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
  #           size = rel(8)) +
  theme(legend.position = "none") +
  labs(title = "Income Inequality Over Time", 
       x = "Year", y = "Income inequality (Gini)") +
  scale_color_identity() #+
# Add Gompertz line and add label saying Gompertz Model fit
#geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
#annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
#         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 
p1
# Save p1 to png
ggplotly(p1 + theme(legend.position = "none")) %>%
  #layout(title = "Income Inequality Over Time",
  #       xaxis = list(title = "Year"),
   #      yaxis = list(title = "Income inequality (Gini)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_gini.html"), selfcontained = TRUE)

ggsave(filename = here::here("plots", "forecast_gini.png"), plot = p1, width = 10, height = 6.3, dpi = 250)

p1 <- ggplot(plot_data, aes(x = year, y = urb_filled, colour = Hex)) +
  geom_line(aes(group = country_name), linetype = "dashed") +
  geom_line(aes(y = SP.URB.TOTL.IN.ZS, group = country_name), linetype = "solid")  +
  theme_bw() + create_theme1(2) +
  # geom_text(data = subset(plot_data, !duplicated(country_name, fromLast = TRUE)),
  #           aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
  #           size = rel(8)) +
  theme(legend.position = "none") +
  labs(title = "Urbanization Over Time", 
       x = "Year", y = "Urbanization (%)") +
  scale_color_identity() #+
# Add Gompertz line and add label saying Gompertz Model fit
#geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
#annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
#         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 
p1
# Save p1 to png
ggplotly(p1 + theme(legend.position = "none")) %>%
  #layout(title = "Income Inequality Over Time",
  #       xaxis = list(title = "Year"),
  #      yaxis = list(title = "Income inequality (Gini)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_urbani.html"), selfcontained = TRUE)

ggsave(filename = here::here("plots", "forecast_urban.png"), plot = p1, width = 10, height = 6.3, dpi = 250)

p1 <- ggplot(plot_data, aes(x = GDP_PPP_pcap, y = gini_filled, colour = Hex)) +
  geom_line(aes(group = country_name), linetype = "dashed") +
  geom_line(aes(y = SI.POV.GINI, group = country_name), linetype = "solid")  +
  theme_bw() + create_theme1(2) +
  # geom_text(data = subset(plot_data, !duplicated(country_name, fromLast = TRUE)),
  #           aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
  #           size = rel(8)) +
  theme(legend.position = "none") +
  labs(title = "Income Inequality vs GDP", 
       x = "GDP/capita (USD 2018 Real) ", y = "Income inequality (Gini)") +
  scale_color_identity() #+
# Add Gompertz line and add label saying Gompertz Model fit
#geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
#annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
#         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 
p1
# Save p1 to png
ggplotly(p1 + theme(legend.position = "none")) %>%
  #layout(title = "Income Inequality Over Time",
  #       xaxis = list(title = "Year"),
  #      yaxis = list(title = "Income inequality (Gini)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_gini_gdp.html"), selfcontained = TRUE)

ggsave(filename = here::here("plots", "forecast_gini_gdp.png"), plot = p1, width = 10, height = 6.3, dpi = 250)

p1 <- ggplot(plot_data, aes(x = GDP_PPP_pcap, y = gini_filled, colour = Hex)) +
  geom_line(aes(group = country_name), linetype = "dashed") +
  geom_line(aes(y = SI.POV.GINI, group = country_name), linetype = "solid")  +
  theme_bw() + create_theme1(2) +
  # geom_text(data = subset(plot_data, !duplicated(country_name, fromLast = TRUE)),
  #           aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
  #           size = rel(8)) +
  theme(legend.position = "none") +
  labs(title = "Income Inequality vs GDP", 
       x = "GDP/capita (USD 2018 Real) ", y = "Income inequality (Gini)") +
  scale_color_identity() #+
# Add Gompertz line and add label saying Gompertz Model fit
#geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
#annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
#         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 
p1
# Save p1 to png
ggplotly(p1 + theme(legend.position = "none")) %>%
  #layout(title = "Income Inequality Over Time",
  #       xaxis = list(title = "Year"),
  #      yaxis = list(title = "Income inequality (Gini)")) %>%
  htmlwidgets::saveWidget(here::here("plots/predicted_gini_gdp.html"), selfcontained = TRUE)

ggsave(filename = here::here("plots", "forecast_gini_gdp.png"), plot = p1, width = 10, height = 6.3, dpi = 250)
  
p1 <- ggplotly(ggplot(all.data.gompertz.reg1, aes(x=year, y = gini_filled, color = country_name, group = country_name)) +
                 geom_line(linetype = "dashed") +
                 geom_line(aes(y = SI.POV.GINI), linetype = "solid") +
                 labs(title = "Gini Coefficient Over Time", x = "Year", y = "Gini Coefficient") +
                 theme_bw() )# +
                 #theme(legend.position = "none"))
#Save to interactive html
htmlwidgets::saveWidget(p1, here::here("plots", "gini_over_time.html"), selfcontained = TRUE)

p1 <- ggplotly(ggplot(all.data.gompertz.reg1, aes(x=year, y = urb_filled, color = country_name, group = country_name)) +
                 geom_line(linetype = "dashed") +
                 geom_line(aes(y = SP.URB.TOTL.IN.ZS), linetype = "solid") +
                 labs(title = "Urbanization Over Time", x = "Year", y = "Urbanization") +
                 theme_bw() #+
                 #theme(legend.position = "none")
               )
#Save to interactive html
htmlwidgets::saveWidget(p1, here::here("plots", "urb_over_time.html"), selfcontained = TRUE)

p1 <- ggplotly(ggplot(all.data.gompertz.reg1, aes(x=GDP_PPP_pcap, y = gini_filled, color = country_name, group = country_name)) +
                 geom_line(linetype = "dashed") +
                 geom_line(aes(y = SI.POV.GINI), linetype = "solid") +
                 labs(title = "Gini Coefficient vs GDP (Kuznets)", x = "GDP per capita", y = "Gini Coefficient") +
                 theme_bw() )# +
               #theme(legend.position = "none")

#Save to interactive html
htmlwidgets::saveWidget(p1, here::here("plots", "gini_vs_gdp.html"), selfcontained = TRUE)

#### Data to Thomas

# thomas_data <- all.data.gompertz.reg1 %>%
#   mutate(WDI_Gini = case_when(gini_missing == 1 ~ NA_real_,
#                               TRUE ~ SI.POV.GINI),
#          WDI_Urbanization = case_when(urbanization_missing == 1 ~ NA_real_,
#                                       TRUE ~ SP.URB.TOTL.IN.ZS),
#          Impute_Forecast_Gini = gini_filled,
#          Impute_Forecast_Urbanization = urb_filled) %>%
#   select(country_id, country_name, wem_regions, year, Population, WDI_Gini, 
#          Impute_Forecast_Gini, WDI_Urbanization, Impute_Forecast_Urbanization, ES_pcap, GDP_PPP_pcap, Sigmoid_GDP) 
# #save to csv file
# write_csv(thomas_data, here::here("data", "thomas_data.csv"))


all.data.gompertz.reg1 <- all.data.gompertz.reg1 %>%
  mutate(SI.POV.GINI = gini_filled,
         SP.URB.TOTL.IN.ZS = urb_filled)

last_historical_year <- 2023
# all.data.gompertz <- all.data.gompertz.reg1 %>%
#   # Make all columns except country_id, country_name, iso3c, year numeric
#   mutate(across(c(SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, energy_service, ES_pcap, GDP_PPP_pcap, prop_TFC_PTR), as.numeric)) %>%
#   # Set values of ES_pcap, energy_service and prop_TFC_PTR to NA for years after 2024
#   mutate(ES_pcap = ifelse(year > last_historical_year, NA, ES_pcap),
#          energy_service = ifelse(year > last_historical_year, NA, energy_service),
#          prop_TFC_PTR = ifelse(year > last_historical_year, NA, prop_TFC_PTR)) %>%
#   mutate(lag_GDP_PPP_pcap = lag(GDP_PPP_pcap),
#          lag_ES_pcap = lag(ES_pcap),
#          lag_energy_service = lag(energy_service),
#          lag_Gini = lag(SI.POV.GINI),
#          lag_density = lag(EN.POP.DNST),
#          lag_urbanization = lag(SP.URB.TOTL.IN.ZS)) %>%
#   rename(
#     #ES = energy_service,
#     #ES_pcap = ES_pcap,
#     #GDP_PPP = GDP_PPP_pcap,
#     Gini = SI.POV.GINI,
#     density_psqkm = EN.POP.DNST,
#     urbanization_perc = SP.URB.TOTL.IN.ZS
#   ) %>%
#   ungroup() %>%
#   filter(GDP_PPP != 0) %>%
#   arrange(country_id, year) %>%
#   mutate(historical_data = ifelse(year <= 2024, 1, 0)) %>%
#   select(country_id, country_name, iso3c, wem_regions, Population, year,
#          Gini, density_psqkm, urbanization_perc, ES_pcap, energy_service, GDP_PPP_pcap,
#          GDP_PPP, prop_TFC_PTR, Land_Area = A_Land_Area, historical_data)
# 
# # Get the density and urbanization of the USA
# all.data.gompertz.usa <- all.data.gompertz %>%
#   ungroup() %>%
#   filter(country_id == 1) %>%
#   select(USA_dens = density_psqkm, USA_urban = urbanization_perc, year)
# 
# # Join with all.data.gompertz on year
# all.data.gompertz.test <- all.data.gompertz %>%
#   left_join(all.data.gompertz.usa, by = "year") %>%
#   mutate(d_bar = case_when(density_psqkm > USA_dens ~ density_psqkm - USA_dens,
#                            TRUE ~ 0),
#          u_bar = case_when(urbanization_perc > USA_urban ~ urbanization_perc - USA_urban,
#                            TRUE ~ 0)) %>%
#   group_by(country_id) %>%
#   arrange(country_id, year) %>%
#   mutate(rising_income = case_when(GDP_PPP_pcap > lag(GDP_PPP_pcap, default = first(GDP_PPP_pcap)) ~ 1,
#                                    TRUE ~ 0),
#          falling_income = case_when(GDP_PPP_pcap < lag(GDP_PPP_pcap, default = first(GDP_PPP_pcap)) ~ 1,
#                                    TRUE ~ 0))        

# Clean and transform the main dataset
all.data.gompertz <- all.data.gompertz.reg1 %>%
  mutate(across(
    c(SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, energy_service, ES_pcap, GDP_PPP_pcap, prop_TFC_PTR),
    as.numeric
  )) %>%
  mutate(ES_pcap_WEM_run = ES_pcap) %>%
  mutate(across(
    c(ES_pcap, energy_service, prop_TFC_PTR),
    ~ if_else(year > last_historical_year, NA_real_, .)
  )) %>%
  group_by(country_id) %>%
  arrange(country_id, year) %>%
  mutate(
    lag_GDP_PPP_pcap = lag(GDP_PPP_pcap),
    lag_ES_pcap = lag(ES_pcap),
    lag_energy_service = lag(energy_service),
    lag_Gini = lag(SI.POV.GINI),
    lag_density = lag(EN.POP.DNST),
    lag_urbanization = lag(SP.URB.TOTL.IN.ZS)
  ) %>%
  rename(
    Gini = SI.POV.GINI,
    density_psqkm = EN.POP.DNST,
    urbanization_perc = SP.URB.TOTL.IN.ZS
  ) %>%
  filter(GDP_PPP != 0) %>%
  ungroup() %>%
  arrange(country_id, year) %>%
  mutate(historical_data = if_else(year <= 2024, 1, 0)) %>%
  select(
    country_id, country_name, iso3c, wem_regions, Population, year,
    Gini, density_psqkm, urbanization_perc, ES_pcap, energy_service,
    GDP_PPP_pcap, GDP_PPP, prop_TFC_PTR, Land_Area = A_Land_Area, historical_data
  ) %>%
  left_join(urban_mappings_list_all, by = c("country_id", "year")) %>%
  # Set urbanization_perc column values to the values of the urbanization_perc column in the urban_mappings_list_all dataset
  mutate(urbanization_perc = wem_urbanization_perc)
  

# Extract USA reference values
all.data.gompertz.usa <- all.data.gompertz %>%
  filter(country_id == 1) %>%
  select(year, USA_dens = density_psqkm, USA_urban = urbanization_perc)

# Join and compute derived variables
all.data.gompertz <- all.data.gompertz %>%
  left_join(all.data.gompertz.usa, by = "year") %>%
  mutate(
    d_bar = pmax(density_psqkm - USA_dens, 0),
    u_bar = pmax(urbanization_perc - USA_urban, 0)
  ) %>%
  group_by(country_id) %>%
  arrange(country_id, year) %>%
  mutate(
    rising_income = if_else(GDP_PPP_pcap > lag(GDP_PPP_pcap, default = first(GDP_PPP_pcap)), 1, 0),
    falling_income = if_else(GDP_PPP_pcap < lag(GDP_PPP_pcap, default = first(GDP_PPP_pcap)), 1, 0)
  ) %>%
  select(-USA_dens, -USA_urban) %>%
  ungroup() %>%
  #Join with wem_urbanization data
  #left_join(urban_mappings_list_all, by = c("country_id", "year")) %>%
  left_join(gini_combined, by = c("country_id", "year")) 

# Merge with countries.data and if country_name is missing, set it to value of country_name in countries.data
all.data.gompertz <- all.data.gompertz %>%
  left_join(countries.data, by = "country_id") %>%
  mutate(
    country_name = ifelse(is.na(country_name.x), country_name.y, country_name.x)
  ) %>%
  select(-country_name.x, -country_name.y) %>%
  # Move country_name to second column and drop wem_regions column
  select(country_id, country_name, everything(), -wem_regions, -iso3c)

# additional_imputation <- all.data.gompertz %>%
#   ungroup() %>%
#   group_by(wem_regions, year) #%>%
#   # If Gini or urbanization_perc is missing, fill with mean of the region for that year
#   #mutate(
#   #  Gini = ifelse(is.na(Gini), mean(Gini, na.rm = TRUE), Gini),
#   #  urbanization_perc = ifelse(is.na(urbanization_perc), mean(urbanization_perc, na.rm = TRUE), urbanization_perc)
#   #) 

# View(additional_imputation %>% filter(wem_regions == "Arabian Peninsula"))

# p1 <- ggplotly(ggplot(all.data.gompertz, aes(x=year, y = urbanization_perc, color = country_name, group = country_name)) +
#                  geom_line() +
#                  labs(title = "Urbanization Over Time", x = "Year", y = "Urbanization") +
#                  theme_bw() #+
#                #theme(legend.position = "none")
# )
# #Save to interactive html
# htmlwidgets::saveWidget(p1, here::here("plots", "urb_over_time.html"), selfcontained = TRUE)


########## Nice extrapolation: https://stackoverflow.com/questions/74858960/how-to-extrapolate-values-over-dates-using-r

### Save to rds in data folder
saveRDS(all.data.gompertz, here::here("data", "all_data_wem_espcap_imputation_wem_urban.rds"))
write_csv(all.data.gompertz, here::here("data", "all_data_wem_espcap_imputation_wem_urban.csv"))
#########################################################################################################################################
# 
# p1 <- ggplot(all.data.gompertz, aes(x = year, y = density_psqkm, colour = country_id)) +
#   geom_line(aes(group = country_name), linetype = "dashed") +
#   #geom_line(aes(y = SI.POV.GINI, group = country_name), linetype = "solid")  +
#   theme_bw() + create_theme1(2) +
#   # geom_text(data = subset(plot_data, !duplicated(country_name, fromLast = TRUE)),
#   #           aes(label = country_name), hjust = 0.5, vjust = 1, position = position_jitter(width = 0.02, height = 0.02),
#   #           size = rel(8)) +
#   theme(legend.position = "none") +
#   labs(title = "Income Inequality Over Time", 
#        x = "Year", y = "Income inequality (Gini)") +
#   scale_x_continuous(breaks = seq(1960, 2024, by = 4), trans = "log2") +
#   scale_color_identity() #+
# # Add Gompertz line and add label saying Gompertz Model fit
# #geom_line(data = fitted_values, aes(x = log_GDP_pcap, y = ES_pcap), color = "black", linetype = "solid", linewidth = 2) #+
# #annotate("text", x = max(fitted_values$log_GDP_pcap) - 0.5, y = max(fitted_values$ES_pcap) - 0.5, 
# #         label = "Gompertz Model Fit", color = "black", size = 10, hjust = 1, vjust = 1) 
# ggplotly(p1)
# 
# ggplot(all.data.gompertz, aes(x = year)) + 
#   geom_line(aes(y = gini_case1, color =
# as.factor(country_id)), size = 0.5) + geom_line(aes(y = gini_case2, color =
# as.factor(country_id)), size = 0.5, linetype = "dashed") + geom_line(aes(y =
# gini_case3, color = as.factor(country_id)), size = 0.5, linetype = "dotted") + 
#   geom_line(aes(y = Gini, color = as.factor(country_id)), size = 0.5, linetype = "longdash") +
# labs(title = "Gini Coefficient Over Time by Case", x = "Year", y = "Gini
# Coefficient", color = "Country ID") + theme_minimal() + facet_wrap(~
# country_id, scales = "free_y") + theme(legend.position = "none")
# 
