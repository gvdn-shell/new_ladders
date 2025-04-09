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
              "plotly") #
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

#####################################################################################################################
####################################################################################################

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
run_id <- 1         # Isolate run interested in
max_year <- 2024  # Get historic data only
max_sector_id <- toString(c(14))  # Focus on PT - Road
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
  " AND [run_id] = (SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])",
  " AND [sector_id] IN (", max_sector_id, ")"
)
enerserv.data <- queryDB(enerserv.query) 

# Sum over all carriers
enerserv.data <- enerserv.data %>%
  group_by(country_id, year, sector_id, run_id) %>%
  summarise(energy_service = sum(energy_service)) %>%
  ungroup() %>%
  as_tibble()

# # # Energy demand data (TFC1): Units as per WEM (Sector by carrier)/ TFC2 Carrier by source
# tfc.enerdem.query <- paste0(
#   "SELECT [run_id], [data_type], [sector_id], [carrier_id], [country_id], [year], [demand]",
#   " FROM [WEMv3_1].[energy_demand]",
#   " WHERE [year] <= ", max_year,
#   " AND [run_id] = (SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])", # Ensure consistency of run_id selected
#   " AND [data_type] = 'TFC1'",
#   " AND [sector_id] IN (", max_sector_id, ")"
# )
# tfc.enerdem.data <- queryDB(tfc.enerdem.query)
# 
# # Population and GDP data
gdp.pop.query <- paste0(
  "SELECT [run_id], [category], [data_type], [country_id], [year], [value]",
  " FROM [WEMv3_1].[other_data]",
  " WHERE [year] <= ", max_year,
  " AND [run_id] = (SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])",
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
#### Import country names and mappings
# Specify the path to your Excel file
excel_file <- here::here("data", "Shell WEM - A Settings v3.4.1.xlsx")

# Create a vector of named regions
named_regions <- c("A_WEM_Countries_Mapped", 
                   "A_IEA_Countries_Mapped", 
                   "A_WEM_Top_100_Classification_Mapped", 
                   "A_ISO_3_letter_code_Mapped")

# Read each named region into a list of data frames
country_mappings_list <- lapply(named_regions, function(region) {
  # Read data without column names
  df <- read.xlsx(excel_file, namedRegion = region, skipEmptyRows = FALSE, colNames = FALSE)
  
  # Assuming each region results in one main data column
  # Set the name of the column to the region name
  # If there are multiple columns, consider appending index or specific identifiers
  colnames(df) <- region
  
  return(df)
})

# Combine all data frames into one by columns
all.country.mappings <- as_tibble(bind_cols(country_mappings_list) %>%
  mutate(iso3c = toupper(A_ISO_3_letter_code_Mapped)) %>%
  select(-A_ISO_3_letter_code_Mapped))

#####################################################################################################################
### Focus on actual countries of WEM 100
# Merge all.country.mappings with countries.data
countries.data.iso <- countries.data %>%
  left_join(all.country.mappings, by = c("country_name" = "A_WEM_Countries_Mapped")) %>%
  select(country_id, country_name, iso3c) %>%
  mutate(country_name = ifelse(is.na(country_name), country_id, country_name)) %>%
  filter(complete.cases(.)) %>%
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
  select(country_id, country_name, iso3c, year, SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, income) %>%
  mutate(SI.POV.GINI = as.numeric(SI.POV.GINI),
         EN.POP.DNST = as.numeric(EN.POP.DNST),
         SP.URB.TOTL.IN.ZS = as.numeric(SP.URB.TOTL.IN.ZS)) %>%
  #filter(complete.cases(.)) %>%
  arrange(country_name, year) %>%
  as_tibble()

#### Merge with energy service data
all.data <- wdi.data %>%
  left_join(enerserv.data, by = c("country_id" = "country_id", "year" = "year")) %>%
  select(country_id, country_name, iso3c, year, SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, energy_service) %>%
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

### Merge with gdp.pop.data
all.data1 <- all.data %>%
  left_join(gdp.pop.data.wider, by = c("country_id" = "country_id", "year" = "year")) %>%
  mutate(ES_pcap = energy_service / (Population)) %>%
  select(country_id, country_name, iso3c, year, SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, ES_pcap, energy_service, GDP_PPP_pcap, GDP_PPP) %>%
  #mutate(value = as.numeric(value)) %>%
  arrange(country_name, year) %>%
  as_tibble()

all.data1 %>% filter(country_id == 1 & year == 2023) %>%
  select(ES_pcap)

ggplotly(ggplot(all.data1, aes(y = ES_pcap, x = year, color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           #labs(title = "Gini Coefficient vs Energy Service", y = "Energy Service", x = "Gini Coefficient") +
           theme_bw() +
           theme(legend.position = "none"))

ggplotly(ggplot(all.data1, aes(y = log(ES_pcap), x = log(GDP_PPP_pcap), color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           #labs(title = "Gini Coefficient vs Energy Service", y = "Energy Service", x = "Gini Coefficient") +
           theme_bw() +
           theme(legend.position = "none"))


### Plot scatter plot of Gini coefficient vs energy service
ggplotly(ggplot(all.data1, aes(y = log(energy_service), x = SI.POV.GINI, color = country_name)) +
  geom_line()+
           geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gini Coefficient vs Energy Service", y = "Energy Service", x = "Gini Coefficient") +
  theme_bw() +
  theme(legend.position = "none"))

### Plot scatter plot of Energy Service as a function of GDP_PPP_pcap
ggplotly(ggplot(all.data1 %>% filter(country_id %in% c(1,11:21)), aes(y = (energy_service), x = log(GDP_PPP_pcap), color = country_name)) +
  geom_line() +
           geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per capita vs Energy Service", y = "Energy Service", x = "GDP per capita") +
  theme_bw() +
  theme(legend.position = "none"))

ggplotly(ggplot(all.data1 %>% filter(country_id %in% c(1:5,11:21)), aes(y = (energy_service), x = GDP_PPP_pcap, color = country_name)) +
           geom_line() +
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           labs(title = "GDP per capita vs Energy Service", y = "Energy Service", x = "GDP per capita") +
           theme_bw() +
           theme(legend.position = "none"))

ggplotly(ggplot(all.data1 %>% filter(country_id %in% c(1:10)), aes(y = (energy_service), x = log(GDP_PPP_pcap), color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           labs(title = "GDP_PPP vs Energy Service", y = "Energy Service", x = "GDP_PPP_pcap") +
           theme_bw() +
           theme(legend.position = "none"))

ggplotly(ggplot(all.data1 %>% filter(country_id %in% c(1:10)), aes(y = log(energy_service), x = log(GDP_PPP_pcap), color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           labs(title = "GDP_PPP vs Energy Service", y = "Energy Service", x = "GDP_PPP_pcap") +
           theme_bw() +
           theme(legend.position = "none"))

ggplotly(ggplot(all.data1, aes(y = log(energy_service), x = log(GDP_PPP_pcap), color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           labs(title = "GDP_PPP vs Energy Service", y = "Energy Service", x = "GDP_PPP_pcap") +
           theme_bw() +
           theme(legend.position = "none"))

ggplotly(ggplot(all.data1, aes(y = log(energy_service), x = log(GDP_PPP_pcap), color = country_name)) +
          geom_line()+
          geom_point() +
          #geom_smooth(method = "lm", se = FALSE) +
          labs(title = "GDP_PPP vs Energy Service", y = "Energy Service", x = "GDP_PPP_pcap") +
          theme_bw() +
          theme(legend.position = "none"))

ggplotly(ggplot(all.data1, aes(y = (energy_service), x = (SI.POV.GINI), color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           #labs(title = "GDP_PPP vs Energy Service", y = "Energy Service", x = "GDP_PPP_pcap") +
           theme_bw() +
           theme(legend.position = "none"))

# Gately ES (per 1000 vehicles) versus GDP per capita

ggplotly(ggplot(all.data1, aes(y = (ES_pcap), x = (GDP_PPP_pcap), color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           labs(title = "GDP_PPP vs Energy Service", y = "Energy Service", x = "GDP_PPP_pcap") +
           theme_bw() +
           theme(legend.position = "none"))

ggplotly(ggplot(all.data1, aes(y = (ES_pcap), x = log(GDP_PPP_pcap), color = country_name)) +
           geom_line()+
           geom_point() +
           #geom_smooth(method = "lm", se = FALSE) +
           labs(title = "GDP_PPP vs Energy Service", y = "Energy Service", x = "GDP_PPP_pcap") +
           theme_bw() +
           theme(legend.position = "none"))

#############################################################################################
## Data wrangling for right format for Gompertz curve function

all.data.gompertz <- all.data1 %>%
  ungroup() %>%
  group_by(country_id) %>%
  arrange(year) %>%
  # Make all columns except country_id, country_name, iso3c, year numeric
  mutate(across(c(SI.POV.GINI, EN.POP.DNST, SP.URB.TOTL.IN.ZS, energy_service, ES_pcap, GDP_PPP_pcap), as.numeric)) %>%
  mutate(lag_GDP_PPP_pcap = lag(GDP_PPP_pcap),
         lag_ES_pcap = lag(ES_pcap),
         lag_energy_service = lag(energy_service),
         lag_Gini = lag(SI.POV.GINI),
         lag_density = lag(EN.POP.DNST),
         lag_urbanization = lag(SP.URB.TOTL.IN.ZS)) %>%
  ungroup() %>%
  filter(GDP_PPP != 0) %>%
  arrange(country_id, year)

### Save to rds in data folder
saveRDS(all.data.gompertz, here::here("data", "all_data_wem.rds"))
