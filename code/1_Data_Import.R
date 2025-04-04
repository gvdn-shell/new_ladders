# Original Python code:

# # expand biofuels split
# biomass_traditional_per_sector_carrier = np.full((num_sectors, num_carriers, num_countries), np.nan)
# for carrier in range(num_carriers):
#   for country in range(num_countries):
#   if np.nansum(TFC[carrier, :, country]) != 0.0:
#   TFC_share_trad_biomass[carrier, country] = (nan_to_zero(TFC[carrier, Source.BIOMASS_TRADITIONAL.value, country])
#                                               / np.nansum(TFC[carrier, :, country]))
#   else:
#     TFC_share_trad_biomass[carrier, country] = 0.0
#     
#     if carrier == Carrier.SOLID_HYDROCARBON_FUELS.value:
#       biomass_traditional_per_sector_carrier[:, carrier, country] = (np.nansum(end_use_energy_demand[:, carrier, country])
#                                                                      * TFC_share_trad_biomass[carrier, country]
#                                                                      * traditional_bio_distribution_percentage[:, country]
#                                                                      / end_use_energy_demand[:, carrier, country])
#       else:
#         biomass_traditional_per_sector_carrier[:, carrier, country] = TFC_share_trad_biomass[carrier, country]
#         
#         return biomass_traditional_per_sector_carrier

# Remove all objects in R workspace
rm(list = ls())

ncpus <- 12 # Check according to number of cpus of system
packages <- c("plm","ggplot2","lattice","nlme","DBI","odbc","readxlsb","here","RODBC","plyr","dplyr",
              "tidyr","plotly","here","mice","tidyr","parallel","foreach","doParallel","purrr", "zoo",
              "here", "openxlsx", "WDI", "gapminder", "rworldmap",
              "systemfit", "flexmix", "stringr", "betareg", "extrafont", "sysfonts", "showtext") #
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages],dependencies = TRUE, Ncpus = ncpus)
}
# Load required packages
invisible(lapply(packages, library, character.only = TRUE))

set.seed(1652)

# Access the proxy settings from environment variables
http_proxy <- Sys.getenv("http_proxy")
Sys.setenv(http_proxy = http_proxy)
Sys.setenv(https_proxy = http_proxy)


##########################################################################################################
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

####################################################################################################
# Establish the database connection
# https://solutions.posit.co/connections/db/databases/microsoft-sql-server/
# # FROM HERE
# # Define the database connection parameters
# sort(unique(odbcListDrivers()[[1]])) # Check for SQL drivers and names which are then input below
# driver <- "ODBC Driver 18 for SQL Server"  # Update with the appropriate driver name
# # Store as environmental variables to enhance security, e.g. Sys.setenv(DB_SERVER = "ABCD")
# server <- Sys.getenv("DB_SERVER")  # Update with the server name
# database <- Sys.getenv("DB_DATABASE")  # Update with the database name
# uid <- Sys.getenv("DB_UID")
# 
# conn <- DBI::dbConnect(odbc::odbc(),
#                        Driver = driver,
#                        Server = server,
#                        Database = database,
#                        Authentication = "ActiveDirectoryInteractive",
#                        UID = uid,
#                        autocommit = TRUE) # Azure MFA authentication
# 
# # Define a function to simplify querying the database
# queryDB <- function(query) {
#   dbGetQuery(conn, query)
# }
# 
# #---LOAD DATA---------------------------
# run_id <- 1         # Isolate run interested in
# max_year <- 2023  # Get historic data only
# max_sector_id <- toString(c(11))  # Focus on residential H&C (11) and Lighting and appliances (10)
# # String so that SQL can understand it
# 
# # Country name data
# countries.query <- paste0(
#   "SELECT [country_id], [country_name]",
#   " FROM [WEMv3_1].[countries]"
# )
# countries.data <- queryDB(countries.query)
# 
# # Energy Service data: Units as per WEM
# enerserv.query <- paste0(
#   "SELECT [run_id], [sector_id], [carrier_id], [country_id], [year], [energy_service]",
#   " FROM [WEMv3_1].[energy_service]",
#   " WHERE [year] <= ", max_year,
#   " AND [run_id] = (SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])",
#   " AND [sector_id] IN (", max_sector_id, ")"
# )
# enerserv.data <- queryDB(enerserv.query)
# 
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
# gdp.pop.query <- paste0(
#   "SELECT [run_id], [category], [data_type], [country_id], [year], [value]",
#   " FROM [WEMv3_1].[other_data]",
#   " WHERE [year] <= ", max_year,
#   " AND [run_id] = (SELECT MAX([run_id]) FROM [WEMv3_1].[energy_service])",
#   " AND ([data_type] = 'GDP_PPP' OR [data_type] = 'Population')"#,
#   #" AND [run_id] = ", run_id
# )
# gdp.pop.data <- queryDB(gdp.pop.query)
# 
# table(unique(gdp.pop.data$data_type))
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
# dbDisconnect(conn)
# 
# ### Data wrangling and aggregation  ####################################################
# 
# # Transform GDP and Population data to wide format
# wide.gdp.pop.data <- gdp.pop.data %>%
#   select(-category) %>%
#   pivot_wider(
#     id_cols = c("run_id", "country_id", "year"),
#     names_from = "data_type",
#     values_from = "value"
#   )
# 
# id.cols <- c("run_id", "sector_id", "carrier_id", "country_id", "year")
# 
# # Assuming price.data is your original dataframe
# 
# price.rel <- price.data %>%
#   mutate(year = as.numeric(year)) %>%  # Ensure 'year' is numeric
#   filter(carrier_id %in% c(1, 2, 3, 4, 10),
#          data_type == "Carrier Prices") %>%
#   select(-c(data_type, source_id)) %>%
#   mutate(carrier_id = case_when(
#     carrier_id == 1 ~ "SHCF",
#     carrier_id == 2 ~ "LHCF",
#     carrier_id == 3 ~ "GHCF",
#     carrier_id == 4 ~ "ELEC",
#     carrier_id == 10 ~ "BMTD",
#     TRUE ~ as.character(carrier_id)  # Keep original value if no match
#   )) %>%
#   ungroup() %>%
#   arrange(run_id, country_id, sector_id, carrier_id, year) %>%
#   complete(run_id, country_id, sector_id, carrier_id, year, fill = list(Value = NA)) %>%
#   group_by(run_id, country_id, sector_id, carrier_id) %>%
#   group_modify(~ gapfill(., "price")) %>% # Impute missing prices
#   ungroup() %>%
#   pivot_wider(
#     id_cols = c(run_id, country_id, sector_id, year),
#     names_from = carrier_id,
#     values_from = price,
#     names_prefix = "price_"
#   )
# 
# summary(price.rel)
# 
# # Join data into one data frame
# all.energy.service.data <- full_join(
#   select(tfc.enerdem.data, -data_type),
#   enerserv.data,
#   by = id.cols
# ) %>%
#   ungroup() %>%
#   group_by(run_id, sector_id, country_id, year) %>%
#   summarise(prop_demand_biomass = demand[carrier_id == 10] / sum(demand),
#             prop_es_biomass = energy_service[carrier_id == 10] / sum(energy_service)) %>%
#   ungroup() %>%
#   left_join(price.rel, by =  c("run_id", "country_id", "sector_id", "year")) %>%
#   left_join(wide.gdp.pop.data, by = c("run_id", "country_id", "year"), relationship= "many-to-one" ) %>%
#   left_join(countries.data, by = "country_id")
# 
# saveRDS(all.energy.service.data, file = here::here("Data", "all.energy.service.data.rds"))
#### TO HERE
all.energy.service.data <- readRDS(file = here::here("Data", "all.energy.service.data.rds"))
summary(all.energy.service.data)

# Set 0 Population and GDP_PPP to NA.
all.energy.service.data <- all.energy.service.data %>%
  mutate(across(c(Population, GDP_PPP), ~ ifelse(. == 0, NA, .))) %>%
  mutate(GDP_PPP_pcap = GDP_PPP / Population * 10 ^ 6)

#### Import country names and mappings
# Specify the path to your Excel file
excel_file <- here::here("Data", "Shell WEM - A Settings v3.3.2.xlsx")

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


# Function to capitalize the first letter of each string
# capitalize_first <- function(x) {
#   str_to_title(x)
# }


# Combine all data frames into one by columns
all.country.mappings <- bind_cols(country_mappings_list) %>%
  mutate(iso3c = toupper(A_ISO_3_letter_code_Mapped)) %>%
  select(-A_ISO_3_letter_code_Mapped) #%>%
  #mutate(text_column = sapply(text_column, capitalize_first))


#####################################################################################################################################################
# Import charcoal data: TFC
charcoal.data <- read.csv(file = here::here("Data/residential_charcoal.csv"),
                          sep = ",",
                          encoding = "UTF-8") 

unique(charcoal.data$COUNTRY)

charcoal.data.long <-  charcoal.data %>%
  pivot_longer(cols = -COUNTRY, names_to = "Year", values_to = "Charcoal") %>%
  rename(Country = COUNTRY)

Encoding(charcoal.data.long$Country) <- "UTF-8" # File-encoding for special characters 
  
# Remove the 'X' prefix from column names that start with 'X'
charcoal.data.long$Year <- as.integer(gsub("^X", "", charcoal.data.long$Year))
charcoal.data.long$Charcoal[charcoal.data.long$Charcoal == ".."] <- NA
charcoal.data.long$Charcoal <- as.numeric(charcoal.data.long$Charcoal)
# Set zero as NA
charcoal.data.long$Charcoal[charcoal.data.long$Charcoal == 0] <- NA
summary(charcoal.data.long)

# Import SHCF-Traditional biomass: TFC

trad.biomass.data <- read.csv(file = here::here("Data/residential_primary_solid_biofuels.csv"),
                              sep = ",",
                              encoding = "UTF-8") 

trad.biomass.data.long <-  trad.biomass.data %>%
  pivot_longer(cols = -COUNTRY, names_to = "Year", values_to = "Trad_biomass") %>%
  rename(Country = COUNTRY)

Encoding(trad.biomass.data.long$Country) <- "UTF-8" # File-encoding for special characters 

# Remove the 'X' prefix from column names that start with 'X'
trad.biomass.data.long$Year <- as.integer(gsub("^X", "", trad.biomass.data.long$Year))
trad.biomass.data.long$Trad_biomass[trad.biomass.data.long$Trad_biomass == ".."] <- NA
trad.biomass.data.long$Trad_biomass <- as.numeric(trad.biomass.data.long$Trad_biomass)
trad.biomass.data.long$Trad_biomass[trad.biomass.data.long$Trad_biomass == 0] <- NA


summary(trad.biomass.data.long)

View(trad.biomass.data.long %>% filter(Country == "United States"))

###### Import total TFC for Residential

# Import SHCF-Traditional biomass: TFC

res.ener.data <- read.csv(file = here::here("Data/residential_TFC_all.csv"),
                              sep = ",",
                          encoding = "UTF-8") 

res.ener.data.long <-  res.ener.data %>%
  pivot_longer(cols = -COUNTRY, names_to = "Year", values_to = "Res_TFC") %>%
  rename(Country = COUNTRY)

Encoding(res.ener.data.long$Country) <- "UTF-8" # File-encoding for special characters 

# Remove the 'X' prefix from column names that start with 'X'
res.ener.data.long$Year <- as.integer(gsub("^X", "", res.ener.data.long$Year))
res.ener.data.long$Res_TFC[res.ener.data.long$Res_TFC == ".."] <- NA
res.ener.data.long$Res_TFC <- as.numeric(res.ener.data.long$Res_TFC)
res.ener.data.long$Res_TFC[res.ener.data.long$Res_TFC == 0] <- NA


summary(res.ener.data.long)

#########

# Join data set

all.data <- charcoal.data.long %>%
  merge(trad.biomass.data.long, by = c("Country", "Year")) %>%
  merge(res.ener.data.long, by = c("Country", "Year")) %>%
  pivot_longer(cols = -c(Country, Year), names_to = "Type", values_to = "TJ") %>%
  group_by(Country, Type) %>%
  arrange(Year) 


unique(all.data$Country)

setdiff(unique(all.data$Country), unique(all.country.mappings$A_WEM_Countries_Mapped))

library(dplyr)



all.data <- all.data %>%
  mutate(Country = case_when(Country == "Cura\xe7ao/Netherlands Antilles" ~ "Curaçao",
                             Country == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
                             Country == "Bolivarian Republic of Venezuela" ~ "Venezuela",
                             Country == "Brunei Darussalam" ~ "Brunei",
                             Country == "People's Republic of China" ~ "China",
                             Country == "Democratic People's Republic of Korea" ~ "North Korea",
                             Country == "Democratic Republic of the Congo" ~ "DR Congo",
                             Country == "Hong Kong (China)" ~ "Hong Kong",
                             Country == "Islamic Republic of Iran" ~ "Iran",
                             Country == "Kingdom of Eswatini" ~ "Eswatini",
                             Country == "Korea" ~ "South Korea",
                             Country == "Lao People's Democratic Republic" ~ "Laos",
                             Country == "Plurinational State of Bolivia" ~ "Bolivia",
                             Country == "Republic of Moldova" ~  "Moldova",
                             Country == "Republic of North Macedonia" ~ "Macedonia FYR",
                             Country == "Republic of the Congo" ~ "Congo-Brazzaville",
                             Country == "Republic of Turkiye" ~ "Türkiye",
                             Country == "Russian Federation" ~ "Russia",
                             Country == "Slovak Republic" ~ "Slovakia",
                             Country == "Syrian Arab Republic" ~ "Syria",
                             Country == "United Republic of Tanzania"  ~ "Tanzania",
                             Country == "United States" ~ "USA",
                             Country == "Viet Nam" ~ "Vietnam",
                                                          TRUE ~ Country))

setdiff(unique(all.data$Country), unique(all.country.mappings$A_WEM_Countries_Mapped))
unique(all.country.mappings$A_WEM_Countries_Mapped)
### Combine with country mappings

all.data.mappings <- all.data %>%
  right_join(all.country.mappings, by = c("Country" = "A_WEM_Countries_Mapped")) %>%
  filter(Year >= 1971) 

unique(all.data.mappings$Country)
unique(all.country.mappings$A_IEA_Countries_Mapped)

### WEM 100 countries

all.data.wem <- all.data.mappings %>%
  ungroup() %>%
  #filter(!is.na(A_WEM_Countries_Mapped)) %>%
  group_by(Type, Country) %>%
  arrange(Type, Country, Year) %>%
  filter(complete.cases(TJ)) %>%
  mutate(Growth = (TJ - lag(TJ))/lag(TJ) * 100) 

tolerance <- 0.01
epsilon <- 1e-04

# all.data.wem.wider <- all.data.wem %>%
#   pivot_wider(names_from = Type,
#               values_from = c(TJ, Growth)) %>%
#   mutate(Prop_TFC_Charcoal = ifelse(!is.na(TJ_Charcoal), TJ_Charcoal / TJ_Res_TFC, 0),
#          Prop_TFC_Trad_biomass = ifelse(!is.na(TJ_Trad_biomass), TJ_Trad_biomass / TJ_Res_TFC, 0),
#          Prop_TFC_Other = 1 - Prop_TFC_Charcoal - Prop_TFC_Trad_biomass,
#          #Check1 = Prop_TFC_Charcoal + Prop_TFC_Trad_biomass + Prop_TFC_Other,
#          Check2 = ifelse(abs(Prop_TFC_Charcoal + Prop_TFC_Trad_biomass + Prop_TFC_Other-1) <= tolerance, 1, 0))

all.data.wem.wider <- all.data.wem %>%
  pivot_wider(names_from = Type, values_from = c(TJ, Growth)) %>%
  mutate(
    Prop_TFC_Charcoal = ifelse(!is.na(TJ_Charcoal), TJ_Charcoal / TJ_Res_TFC, NA),
    Prop_TFC_Trad_biomass = ifelse(!is.na(TJ_Trad_biomass), TJ_Trad_biomass / TJ_Res_TFC, NA)
  ) %>%
  mutate(
    Prop_TFC_Trad_biomass = case_when(
      is.na(Prop_TFC_Trad_biomass) ~ NA_real_,
      TRUE ~ pmax(pmin(Prop_TFC_Trad_biomass, 1 - epsilon), epsilon)
    ),
    Prop_TFC_Charcoal = case_when(
      is.na(Prop_TFC_Charcoal) ~ NA_real_,
      TRUE ~ pmax(pmin(Prop_TFC_Charcoal, 1 - epsilon), epsilon)
    ),
    Prop_TFC_Other = ifelse(is.na(Prop_TFC_Charcoal) & is.na(Prop_TFC_Trad_biomass), 
                            NA_real_, 
                            1 - Prop_TFC_Charcoal - Prop_TFC_Trad_biomass),
    Check2 = ifelse(abs(Prop_TFC_Charcoal + Prop_TFC_Trad_biomass + Prop_TFC_Other - 1) <= tolerance, 1, 0)
  ) %>%
  mutate(
    Prop_TFC_Other = case_when(
      is.na(Prop_TFC_Other) ~ NA_real_,
      TRUE ~ pmax(pmin(Prop_TFC_Other, 1 - epsilon), epsilon)
    )
  )

summary(all.data.wem.wider)
# all.data.wem.wider <- all.data.wem %>%
#   pivot_wider(names_from = Type,
#               values_from = c(TJ, Growth)) %>%
#   mutate(Prop_TFC_Charcoal = ifelse(!is.na(TJ_Charcoal), TJ_Charcoal / TJ_Res_TFC, NA),
#          Prop_TFC_Trad_biomass = ifelse(!is.na(TJ_Trad_biomass), TJ_Trad_biomass / TJ_Res_TFC, NA)) %>%
#   mutate(Prop_TFC_Trad_biomass = ifelse(is.na(Prop_TFC_Trad_biomass), NA, pmax(pmin(Prop_TFC_Trad_biomass, 1 - epsilon), epsilon)), # Add small constant so not exactly 0/1
#          Prop_TFC_Charcoal = ifelse(is.na(Prop_TFC_Charcoal), NA, pmax(pmin(Prop_TFC_Charcoal, 1 - epsilon), epsilon)),
#          Prop_TFC_Other = 1 - Prop_TFC_Charcoal - Prop_TFC_Trad_biomass,
#          Check2 = ifelse(abs(Prop_TFC_Charcoal + Prop_TFC_Trad_biomass + Prop_TFC_Other-1) <= tolerance, 1, 0)) %>%
#   mutate(Prop_TFC_Other = pmax(pmin(Prop_TFC_Other, 1 - epsilon), epsilon))


summary(all.data.wem.wider)

# all.data.wem.wider <- all.data.wem %>%
#   pivot_wider(names_from = Type,
#               values_from = c(TJ, Growth)) %>%
#   mutate(Prop_TFC_Charcoal = ifelse(!is.na(TJ_Charcoal), TJ_Charcoal / TJ_Res_TFC, 0),
#          Prop_TFC_Trad_biomass = ifelse(!is.na(TJ_Trad_biomass), TJ_Trad_biomass / TJ_Res_TFC, 0)) %>%
#   mutate(Prop_TFC_Trad_biomass = pmax(pmin(Prop_TFC_Trad_biomass, 1 - epsilon), epsilon), # Add small constant so not exactly 0/1
#          Prop_TFC_Charcoal = pmax(pmin(Prop_TFC_Charcoal, 1 - epsilon), epsilon),
#          Prop_TFC_Other = 1 - Prop_TFC_Charcoal - Prop_TFC_Trad_biomass,
#          Check2 = ifelse(abs(Prop_TFC_Charcoal + Prop_TFC_Trad_biomass + Prop_TFC_Other-1) <= tolerance, 1, 0)) %>%
#   mutate(Prop_TFC_Other = pmax(pmin(Prop_TFC_Other, 1 - epsilon), epsilon))

View(all.data.wem.wider %>% filter(Check2 == 0))
View(all.data.wem.wider %>% 
       filter(Prop_TFC_Trad_biomass > 0.9999))


summary(all.data.wem.wider)

ggplotly(ggplot(data = all.data.wem.wider, aes(x= Year, y = Prop_TFC_Trad_biomass, 
                                               group = factor(Country), colour = factor(Country))) + geom_line())

### To do: 12/4/2024
### Import GDP, urbanization, population data, forest cover, social norms
### Price interaction / price substitution
### Translate into per capita

### Take data in WEM as given.
### Model as proportion of ES for industry - use ln transformation as in LFPR to limit size.

# Search for GDP per capita (constant 2010 US$)
# WDIsearch("GDP per capita")
# 
# # Search for Population, total
# WDIsearch("Population")
# 
# WDIsearch("energy")
# 
# View(WDIsearch("forest"))

WDIsearch("Country")
# Example to download GDP per capita (constant 2010 US$) and Population, total
gdp_indicator <- "NY.GDP.PCAP.KD"  # GDP per capita (constant 2017 US$)
population_indicator <- "SP.POP.TOTL"  # Population, total
tfc_wdi <- "1.1_TOTAL.FINAL.ENERGY.CONSUM"
urban.percentage <- "SP.URB.TOTL.IN.ZS"
forest.area <- "AG.LND.FRST.K2" #sqkm of forest 

# Download data
# 
# Urban_data <- WDI(indicator = urban.percentage, start = 1960, end = format(Sys.Date(), "%Y")) %>%
#   filter(iso3c %in% unique(all.data.wem$iso3c))
# 
# Forest_data <- WDI(indicator = forest.area, start = 1960, end = format(Sys.Date(), "%Y")) %>%
#   filter(iso3c %in% unique(all.data.wem$iso3c))
# 
# 
# GDP_data <- WDI(indicator = gdp_indicator, start = 1960, end = format(Sys.Date(), "%Y")) %>%
#   filter(iso3c %in% unique(all.data.wem$iso3c))
# 
# 
# population_data <- WDI(indicator = population_indicator, start = 1960, end = format(Sys.Date(), "%Y")) %>%
#   filter(iso3c %in% unique(all.data.wem$iso3c))
# 
# #View(population_data %>% filter(is.na(SP.POP.TOTL)))
# 
# #View(GDP_data %>% filter(iso3c == "CIV"))
# 
# tfc_data <- WDI(indicator = tfc_wdi, start = 1960, end = format(Sys.Date(), "%Y")) %>%
#   filter(iso3c %in% unique(all.data.wem$iso3c))

# setdiff(unique(GDP_data$country), unique(all.data.wem.wider$Country)) # What is not found in all.data.wem.wider
#
#
# doing this because GDP/ Population not available for full set of countries
# 
# # Create a list of  original data frames
# data_list <- list(GDP_data, population_data, Forest_data, Urban_data)
# 
# # Apply select() to each data frame to drop the country and iso2c columns
# data_list_cleaned <- map(data_list, ~ select(.x, -country, -iso2c))
# 
# # Perform the full join across all cleaned data frames
# gdp_pop_data_wdi1 <- reduce(data_list_cleaned, full_join, by = c("iso3c", "year"))
# 
# gdp.pop.data.wdi <- gdp_pop_data_wdi1 %>% #full_join(select(GDP_data, -country, - iso2c), population_data, by = c("iso3c", "year")) %>%
#   #merge(tfc_data, by = c("iso3c", "year"))
#   select(Year = year, iso3c, GDP_ppp_pcap = NY.GDP.PCAP.KD, 
#          Population = SP.POP.TOTL, Forest_cover = AG.LND.FRST.K2, Urban_perct = SP.URB.TOTL.IN.ZS) %>%
#   right_join(all.data.wem.wider, by = c("Year", "iso3c")) %>%# Right join to keep only matches with energy data
#   #mutate(Population = Population * 1000) %>%
#   mutate(biomass_pcap = ifelse(!is.na(TJ_Trad_biomass), TJ_Trad_biomass / Population, 0),
#          charcoal_pcap = ifelse(!is.na(TJ_Charcoal), TJ_Charcoal / Population, 0),
#          forest_cover_pcap = ifelse(!is.na(Forest_cover), Forest_cover / Population, NA)) 
# 
# glimpse(gdp.pop.data.wdi)
# 
# saveRDS(gdp.pop.data.wdi, file = here::here("Data", "WDI__Pop_GDP.rds"))

gdp.pop.data.wdi <- readRDS(file = here::here("Data", "WDI__Pop_GDP.rds")) %>%
  mutate(iso3c = case_when(Country == "Kosovo" ~ "XKX", TRUE ~ iso3c))
original_countries <- unique(gdp.pop.data.wdi$Country)

regions.wdi <- read.csv(file = here::here("Data", "continents-according-to-our-world-in-data.csv"), header = TRUE) %>%
  select(Code, Continent)

gdp.pop.data.wdi2 <- gdp.pop.data.wdi %>%
  inner_join(regions.wdi, by = c("iso3c" = "Code"))
joined_countries <- unique(gdp.pop.data.wdi2$Country)

# Find missing countries
missing_countries <- setdiff(original_countries, joined_countries)

# Print missing countries
print(missing_countries)


gdp.pop.data.wdi <- gdp.pop.data.wdi2

#length(unique(gdp.pop.data.wdi$Country.x))
length(unique(gdp.pop.data.wdi$Country))

summary(gdp.pop.data.wdi)

# View(gdp.pop.data.wdi)
# 
# View(gdp.pop.data.wdi %>% filter(is.na(GDP_ppp_pcap)))

# View(GDP_data %>% filter(grepl("Burkina", country)))
# View(all.data.wem.wider %>% filter(grepl("Burkina", Country)))
# 
# View(gdp.pop.data.wdi %>% filter(is.na(Population)))
# 
# View(all.data.wem %>% filter(Country == "Angola"))Country.xView(all.data.wem %>% filter(Country == "Angola"))
# View(gdp.pop.data.wdi %>% filter(Country == "Angola"))
####

excel_file2 <- here::here("Data", "GDP and Population tool v3.1 - April 2024 - IMF update.xlsb")

read_named_region <- function(file, region_names, colskip, rowskip, colnames_take) {
  regions <- lapply(region_names, function(region) {
    openxlsx::read.xlsx(file, namedRegion = region, colNames = colnames_take, #startRow = 1, 
                        skipEmptyRows = rowskip,
                        skipEmptyCols = colskip)
  })
  do.call(cbind, regions)
}

data.gdp.ppp <- read_xlsb(path = excel_file2, range = "GDP_PPP", col_names = TRUE) %>%
  pivot_longer(
    cols = -column.1,       # All columns except "column.1"
    names_to = "year",      # Name of the new "year" column
    values_to = "GDP_PPP_pcap"   # Name of the new "value" column
  ) %>%
  mutate(year = as.integer(sub("^X", "", year))) %>%
  rename(country = column.1)


###################################################################################################################################################
### Exploratory Data Analysis

# select complete cases of variables important

gdp.pop.data.wdi <- as_tibble(gdp.pop.data.wdi) %>%
  filter(Year >= 1990, !is.na(iso3c), !is.na(GDP_ppp_pcap), !is.na(Prop_TFC_Trad_biomass)) %>%
  arrange(Country, Year)

glimpse(gdp.pop.data.wdi)



# Low p-values. Reject HO - evidence to suggest the series are stationary
# 
# View(gdp.pop.data.wdi %>% filter(is.na(biomass_pcap)))
# 
# summary(gdp.pop.data.wdi)
# 
# View((gdp.pop.data.wdi))
# 
# View(gdp.pop.data.wdi %>% filter(is.na(GDP_ppp_pcap)))

# Select only countries determined by ISO-code

# All data in TJ

# ggplotly(ggplot(data = all.data.wem, aes(x= Year, y = TJ, group = factor(Country), colour = factor(Country))) +
#            facet_wrap(~ Continent) +
#   geom_line() + scale_y_continuous(trans = "sqrt"))

ggplotly(ggplot(data = all.data.wem, aes(x= Year, y = Growth, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

# Plot against GDP
ggplotly(ggplot(data = gdp.pop.data.wdi, aes(x= charcoal_pcap, y = biomass_pcap, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = gdp.pop.data.wdi, aes(x= (GDP_ppp_pcap), y = (biomass_pcap), group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = gdp.pop.data.wdi, aes(x= (GDP_ppp_pcap), y = (Prop_TFC_Trad_biomass), group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = gdp.pop.data.wdi, aes(x= (GDP_ppp_pcap), y = (Prop_TFC_Charcoal), group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = gdp.pop.data.wdi, aes(x= (GDP_ppp_pcap), y = (Prop_TFC_Other), group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = gdp.pop.data.wdi, aes(x= log(GDP_ppp_pcap), y = log(Prop_TFC_Trad_biomass), group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = gdp.pop.data.wdi, aes(x= (Year), y = (Prop_TFC_Trad_biomass), group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())


# library(reshape2)  # for melting data
# 
# # Reshape the data to long format
# numeric_vars <- c("GDP_ppp_pcap", "Population", "TJ_Charcoal", "TJ_Res_TFC", 
#                   "TJ_Trad_biomass", "Growth_Charcoal", "Growth_Res_TFC", 
#                   "Growth_Trad_biomass", "Prop_TFC_Charcoal", "Prop_TFC_Trad_biomass", 
#                   "Prop_TFC_Other", "biomass_pcap", "charcoal_pcap", 
#                   "log_GDP_ppp_pcap", "logit_Prop_TFC_Trad_biomass", 
#                   "logit_Prop_TFC_Other")
# 
# # Convert columns to numeric
# cluster.data[numeric_vars] <- lapply(cluster.data[numeric_vars], function(x) {
#   if (is.factor(x) || is.character(x)) as.numeric(as.character(x)) else x
# })
# 
# # Reshape the data to long format
# long_data <- cluster.data %>%
#   select(Year, Country, all_of(numeric_vars)) %>%
#   melt(id.vars = c("Year", "Country"), variable.name = "Variable", value.name = "Value")
# 
# # Plot using ggplot2
# ggplot(long_data, aes(x = Variable, y = Value)) +
#   geom_violin() +
#   facet_wrap(~ Year, scales = "free_y") +  # Facet by Year if needed
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(size = 14), 
#         axis.text.y = element_text(size = 12),
#         plot.title = element_text(size = 16, face = "bold")) +
#   labs(title = "Distribution of Numeric Variables by Year",
#        y = "Value")

# If you want to facet by Country, you can use the following instead:
# ggplot(long_data, aes(x = Variable, y = Value)) +
#   geom_violin() +
#   facet_wrap(~ Country, scales = "free_y") +  # Facet by Country if needed
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(size = 14), 
#         axis.text.y = element_text(size = 12),
#         plot.title = element_text(size = 16, face = "bold")) +
#   labs(title = "Distribution of Numeric Variables by Country",
#        y = "Value")

#############################################################################################################################

#### clustering
#################################################################################################################################
### conduct some sort of cluster analysis to determine clusters of countries most similar

# View(gdp.pop.data.wdi %>% filter(Prop_TFC_Other == 0))

### to do: ifelse(total_tfc_res > 0, then....)

# View(gdp.pop.data.wdi %>% filter(iso3c == "NAM"))

country_data <- gdp.pop.data.wdi %>%
  #filter(Sector == use_sector_id) %>%
  arrange(Country, Year) %>%
  mutate(
    forest_cover_pcap = case_when(
      is.na(forest_cover_pcap) ~ NA_real_,
      TRUE ~ pmax(pmin(forest_cover_pcap, 1 - epsilon), epsilon)
    )) %>%
  mutate(log_GDP_ppp_pcap = log(GDP_ppp_pcap),
         logit_Prop_TFC_Trad_biomass = log(Prop_TFC_Trad_biomass/(1 - Prop_TFC_Trad_biomass)),
         logit_Prop_TFC_Other = log(Prop_TFC_Other/(1 - Prop_TFC_Other)),
         log_Forest_pcap = log(forest_cover_pcap),
         log_Urban = log(Urban_perct))

summary(country_data)

View(country_data %>% filter(Prop_TFC_Other < 0.001))

# Namibia has proportion other very small in 2009
ggplotly(ggplot(data = gdp.pop.data.wdi %>% filter(iso3c == "NAM"), aes(x = Year, y = Prop_TFC_Other)) + geom_line()+ 
           geom_line(aes(y = Prop_TFC_Charcoal)) +
           geom_line(aes(y = Prop_TFC_Trad_biomass)))

# Change to be average of 2008 and 2010 values

 gdp.pop.data.wdi2 <- gdp.pop.data.wdi # %>%
  # mutate(Prop_TFC_Charcoal = case_when(
  #   Year == 2009 & iso3c == "NAM" ~ mean(Prop_TFC_Charcoal[Year %in% c(2008, 2010) & iso3c == "NAM"]),
  #   TRUE ~ Prop_TFC_Charcoal
  # )) %>%
  # mutate(Prop_TFC_Trad_biomass = case_when(
  #   Year == 2009 & iso3c == "NAM" ~ mean(Prop_TFC_Trad_biomass[Year %in% c(2008, 2010) & iso3c == "NAM"]),
  #   TRUE ~ Prop_TFC_Trad_biomass
  # )) %>%
  # mutate(Prop_TFC_Other = case_when(
  #   Year == 2009 & iso3c == "NAM" ~ 1 - Prop_TFC_Charcoal - Prop_TFC_Trad_biomass,
  #   TRUE ~ Prop_TFC_Other
  # ))

ggplotly(ggplot(data = gdp.pop.data.wdi2 %>% filter(iso3c == "NAM"), aes(x = Year, y = Prop_TFC_Other)) + geom_line()+ 
           geom_line(aes(y = Prop_TFC_Charcoal)) +
           geom_line(aes(y = Prop_TFC_Trad_biomass)))
# 
# 
# View(gdp.pop.data.wdi %>% filter(iso3c == "NAM"))
### wht are some countries all 0 for TFC uses?!

#country_data <- gdp.pop.data.wdi2 #%>%
  # #filter(Sector == use_sector_id) %>%
  # arrange(Country, Year) %>%
  # mutate(log_GDP_ppp_pcap = log(GDP_ppp_pcap),
  #        logit_Prop_TFC_Trad_biomass = log(Prop_TFC_Trad_biomass/(1 - Prop_TFC_Trad_biomass)),
  #        logit_Prop_TFC_Other = log(Prop_TFC_Other/(1 - Prop_TFC_Other)))

summary(country_data)

View(country_data %>% filter(is.infinite(logit_Prop_TFC_Trad_biomass)))

infinite.issues <- country_data %>% filter(is.infinite(logit_Prop_TFC_Trad_biomass))
unique(infinite.issues$Country)

country_data <- country_data %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(GDP_pcap_growth = (GDP_ppp_pcap-lag(GDP_ppp_pcap))/ lag(GDP_ppp_pcap))

### Check for outliers
ggplotly(ggplot(data = country_data, aes(x= Year, y = log_GDP_ppp_pcap, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= Year, y = GDP_pcap_growth, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())


ggplotly(ggplot(data = country_data, aes(x= log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

# Polynomial function of GDP_ppp_pcap
ggplotly(ggplot(data = country_data, aes(x= GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= GDP_ppp_pcap, y = logit_Prop_TFC_Other, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= GDP_ppp_pcap, y = Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= Year, y = Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

summary(country_data)

View(country_data)

# library(dtwclust)

# Standardize the numeric columns (GDP_pcap and ES_pcap)
# country_data[, c("GDP_ppp_pcap", "Prop_TFC_Trad_biomass", "Prop_TFC_Charcoal",
#                  "Prop_TFC_Other", "log_GDP_ppp_pcap", "logit_Prop_TFC_Other")] <- scale(country_data[, c("GDP_ppp_pcap", "Prop_TFC_Trad_biomass", "Prop_TFC_Charcoal",
#                                                               "Prop_TFC_Other", "log_GDP_ppp_pcap", "logit_Prop_TFC_Other")])

# Step 3: Fit the Gaussian mixture model using flexmix

# Add a small constant to avoid exact 0 and 1

# Extract the columns to be scaled

country_data_orig <- country_data


# cols_to_scale <- c("log_GDP_ppp_pcap", "logit_Prop_TFC_Trad_biomass")
# 
# # Store the original means and standard deviations
# original_means <- colMeans(country_data[, cols_to_scale], na.rm = TRUE)
# original_sds <- apply(country_data[, cols_to_scale], 2, sd, na.rm = TRUE)
# 
# # Scale the data
# country_data[, cols_to_scale] <- scale(country_data[, cols_to_scale])

# # To reverse the scaling, multiply by the original standard deviation and add the original mean
# country_data[, cols_to_scale] <- sweep(country_data[, cols_to_scale], 2, original_sds, FUN = "*")
# country_data[, cols_to_scale] <- sweep(country_data[, cols_to_scale], 2, original_means, FUN = "+")
# 
# country_data[, c("log_GDP_ppp_pcap", "logit_Prop_TFC_Trad_biomass")] <- scale(country_data[, c("log_GDP_ppp_pcap", "logit_Prop_TFC_Trad_biomass")])

ggplotly(ggplot(data = country_data, aes(x= Year, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

# country_data2 <- country_data %>%
#   mutate(
#     # Adjusted proportions
#     adjusted_Prop_TFC_Trad_biomass = pmax(pmin(Prop_TFC_Trad_biomass, 1 - epsilon), epsilon),
#     
#     # Apply logit transformation
#     adj_logit_Prop_TFC_Trad_biomass = log(adjusted_Prop_TFC_Trad_biomass / (1 - adjusted_Prop_TFC_Trad_biomass)),
#     adjusted_Prop_TFC_Charcoal = pmax(pmin(Prop_TFC_Charcoal, 1 - epsilon), epsilon),
#     adj_logit_Prop_TFC_Charcoal = log(adjusted_Prop_TFC_Charcoal / (1 - adjusted_Prop_TFC_Charcoal)),
#     adjusted_Prop_TFC_Other = 1 - adjusted_Prop_TFC_Trad_biomass - adjusted_Prop_TFC_Charcoal,
#     adj_logit_Prop_TFC_Other = log(adjusted_Prop_TFC_Other / (1 - adjusted_Prop_TFC_Other))
#   )

country_data2 <- country_data 

summary(country_data2)

ggplotly(ggplot(data = country_data2, aes(x= GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data2, aes(x= Year, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data2, aes(x= log(GDP_ppp_pcap), y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data2, aes(x= log_Forest_pcap, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data2, aes(x= log_Urban, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

View(country_data2 %>% filter(Country == "Cyprus"))

# View the transformed data
head(country_data)

ggplotly(ggplot(data = country_data2, aes(x= GDP_ppp_pcap, y = Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

# Convert the data to long format

country_data <- country_data2 %>%
  mutate(ID = factor(as.numeric(factor(Country)))) %>%
  ungroup() %>% 
  mutate(Year = Year - 1990) %>%
  mutate(s.curve.Trad.Biomass = exp(Prop_TFC_Trad_biomass)/ (1 + exp(Prop_TFC_Trad_biomass))) %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(GDP_growth_diff = GDP_pcap_growth - lag(GDP_pcap_growth)) %>%
  ungroup()

summary(country_data)

### EDA
# Stationarity tests

# Convert to panel data frame
panel_data <- pdata.frame(country_data, index = c("Country", "Year"))
# Check the number of observations for each country
obs_count <- table(index(panel_data)$Country)
print(obs_count)

# Option 1: Reduce pmax if necessary
pmax <-  4
pmax_value <- min(4, min(obs_count) - 1) # Ensure pmax is not greater than the minimum obs count - 1

# Option 2: Exclude countries with insufficient observations
sufficient_data <- names(obs_count)[obs_count >= (pmax + 1)]
panel_data <- panel_data[index(panel_data)$Country %in% sufficient_data, ]

# Check for NA values
sum(is.na(panel_data$GDP_pcap_growth))

# Check for NaN or Inf values
sum(is.nan(panel_data$GDP_pcap_growth))
sum(is.infinite(panel_data$GDP_pcap_growth))

panel_data <- panel_data[!is.na(panel_data$GDP_pcap_growth) & 
                           !is.nan(panel_data$GDP_pcap_growth) & 
                           !is.infinite(panel_data$GDP_pcap_growth), ]

panel_data <- panel_data[!is.na(panel_data$forest_cover_pcap) & 
                           !is.nan(panel_data$forest_cover_pcap) & 
                           !is.infinite(panel_data$forest_cover_pcap), ]


collapse::psacf(panel_data$log_GDP_ppp_pcap)
collapse::psacf(panel_data$GDP_pcap_growth)
collapse::pspacf(panel_data$GDP_pcap_growth)

sum(is.na(panel_data$GDP_ppp_pcap))
sum(is.nan(panel_data$GDP_ppp_pcap))
sum(is.infinite(panel_data$GDP_ppp_pcap))

(purtest((panel_data$log_GDP_ppp_pcap), pmax = 1, exo = "trend",
         test = "madwu")) # Trend Stationary series



(purtest((panel_data$GDP_pcap_growth), pmax = 1, exo = "intercept",
         test = "madwu")) # Stationary series

(purtest((panel_data$logit_Prop_TFC_Trad_biomass), pmax = 1, exo = "intercept",
         test = "madwu"))

(purtest((panel_data$log_GDP_ppp_pcap), pmax = 1, exo = "intercept",
         test = "madwu"))

(purtest((panel_data$Urban_perct), pmax = 1, exo = "intercept",
         test = "madwu"))

# sum(is.na(panel_data$Forest_cover))
# (purtest((panel_data$Forest_cover), pmax = 1, exo = "intercept",
#          test = "madwu"))

# Low p-values. Reject HO - evidence to suggest the series are stationary
### Beta regression: https://stats.stackexchange.com/questions/114959/mixture-of-beta-distributions-full-example/115338#115338

# country_data[, c("GDP_ppp_pcap", "Prop_TFC_Trad_biomass", "Prop_TFC_Charcoal",
#                  "Prop_TFC_Other", "log_GDP_ppp_pcap", "logit_Prop_TFC_Other")] <- scale(country_data[, c("GDP_ppp_pcap", "Prop_TFC_Trad_biomass", "Prop_TFC_Charcoal",
#                                                                                                           "Prop_TFC_Other", "log_GDP_ppp_pcap", "logit_Prop_TFC_Other")])
ggplotly(ggplot(data = country_data, aes(x= Year, y = log_GDP_ppp_pcap, group = factor(Country), colour = factor(Country))) +
           facet_wrap(~ Continent) +#facet_wrap(~ Type) +
           geom_line())


ggplotly(ggplot(data = country_data, aes(x= GDP_pcap_growth, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           facet_wrap(~ Continent) +#facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= Urban_perct, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           facet_wrap(~ Continent) +#facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= Year, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           facet_wrap(~ Continent) +#facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= Year, y = GDP_pcap_growth, group = factor(Country), colour = factor(Country))) +
           facet_wrap(~ Continent) +#facet_wrap(~ Type) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= log_GDP_ppp_pcap, y = s.curve.Trad.Biomass, group = factor(Country), colour = factor(Country))) +
           facet_wrap(~ Continent) +
           geom_line())

ggplotly(ggplot(data = country_data, aes(x= log_Urban, y = logit_Prop_TFC_Trad_biomass, group = factor(Country), colour = factor(Country))) +
           #facet_wrap(~ Type) +
           geom_line())

# Remove lg.lag.trans.LFPR for obvious correlation
re.panel_model1.further <- plm(logit_Prop_TFC_Trad_biomass ~ log_GDP_ppp_pcap + 
                                 log_Urban + factor(Continent) + Year,# +
                               #    Educ_mid_years + Prop.WAP, 
                               #+
                               #  Prop.WAP + Lab_qual_growth + lag(Lab_qual_growth) + UN_Urban +
                               #  Lab_quan_growth + lag(Lab_quan_growth),
                               data = country_data, model = "random")
summary(re.panel_model1.further)

####
fe.panel_model1.further <- plm(logit_Prop_TFC_Trad_biomass ~ log_GDP_ppp_pcap + 
                                 log_Urban + factor(Continent) + Year, # +
                               # Educ_mid_years + Prop.WAP, 
                               #+
                               #  Prop.WAP + Lab_qual_growth + lag(Lab_qual_growth) + UN_Urban +
                               #  Lab_quan_growth + lag(Lab_quan_growth),
                               data = country_data, model = "within")
summary(fe.panel_model1.further)

## Test for random vs fixed effects
phtest(fe.panel_model1.further, re.panel_model1.further)


# Fit the Gaussian mixture model using flexmix
set.seed(1652)  # For reproducibility

### Remove NA values

country_data <- country_data[!is.na(country_data$GDP_pcap_growth) & 
                           !is.nan(country_data$GDP_pcap_growth) & 
                           !is.infinite(country_data$GDP_pcap_growth), ]

# Went with this as intial clustering
lcgaMix <- stepFlexmix(.~ .|Country, k = 1:10, nrep = 200, # Ensure nested by correct country identifiers if changed before
                       model = FLXMRglmfix(logit_Prop_TFC_Trad_biomass ~  log_GDP_ppp_pcap + Urban_perct + Year, # + Prop_TFC_Other,
                                           varFix=FALSE), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
                       control = list(iter.max = 1000, minprior = 0))

# # # GMM with random intercept and slope
# # # 
# gmmMix <- stepFlexmix(.~ .|Country, k = 1:6, nrep = 200, model
#                        = FLXMRlmm(logit_Prop_TFC_Trad_biomass ~ log_GDP_ppp_pcap, random =  ~ 1,
#                                   varFix=c(Random = FALSE, Residual = TRUE)), data = country_data, control
#                        = list(iter.max = 1000, minprior = 0))
# 
# saveRDS(gmmMix, file = here::here("Data", "cluster_gmm_1.rds"))

gmmMix <- readRDS(file = here::here("Data", "cluster_gmm_1.rds"))
### Plotting trajectory
### https://stackoverflow.com/questions/77607482/group-based-trajectory-modeling-in-r

# betaMix <- betamix(Prop_TFC_Trad_biomass ~ poly(GDP_ppp_pcap, 2), data = country_data, k = 1:3, country_id, nstart = 200,
#                    FLXcontrol = list(iter.max = 1000, minprior = 0))
# 
# betaMix <- betamix(Prop_TFC_Trad_biomass ~ (GDP_ppp_pcap), data = country_data, k = 2, country_id, nstart = 200,
#                    FLXcontrol = list(iter.max = 1000, minprior = 0))
# 
# summary(country_data)
# 
# country_data3 <- country_data
# 
# country_data3[, c("GDP_ppp_pcap")] <- scale(country_data3[, c("GDP_ppp_pcap")])
# 
# summary(country_data3)
# 
# ggplotly(ggplot(data = country_data3, aes(x= GDP_ppp_pcap, y = Prop_TFC_Trad_biomass, group = factor(ID), colour = factor(ID))) +
#            #facet_wrap(~ Type) +
#            geom_line())
# 
# betaMix <- betamix(Prop_TFC_Trad_biomass ~ GDP_ppp_pcap, data = country_data3, k = 3, nstart = 10,
#                  country_data3$ID,
#                    #FLXcontrol = list(iter.max = 20, minprior = 0), 
#                    #extra_components = extraComponent(type = "uniform", coef = 0.01, delta = 0.005)
#                    )

# lcgaMix <- stepFlexmix(.~ .|Country, k = 1:10, nrep = 200, # Ensure nested by correct country identifiers if changed before
#                        model = FLXMRglmfix(logit_Prop_TFC_Trad_biomass ~ poly(Year,2), # + Prop_TFC_Other, 
#                                            varFix=FALSE), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
#                        control = list(iter.max = 1000, minprior = 0))




# lcgaMix <- stepFlexmix(.~ .|Country, k = 1:10, nrep = 200, # Ensure nested by correct country identifiers if changed before
#                        model = FLXMRglmfix(logit_Prop_TFC_Trad_biomass ~ log_GDP_ppp_pcap, # + Prop_TFC_Other, 
#                                            varFix=FALSE), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
#                        control = list(iter.max = 1000, minprior = 0))


# lcgaMix <- stepFlexmix(.~ .|Country, k = 1:10, nrep = 200, # Ensure nested by correct country identifiers if changed before
#                        model = FLXMRglmfix(Prop_TFC_Trad_biomass ~ poly(GDP_ppp_pcap,2), # + Prop_TFC_Other, 
#                                            varFix=FALSE), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
#                        control = list(iter.max = 1000, minprior = 0))

# saveRDS(lcgaMix, file = here::here( "Data", "cluster_lcga_1.rds"))

#saveRDS(gmmMix, file = here("Ladder estimation", "Data", "1_IS_HI_phase1_cluster_gmm.rds"))


lcgaMix <- readRDS(file = here::here("Data", "cluster_lcga_1.rds"))

# lcgaMix2 <- stepFlexmix(.~ .|Country, k = 1:10, nrep = 200, # Ensure nested by correct country identifiers if changed before
#                        model = FLXMRglmfix(Prop_TFC_Trad_biomass ~ poly(GDP_ppp_pcap,2), # + Prop_TFC_Other, 
#                                            varFix=FALSE), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
#                        control = list(iter.max = 1000, minprior = 0))
# 
# lcgaMix <- lcgaMix2

# 
# beta_mix <- betamix(Prop_TFC_Trad_biomass ~ GDP_ppp_pcap | Country, 
#                     data = country_data, k = 2:4)


# lcgaMix <- stepFlexmix(. ~ . | Country,
#                        k = 1:6, nrep = 100,
#                        model = FLXMRglmfix(Prop_TFC_Trad_biomass ~ GDP_ppp_pcap,
#                                            family = binomial(link = "logit"), 
#                                            varFix = FALSE),
#                        data = country_data,
#                        control = list(iter.max = 500, minprior = 0))

# lcgaMix <- stepFlexmix(.~ .|Country, k = 1:6, nrep = 100, # Ensure nested by correct country identifiers if changed before
#                        model = FLXMRglmfix(Prop_TFC_Trad_biomass ~ poly(GDP_ppp_pcap,2), varFix=FALSE), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
#                        control = list(iter.max = 500, minprior = 0))



# lcgaMix <- stepFlexmix(.~ .|Country, k = 1:6, nrep = 100, # Ensure nested by correct country identifiers if changed before
#                        model = FLXMRglmfix(logit_Prop_TFC_Trad_biomass ~ poly(GDP_ppp_pcap,2) , varFix=FALSE), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
#                        control = list(iter.max = 500, minprior = 0))
# lcgaMix2 <- stepFlexmix(.~ .|Country, k = 1:6, nrep = 100, # Ensure nested by correct country identifiers if changed before
#                        model = FLXMRglmfix(Prop_TFC_Trad_biomass ~ GDP_ppp_pcap, varFix=FALSE,
#                                            family = "gaussian"), data = country_data,  # Assuming a linear relationship - investigate NL mixtures
#                        control = list(iter.max = 500, minprior = 0))
# # 

# # # 

# Extract the BIC values from the 'lcgaMix' object
bic_values <- BIC(lcgaMix)#BIC(lcgaMix)#BIC(gmmMix)#

# Plot the BIC values as a fit statistic curve
plot(1:10, bic_values, type = "b", xlab = "Number of Clusters (K)", ylab = "BIC Value",
     main = "BIC Value vs. Number of Clusters", lwd = 4)

# Add a horizontal line to indicate the minimum BIC value
abline(h = min(bic_values), col = "red", lwd = 4)

# Add a legend for the horizontal line
legend("topright", legend = "Optimal K", col = "red", lty = 1, cex = 0.8, lwd = 4)

# Elbow at K = 4
### https://cran.r-project.org/web/packages/flexmix/vignettes/mixture-regressions.pdf
lcga5 <- getModel(lcgaMix, which = 3) #getModel(lcgaMix, which = 3) #getModel(gmmMix, which = 6)#
(lcga5_summary  <- summary(lcga5))

# Capture the summary output
lcga5_summary <- capture.output(summary(lcga5))

# Define the path using `here`
html_file <- here::here("Plots", "lcga3_summary.html")

# Write the HTML content to the file
html_conn <- file(html_file, "w")
writeLines("<html><head><title>LCGA5 Model Summary</title></head><body>", html_conn)
writeLines("<h2>LCGA5 Model Summary</h2>", html_conn)
writeLines("<pre>", html_conn)
writeLines(lcga5_summary, html_conn)
writeLines("</pre></body></html>", html_conn)
close(html_conn)

#tidy(summary(refit(lcga5)))

parameters(lcga5)

# Example refit (replace with your actual refit function)
lcga5_refit <- refit(lcga5)

# Capture the summary output
summary_output <- capture.output(summary(lcga5_refit))

# Define the path using `here`
html_file <- here::here("Plots", "model_3_clusters.html")

# Write the HTML content to the file
html_conn <- file(html_file, "w")
writeLines("<html><head><title>Model Summary Report</title></head><body>", html_conn)
writeLines("<h2>Summary of Refitted Model</h2>", html_conn)
writeLines("<pre>", html_conn)
writeLines(summary_output, html_conn)
writeLines("</pre></body></html>", html_conn)
close(html_conn)

# Optionally print the path to confirm
print(html_file)

# Add clusters to data_set

cluster.data <- country_data 
#select(country_id, Country, Year, Sector, ES_pcap, GDP_pcap) %>%
#filter(Sector == use_sector_id)

cluster.data$cluster <- clusters(lcga5)

table(cluster.data$cluster)

xtabs(~ cluster + Country, data = cluster.data)

summary_df <- cluster.data %>%
  group_by(cluster) %>%
  summarize(Countries = paste(unique(Country), collapse = ", "), .groups = 'drop')

summary_df <- aggregate(Country ~ cluster, data = cluster.data, 
                        FUN = function(x) paste(unique(x), collapse = ", "))


# View the summary
(summary_df)

library(writexl)
write_xlsx(summary_df, path = here::here("Data", "summary_df.xlsx"))
#######################################
##################### Plotting for presentation
# syntax: font_add(family = "<family_name>", regular = "/path/to/font/file")
font_add("ShellMedium", "ShellMedium.ttf")
font_families()

## automatically use showtext for new devices
showtext_auto()

#Plot: need to open Windows graphics device as showtext does not work well with RStudio built-in graphics device
windows()

rel.size <- 2

tt1 <- theme(
  axis.title.y = element_text(size = 18 * rel.size, family = "ShellMedium"),
  axis.title.x = element_text(size = 18 * rel.size, family = "ShellMedium"),
  axis.text.y = element_text(size = 16 * rel.size, family = "ShellMedium"),
  axis.text.x = element_text(size = 16 * rel.size, family = "ShellMedium"),
  plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
  legend.position = "none",
  legend.text = element_text(size = 18 * rel.size, family = "ShellMedium"),
  legend.title = element_blank(),
  legend.background = element_rect(fill = "white", color = "black"),
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "gray", linetype = "dashed"),
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 18 * rel.size, family = "ShellMedium", hjust = 0.5),  # Center-align the title
  plot.caption = element_blank(),
  plot.subtitle = element_blank()
)

tt2 <- theme(
  axis.title.y = element_text(size = 18 , family = "ShellMedium"),
  axis.title.x = element_text(size = 18, family = "ShellMedium"),
  axis.text.y = element_text(size = 16, family = "ShellMedium"),
  axis.text.x = element_text(size = 16, family = "ShellMedium"),
  plot.margin = margin(l = 40, r = 40, t = 60, b = 40),
  legend.position =  c(1.05, 0.9),
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

plot1 <- (ggplot(data = cluster.data, aes(x= (Year), y = (logit_Prop_TFC_Trad_biomass), 
                                          group = factor(Country), colour = factor(Continent))) +
            facet_wrap(~ cluster) +
            geom_point() +
            geom_line() + 
            labs(title = "Proportion TFC as Traditional Biomass in Residential Heating and Cooking",
                 x = "Year",
                 y = "Proportion of TFC from Traditional Biomass",
                 color = "Continent") + tt1)

(plot1a <- ggplotly(plot1 + tt2))

plot1 <- (ggplot(data = cluster.data, aes(x= (GDP_pcap_growth), y = (logit_Prop_TFC_Trad_biomass), 
                                          group = factor(Country), colour = factor(Continent))) +
            facet_wrap(~ cluster) +
            geom_point() +
            geom_line() + 
            labs(title = "Proportion TFC as Traditional Biomass in Residential Heating and Cooking",
                 x = "GDP_pcap_growth",
                 y = "logit_Prop_TFC_Trad_biomass",
                 color = "Continent") + tt1)

(plot1a <- ggplotly(plot1 + tt2))


plot1 <- (ggplot(data = cluster.data, aes(x= (Year), y = (Prop_TFC_Trad_biomass), 
                                          group = factor(Country), colour = factor(Continent))) +
            facet_wrap(~ cluster) +
            geom_point() +
            geom_line() + 
            labs(title = "Proportion TFC as Traditional Biomass in Residential Heating and Cooking",
                 x = "Year",
                 y = "Proportion of TFC from Traditional Biomass",
                 color = "Country") + tt1)

plot1a <- ggplotly(plot1 + tt2)

ggsave(here::here("Plots", "plot1.png"), plot = plot1, dpi = 300)

htmlwidgets::saveWidget(plot1a, file = here::here("Plots", "prop_biomass_time.html"), selfcontained = TRUE)

##############################################################################

plot1 <- (ggplot(data = cluster.data, aes(x= (GDP_ppp_pcap), y = (Prop_TFC_Trad_biomass), 
                                          group = factor(Country), colour = factor(Country))) +
            #facet_wrap(~ Type) +
            geom_point() +
            geom_line() + 
            labs(title = "Proportion TFC as Traditional Biomass in Residential Heating and Cooking",
                 x = "GDP PPP / cap (Real USD 2015)",
                 y = "Proportion of TFC from Traditional Biomass",
                 color = "Country") + tt1)

plot1a <- ggplotly(plot1 + tt2)

ggsave(here::here("Plots", "plot2.png"), plot = plot1, dpi = 300)

htmlwidgets::saveWidget(plot1a, file = here::here("Plots", "prop_biomass_gdp.html"), selfcontained = TRUE)

##############################################################################
plot1 <- (ggplot(data = cluster.data, aes(x= (log_Urban), y = (logit_Prop_TFC_Trad_biomass), 
                                          group = factor(Country), colour = factor(Continent))) +
            facet_wrap(~ cluster) +
            geom_point() +
            geom_line() + 
            labs(title = "Proportion TFC as Traditional Biomass in Residential Heating and Cooking",
                 x = "log(GDP PPP / cap (Real USD 2015))",
                 y = "logit(Proportion of TFC from Traditional Biomass)",
                 color = "Country") + tt1)

plot1a <- ggplotly(plot1 + tt2)


plot1 <- (ggplot(data = cluster.data, aes(x= (log_GDP_ppp_pcap), y = (logit_Prop_TFC_Trad_biomass), 
                                          group = factor(Country), colour = factor(Continent))) +
            facet_wrap(~ cluster) +
            geom_point() +
            geom_line() + 
            labs(title = "Proportion TFC as Traditional Biomass in Residential Heating and Cooking",
                 x = "log(GDP PPP / cap (Real USD 2015))",
                 y = "logit(Proportion of TFC from Traditional Biomass)",
                 color = "Country") + tt1)

plot1a <- ggplotly(plot1 + tt2)

ggsave(here::here("Plots", "plot3.png"), plot = plot1, dpi = 300)

htmlwidgets::saveWidget(plot1a, file = here::here("Plots", "logit_log_prop_biomass_gdp.html"), selfcontained = TRUE)


#############################################################################################################################






# summary_df <- cluster.data %>%
#   group_by(cluster) %>%
#   summarize(Countries = list(unique(Country)), .groups = 'drop')
# 
# # Convert list of countries to a wide format
# wide_df <- summary_df %>%
#   unnest_wider(Countries, names_sep = "_") %>%
#   arrange(cluster)  # Optional: arrange by cluster if needed
# 
# # Create a new workbook and add a worksheet
# wb <- createWorkbook()
# addWorksheet(wb, "Summary")
# 
# # Write the data to the worksheet
# writeData(wb, sheet = 1, wide_df)
# 
# # Adjust column widths
# setColWidths(wb, sheet = 1, cols = 1:ncol(wide_df), widths = "auto")
# 
# # Add header
# writeData(wb, sheet = 1, x = "Cluster", startCol = 1, startRow = 1)
# writeData(wb, sheet = 1, x = "Countries", startCol = 2, startRow = 1)
# 
# # Save the workbook
# saveWorkbook(wb, file = here::here("Data", "summary_df.xlsx"), overwrite = TRUE)
# 

# log_GDP_ppp_pcap found to be trend stationary - create trend variable

# 13 August 2024
# Remove lg.lag.trans.LFPR for obvious correlation

cluster.data.panel <- pdata.frame(cluster.data, index = c("Country", "Year"))

re.panel_model1.further <- plm(logit_Prop_TFC_Trad_biomass ~ log_GDP_ppp_pcap + Year + 
                                 Urban_perct + factor(cluster),# +
                               #    Educ_mid_years + Prop.WAP, 
                               #+
                               #  Prop.WAP + Lab_qual_growth + lag(Lab_qual_growth) + UN_Urban +
                               #  Lab_quan_growth + lag(Lab_quan_growth),
                               data = cluster.data.panel, model = "random")
summary(re.panel_model1.further)

####

fe.panel_model1.further <- plm(logit_Prop_TFC_Trad_biomass ~ log_GDP_ppp_pcap +
                                 Urban_perct, # +
                               # Educ_mid_years + Prop.WAP, 
                               #+
                               #  Prop.WAP + Lab_qual_growth + lag(Lab_qual_growth) + UN_Urban +
                               #  Lab_quan_growth + lag(Lab_quan_growth),
                               data = cluster.data.panel, model = "within")
summary(fe.panel_model1.further)


fe.panel_model1.further <- plm(logit_Prop_TFC_Trad_biomass ~ log_GDP_ppp_pcap +
                                 Urban_perct + factor(cluster), # +
                               # Educ_mid_years + Prop.WAP, 
                               #+
                               #  Prop.WAP + Lab_qual_growth + lag(Lab_qual_growth) + UN_Urban +
                               #  Lab_quan_growth + lag(Lab_quan_growth),
                               data = cluster.data.panel, model = "within")
summary(fe.panel_model1.further)
fixef(fe.panel_model1.further)
# ## Test for random vs fixed effects
# phtest(fe.panel_model1.further, re.panel_model1.further)



#################
# Plot countries by clusters and trajectories

p1 <- ggplotly(ggplot(data = cluster.data, aes(x = Year, y = logit_Prop_TFC_Trad_biomass, 
                                               color = factor(Continent), group = factor(Country))) +
                 facet_wrap(facets = vars(cluster)) +
                 geom_point() +
                 stat_smooth(formula = y ~ poly(x,2), method = "lm", se = FALSE)) #+
# scale_y_continuous(trans = "sqrt"))

p1

p1 <- ggplotly(ggplot(data = cluster.data, aes(x = Year, y = Prop_TFC_Trad_biomass, 
                                               color = factor(Country), group = factor(Country))) +
                 facet_wrap(facets = vars(cluster)) +
                 geom_point() +
                 stat_smooth(formula = y ~ poly(x,2), method = "lm", se = FALSE)) #+
# scale_y_continuous(trans = "sqrt"))

p1

p1 <- ggplotly(ggplot(data = cluster.data, aes(x = GDP_ppp_pcap, y = Prop_TFC_Trad_biomass, 
                                               color = factor(Continent), group = factor(Country))) +
                 facet_wrap(facets = vars(cluster)) +
                 geom_point() +
                 stat_smooth(formula = y ~ poly(x,2), method = "lm", se = FALSE)) #+
# scale_y_continuous(trans = "sqrt"))

p1

p1 <- (ggplot(data = cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass, 
                                               color = factor(Country), group = factor(Country))) +
                 facet_wrap(facets = vars(cluster)) +
                 geom_point() +
                 geom_line())
                 #stat_smooth(formula = y ~ poly(x,1), method = "lm", se = FALSE)) #+
# scale_y_continuous(trans = "sqrt"))

model <- lcga5


# # Transpose parameters(model) to work with it more easily
# param_matrix <- t(parameters(model))
# 
# # Define the polynomial degree 
# degree <- 1
# 
# # Create a sequence of x values for the line
# # x_values_time <- seq(min(cluster.data$Year), max(cluster.data$Year), length.out = 100)
# x_values_gdp <- seq(min(cluster.data$log_GDP_ppp_pcap), max(cluster.data$log_GDP_ppp_pcap), length.out = 100)
# 
# # Calculate polynomial values for each component
# polynomial_lines <- do.call(rbind, lapply(seq_len(nrow(param_matrix)), function(i) {
#   intercept <- param_matrix[i, "coef.(Intercept)"]
#   coeffs <- param_matrix[i, c("coef.log_GDP_ppp_pcap")]
#   #y_values_year <- (intercept + coeffs[1] * x_values_time + coeffs[2] * (x_values_time^2))
#   y_values_gdp <- intercept + coeffs[1] * (x_values_gdp)
#   #y_values <- exp(intercept + coeffs[1] * x_values + coeffs[2] * x_values^2) - 1e-05
#   data.frame(x_gdp = x_values_gdp, y_gdp = y_values_gdp, cluster = factor(i))
# }))
# 
# # Plotting
# 
# ggplot(cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass)) +
#   geom_point(aes(col = as.factor(model@cluster))) + 
#   geom_line(data = polynomial_lines, aes(x = x_gdp, y = y_gdp, col = cluster))

###################################################################################################
###################################################################################################
##### Nice plots
# Convert parameters to a data frame
params_df <- as.data.frame(t(parameters(model)))

# Assuming `params_df` includes a column `cluster` indicating the cluster for each line
params_df$cluster <- factor(seq_along(params_df$`coef.(Intercept)`))

# Ensure each cluster has unique parameters
# You might need to aggregate or select appropriate parameters based on your model

# Create a mapping for clusters and parameters
params_df <- params_df %>%
  mutate(cluster = factor(seq_len(nrow(params_df))))

# Prepare the ggplot object
# p1 <- ggplot(data = cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass,
#                                       color = factor(Country), group = factor(Country))) +
#   geom_point() +
#   geom_abline(data = params_df, aes(intercept = `coef.(Intercept)`, slope = `coef.log_GDP_ppp_pcap`, color = cluster)) +
#   facet_wrap(facets = vars(cluster)) +
#   labs(title = "Plot with Ablines Per Cluster",
#        x = "log(GDP PPP / cap) (USD Constant 2015)",
#        y = "logit(Proportion of TFC from Traditional Biomass)",
#        color = "Cluster")

p1 <- ggplot(data = cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass,
                                      color = factor(Country), group = factor(Country))) +
  geom_point() +
  geom_line() + # Line per country
  geom_abline(data = params_df, aes(intercept = `coef.(Intercept)`, slope = `coef.log_GDP_ppp_pcap`), color = "black", linewidth = 2) +
  facet_wrap(facets = vars(cluster)) +
  labs(title = "Latent Classes of Traditional Biomass in Residential Heating and Cooking",
       x = "log(GDP PPP / cap) (USD Constant 2015)",
       y = "logit(Proportion of TFC from Traditional Biomass)",
       color = "Country") + tt1 + theme(legend.position = "none")



p2 <- ggplotly(p1 + tt2)

ggsave(here::here("Plots", "plot_cluster_3.png"), plot = p1, dpi = 300)

htmlwidgets::saveWidget(p2, file = here::here("Plots", "cluster_3.html"), selfcontained = TRUE)

#### 4 classes

lcga5 <- getModel(lcgaMix, which = 4)
(lcga5_summary  <- summary(lcga5))

# Capture the summary output
lcga5_summary <- capture.output(summary(lcga5))

# Define the path using `here`
html_file <- here::here("Plots", "lcga4_summary.html")

# Write the HTML content to the file
html_conn <- file(html_file, "w")
writeLines("<html><head><title>LCGA5 Model Summary</title></head><body>", html_conn)
writeLines("<h2>LCGA5 Model Summary</h2>", html_conn)
writeLines("<pre>", html_conn)
writeLines(lcga5_summary, html_conn)
writeLines("</pre></body></html>", html_conn)
close(html_conn)


# Example refit (replace with your actual refit function)
lcga5_refit <- refit(lcga5)

# Capture the summary output
summary_output <- capture.output(summary(lcga5_refit))

# Define the path using `here`
html_file <- here::here("Plots", "model_4_clusters.html")

# Write the HTML content to the file
html_conn <- file(html_file, "w")
writeLines("<html><head><title>Model Summary Report</title></head><body>", html_conn)
writeLines("<h2>Summary of Refitted Model</h2>", html_conn)
writeLines("<pre>", html_conn)
writeLines(summary_output, html_conn)
writeLines("</pre></body></html>", html_conn)
close(html_conn)

# Optionally print the path to confirm
print(html_file)

parameters(lcga5)
# Add clusters to data_set

cluster.data <- country_data 
#select(country_id, Country, Year, Sector, ES_pcap, GDP_pcap) %>%
#filter(Sector == use_sector_id)

cluster.data$cluster <- clusters(lcga5)

table(cluster.data$cluster)

xtabs(~ cluster + Country, data = cluster.data)

summary_df <- cluster.data %>%
  group_by(cluster) %>%
  summarize(Countries = paste(unique(Country), collapse = ", "), .groups = 'drop')

summary_df <- aggregate(Country ~ cluster, data = cluster.data, 
                        FUN = function(x) paste(unique(x), collapse = ", "))


# View the summary
(summary_df)

library(writexl)
write_xlsx(summary_df, path = here::here("Data", "4class_summary_df.xlsx"))
# ## Plotting trajectories
# 
model <- getModel(lcgaMix, which = 4)
# Convert parameters to a data frame
params_df <- as.data.frame(t(parameters(model)))

# Assuming `params_df` includes a column `cluster` indicating the cluster for each line
params_df$cluster <- factor(seq_along(params_df$`coef.(Intercept)`))

# Ensure each cluster has unique parameters
# You might need to aggregate or select appropriate parameters based on your model

# Create a mapping for clusters and parameters
params_df <- params_df %>%
  mutate(cluster = factor(seq_len(nrow(params_df))))

# Prepare the ggplot object
# p1 <- ggplot(data = cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass,
#                                       color = factor(Country), group = factor(Country))) +
#   geom_point() +
#   geom_abline(data = params_df, aes(intercept = `coef.(Intercept)`, slope = `coef.log_GDP_ppp_pcap`, color = cluster)) +
#   facet_wrap(facets = vars(cluster)) +
#   labs(title = "Plot with Ablines Per Cluster",
#        x = "log(GDP PPP / cap) (USD Constant 2015)",
#        y = "logit(Proportion of TFC from Traditional Biomass)",
#        color = "Cluster")

p1 <- ggplot(data = cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass,
                                      color = factor(Country), group = factor(Country))) +
  geom_point() +
  geom_line() + # Line per country
  geom_abline(data = params_df, aes(intercept = `coef.(Intercept)`, slope = `coef.log_GDP_ppp_pcap`), color = "black", linewidth = 2) +
  facet_wrap(facets = vars(cluster)) +
  labs(title = "Latent Classes of Traditional Biomass in Residential Heating and Cooking",
       x = "log(GDP PPP / cap) (USD Constant 2015)",
       y = "logit(Proportion of TFC from Traditional Biomass)",
       color = "Country")+ tt1 + theme(legend.position = "none")

p2 <- ggplotly(p1 + tt2)

ggsave(here::here("Plots", "plot_cluster_4.png"), plot = p1, dpi = 300)

htmlwidgets::saveWidget(p2, file = here::here("Plots", "cluster_4.html"), selfcontained = TRUE)

#### 5 classes

lcga5 <- getModel(lcgaMix, which = 5)
(lcga5_summary  <- summary(lcga5))

# Capture the summary output
lcga5_summary <- capture.output(summary(lcga5))

# Define the path using `here`
html_file <- here::here("Plots", "lcga5_summary.html")

# Write the HTML content to the file
html_conn <- file(html_file, "w")
writeLines("<html><head><title>LCGA5 Model Summary</title></head><body>", html_conn)
writeLines("<h2>LCGA5 Model Summary</h2>", html_conn)
writeLines("<pre>", html_conn)
writeLines(lcga5_summary, html_conn)
writeLines("</pre></body></html>", html_conn)
close(html_conn)

# Example refit (replace with your actual refit function)
lcga5_refit <- refit(lcga5)

# Capture the summary output
summary_output <- capture.output(summary(lcga5_refit))

# Define the path using `here`
html_file <- here::here("Plots", "model_5_clusters.html")

# Write the HTML content to the file
html_conn <- file(html_file, "w")
writeLines("<html><head><title>Model Summary Report</title></head><body>", html_conn)
writeLines("<h2>Summary of Refitted Model</h2>", html_conn)
writeLines("<pre>", html_conn)
writeLines(summary_output, html_conn)
writeLines("</pre></body></html>", html_conn)
close(html_conn)

# Optionally print the path to confirm
print(html_file)

parameters(lcga5)

# Example refit (replace with your actual refit function)
lcga5_refit <- refit(lcga5)

# Capture the summary output
summary_output <- capture.output(summary(lcga5_refit))

# Define the path using `here`
html_file <- here::here("Data", "model1.html")

# Write the HTML content to the file
html_conn <- file(html_file, "w")
writeLines("<html><head><title>Model Summary Report</title></head><body>", html_conn)
writeLines("<h2>Summary of Refitted Model</h2>", html_conn)
writeLines("<pre>", html_conn)
writeLines(summary_output, html_conn)
writeLines("</pre></body></html>", html_conn)
close(html_conn)

# Optionally print the path to confirm
print(html_file)


# Add clusters to data_set

cluster.data <- country_data 
#select(country_id, Country, Year, Sector, ES_pcap, GDP_pcap) %>%
#filter(Sector == use_sector_id)

cluster.data$cluster <- clusters(lcga5)

table(cluster.data$cluster)

xtabs(~ cluster + Country, data = cluster.data)

summary_df <- cluster.data %>%
  group_by(cluster) %>%
  summarize(Countries = paste(unique(Country), collapse = ", "), .groups = 'drop')

summary_df <- aggregate(Country ~ cluster, data = cluster.data, 
                        FUN = function(x) paste(unique(x), collapse = ", "))


# View the summary
(summary_df)

library(writexl)
write_xlsx(summary_df, path = here::here("Data", "4class_summary_df.xlsx"))
# ## Plotting trajectories
# 
model <- getModel(lcgaMix, which = 5)
# Convert parameters to a data frame
params_df <- as.data.frame(t(parameters(model)))

# Assuming `params_df` includes a column `cluster` indicating the cluster for each line
params_df$cluster <- factor(seq_along(params_df$`coef.(Intercept)`))

# Ensure each cluster has unique parameters
# You might need to aggregate or select appropriate parameters based on your model

# Create a mapping for clusters and parameters
params_df <- params_df %>%
  mutate(cluster = factor(seq_len(nrow(params_df))))

# Prepare the ggplot object
# p1 <- ggplot(data = cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass,
#                                       color = factor(Country), group = factor(Country))) +
#   geom_point() +
#   geom_abline(data = params_df, aes(intercept = `coef.(Intercept)`, slope = `coef.log_GDP_ppp_pcap`, color = cluster)) +
#   facet_wrap(facets = vars(cluster)) +
#   labs(title = "Plot with Ablines Per Cluster",
#        x = "log(GDP PPP / cap) (USD Constant 2015)",
#        y = "logit(Proportion of TFC from Traditional Biomass)",
#        color = "Cluster")

p1 <- ggplot(data = cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass,
                                      color = factor(Country), group = factor(Country))) +
  geom_point() +
  geom_line() + # Line per country
  geom_abline(data = params_df, aes(intercept = `coef.(Intercept)`, slope = `coef.log_GDP_ppp_pcap`), color = "black", linewidth = 2) +
  facet_wrap(facets = vars(cluster)) +
  labs(title = "Latent Classes of Traditional Biomass in Residential Heating and Cooking",
       x = "log(GDP PPP / cap) (USD Constant 2015)",
       y = "logit(Proportion of TFC from Traditional Biomass)",
       color = "Country")+ tt1 + theme(legend.position = "none")

p2 <- ggplotly(p1 + tt2)

ggsave(here::here("Plots", "plot_cluster_5.png"), plot = p1, dpi = 300)

htmlwidgets::saveWidget(p2, file = here::here("Plots", "cluster_5.html"), selfcontained = TRUE)

# ggplot(cluster.data, aes(x=GDP_ppp_pcap,y=Prop_TFC_Trad_biomass))+
#   geom_point()+
#   geom_point(aes(col = as.factor(model@cluster))) +
#   geom_abline(data = as.data.frame(t(parameters(model))), aes(intercept = `coef.(Intercept)`, slope = `coef.Time`, col = as.factor(seq_along(sigma))))
# 
# data %<>% group_by(ID)%>%
#   dplyr::complete(t = seq(min(t), max(t), by = 1)) %>%
#   mutate(Y = zoo::na.approx(Y))
# 
# 
# print(colnames(parameters(model)))
# print(rownames(parameters(model)))
# # Assuming `parameters(model)` provides coefficients in a matrix
# poly_coefficients <- parameters(model)[, c("coef.(Intercept)", "coef.poly(GDP_ppp_pcap, 2)1", "coef.poly(GDP_ppp_pcap, 2)2")]
# 
# # Create a sequence of x values for the line
# x_values <- seq(min(cluster.data$GDP_ppp_pcap), max(cluster.data$GDP_ppp_pcap), length.out = 100)
# 
# # Calculate polynomial values for each component
# polynomial_lines <- do.call(rbind, lapply(1:ncol(poly_coefficients), function(i) {
#   intercept <- poly_coefficients[i, "coef.(Intercept)"]
#   coeff1 <- poly_coefficients[i, "coef.poly(GDP_ppp_pcap, 2)1"]
#   coeff2 <- poly_coefficients[i, "coef.poly(GDP_ppp_pcap, 2)2"]
#   y_values <- intercept + coeff1 * x_values + coeff2 * x_values^2
#   data.frame(x = x_values, y = y_values, component = factor(i))
# }))
# 
# ggplot(cluster.data, aes(x = GDP_ppp_pcap, y = Prop_TFC_Trad_biomass)) +
#   geom_point(aes(col = as.factor(model@cluster))) +
#   geom_line(data = polynomial_lines, aes(x = x, y = y, col = component))

# Transpose parameters(model) to work with it more easily
param_matrix <- t(parameters(model))

# Define the polynomial degree (2 in this case)
degree <- 1

# Create a sequence of x values for the line
x_values_time <- seq(min(cluster.data$Year), max(cluster.data$Year), length.out = 100)
x_values_gdp <- seq(min(cluster.data$log_GDP_ppp_pcap), max(cluster.data$log_GDP_ppp_pcap), length.out = 100)

# Calculate polynomial values for each component
polynomial_lines <- do.call(rbind, lapply(seq_len(nrow(param_matrix)), function(i) {
  intercept <- param_matrix[i, "coef.(Intercept)"]
  coeffs <- param_matrix[i, c("coef.poly(Year, 2)1", "coef.poly(Year, 2)2", "coef.log(GDP_ppp_pcap)")]
  y_values_year <- (intercept + coeffs[1] * x_values_time + coeffs[2] * (x_values_time^2))
  y_values_gdp <- intercept + coeffs[3] * (x_values_gdp)
  #y_values <- exp(intercept + coeffs[1] * x_values + coeffs[2] * x_values^2) - 1e-05
  data.frame(x_year = x_values_time, x_gdp = x_values_gdp, y_year = y_values_year, y_gdp = y_values_gdp, component = factor(i))
}))

# Plotting

ggplot(cluster.data, aes(x = log_GDP_ppp_pcap, y = logit_Prop_TFC_Trad_biomass)) +
  geom_point(aes(col = as.factor(model@cluster))) + 
  geom_line(data = polynomial_lines, aes(x = x_gdp, y = y_gdp, col = component))


ggplot(cluster.data, aes(x = Year, y = logit_Prop_TFC_Trad_biomass)) +
  geom_point(aes(col = as.factor(model@cluster))) +
  geom_line(data = polynomial_lines, aes(x = x_year, y = y_year, col = component))





###########################################################################################################
###########################################################################################################
###########################################################################################################

#############################################################################################################################
eq1 <- (Prop_TFC_Trad_biomass ~ GDP_ppp_pcap + Prop_TFC_Other)

# Example for cluster == 1
data_cluster1 <- subset(cluster.data, cluster == 1)  # Subset the data for cluster 1
data_cluster2 <- subset(cluster.data, cluster == 2)  # Subset the data for cluster 1
data_cluster3 <- subset(cluster.data, cluster == 3)  # Subset the data for cluster 1
data_cluster4 <- subset(cluster.data, cluster == 4)  # Subset the data for cluster 1

# Fit the model
model1 <- plm(eq1, model = "within", index = c("Country", "Year"), data = data_cluster1)
summary(model1)

model2 <- plm(eq1, model = "within", index = c("Country", "Year"), data = data_cluster2)
summary(model2)

model3 <- plm(eq1, model = "within", index = c("Country", "Year"), data = data_cluster3)
summary(model3)

model4 <- plm(eq1, model = "within", index = c("Country", "Year"), data = data_cluster4)
summary(model4)


# Split data by cluster
cluster_list <- split(cluster.data, cluster.data$cluster)



######################################



# Define a function to fit the model and extract results
fit_model <- function(data, cluster_id) {
  model <- plm(eq1, model = "within", index = c("Country", "Year"), data = data)
  summary_model <- summary(model)
  
  # Extract coefficients and statistics
  coefficients <- summary_model$coefficients
  coefficients_df <- as.data.frame(coefficients)
  
  # Add cluster identifier
  coefficients_df$cluster <- cluster_id
  
  return(coefficients_df)
}

# Split data by cluster
cluster_list <- split(cluster.data, cluster.data$cluster)

# Apply the function to each cluster subset with cluster_id
results <- lapply(names(cluster_list), function(cluster_id) {
  data <- cluster_list[[cluster_id]]
  fit_model(data, cluster_id)
})

# Combine all coefficients into a single data frame
results_df <- do.call(rbind, results)

# View results
print(results_df)

############################################################
# Define a function to fit the model and extract results
fit_model <- function(data, cluster_id) {
  model <- plm(eq1, model = "within", index = c("Country", "Year"), data = data)
  summary_model <- summary(model)
  
  # Extract coefficients and statistics
  coefficients <- summary_model$coefficients
  coefficients_df <- as.data.frame(coefficients)
  
  # Extract adjusted R-squared
  adj_r_squared <- summary_model$r.squared["adj.r.squared"]
  
  # Calculate fitted values
  fitted_values <- fitted(model)
  fitted_values_df <- data.frame(Country = data$Country, Year = data$Year, Fitted = fitted_values)
  
  # Add cluster identifier to the coefficients data frame
  coefficients_df$cluster <- cluster_id
  
  # Combine coefficients and fitted values
  results_df <- list(
    coefficients = coefficients_df,
    fitted_values = fitted_values_df,
    adj_r_squared = adj_r_squared
  )
  
  return(results_df)
}

# Split data by cluster
cluster_list <- split(cluster.data, cluster.data$cluster)

# Apply the function to each cluster subset with cluster_id
results <- lapply(names(cluster_list), function(cluster_id) {
  data <- cluster_list[[cluster_id]]
  fit_model(data, cluster_id)
})

# Extract and combine coefficients and fitted values
coefficients_combined <- do.call(rbind, lapply(results, `[[`, "coefficients"))
fitted_values_combined <- do.call(rbind, lapply(results, `[[`, "fitted_values"))

# View results
print(coefficients_combined)
print(fitted_values_combined)

# Print adjusted R-squared for each cluster
adj_r_squared_list <- sapply(results, `[[`, "adj_r_squared")
print(adj_r_squared_list)


###############################

# Load necessary libraries
library(plm)
library(dplyr)

# Define the model formula
eq1 <- Prop_TFC_Trad_biomass ~ GDP_ppp_pcap + Prop_TFC_Other

# Function to fit model and extract results
fit_model <- function(data, cluster_id) {
  model <- plm(eq1, model = "within", index = c("Country", "Year"), data = data)
  summary_model <- summary(model)
  
  # Extract coefficients and statistics
  coefficients <- summary_model$coefficients
  coefficients_df <- as.data.frame(coefficients)
  
  # Extract adjusted R-squared
  adj_r_squared <- summary_model$r.squared["adj.r.squared"]
  
  # Calculate fitted values
  fitted_values <- fitted(model)
  fitted_values_df <- data.frame(Country = data$Country, Year = data$Year, Fitted = fitted_values)
  
  # Add cluster identifier to the coefficients data frame
  coefficients_df$cluster <- cluster_id
  
  return(list(
    coefficients = coefficients_df,
    fitted_values = fitted_values_df,
    adj_r_squared = adj_r_squared
  ))
}

# Get unique clusters
clusters <- unique(cluster.data$cluster)

# Apply the function to each cluster subset
results <- lapply(clusters, function(cluster_id) {
  data <- subset(cluster.data, cluster == cluster_id)
  fit_model(data, cluster_id)
})

# Extract and combine coefficients and fitted values
coefficients_combined <- do.call(rbind, lapply(results, `[[`, "coefficients"))
fitted_values_combined <- do.call(rbind, lapply(results, `[[`, "fitted_values"))

# Print adjusted R-squared for each cluster
adj_r_squared_list <- sapply(results, `[[`, "adj_r_squared")
print(adj_r_squared_list)

# View combined results
print(coefficients_combined)
print(fitted_values_combined)



  
#### SUR example
### Also account for regional variation - dummy for it - classify according to mixture approach
### Or some experience

# Create dummy variables for countries and years
all.data.wem.wider$A_WEM_Countries_Mapped <- factor(all.data.wem.wider$A_WEM_Countries_Mapped)
all.data.wem.wider$Year <- factor(all.data.wem.wider$Year)

# Define the model with fixed effects
# Add GDP and other variables
eq1 <- Growth_Trad_biomass ~ lag(Growth_Charcoal) + factor(A_WEM_Countries_Mapped) + factor(Year)
eq2 <- Growth_Charcoal ~ lag(Growth_Trad_biomass) + factor(A_WEM_Countries_Mapped) + factor(Year)

# List of equations
equations <- list(wood = eq1, charcoal = eq2)

# Estimate the system using SUR
fit <- systemfit(equations, method = "SUR", data = all.data.wem.wider)

##### Panel data:
##### Other Africa is over several ...

panel.biomass <- pdata.frame(all.data.wem.wider, index=c("A_WEM_Countries_Mapped","Year"), 
                             drop.index=TRUE, row.names=TRUE)

##### Test

# Tests of poolability

znp <- pvcm(Growth_Trad_biomass ~ lag(Growth_Charcoal), data = panel.biomass, model = "within")
zplm <- plm(Growth_Trad_biomass ~ lag(Growth_Charcoal), data = panel.biomass, model = "within")

# 1. Verify that lag(Growth_Charcoal) has sufficient within-panel variability. You can check the variance of this predictor within groups to confirm this:
panel.biomass %>%
  group_by(Country) %>%  # 
  summarise(variance = var(lag(Growth_Charcoal, default = NA)), .groups = "drop") %>%
  filter(is.na(variance) | variance == 0)


# 2. Ensure that the NA values created by lagging are handled appropriately. 
# If many values turn into NA, consider adjusting your data or using methods to deal with missing data:

# Check how many NAs are in your lagged variable
sum(is.na(panel.biomass$Growth_Charcoal_lagged))  # Assume Growth_Charcoal_lagged is your lagged variable

# Optionally, use na.omit to remove rows with NA values if appropriate
panel.biomass <- na.omit(panel.biomass)


# 3. Examine data structure
# Check first few rows to understand the structure
head(panel.biomass)

# Make sure the lagging is done correctly
panel.biomass$Growth_Charcoal_lagged <- with(panel.biomass, ave(Growth_Charcoal, id, FUN = function(x) lag(x)))  # replace 'id' with the appropriate group identifier


# 4. Modify model

# Maybe add more predictors if available
zplm <- plm(Growth_Trad_biomass ~ lag(Growth_Charcoal) + Other_Variable, data = panel.biomass, model = "within")


# Example: Download GDP data for a specific country
gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.CD", start = 2000, end = 2020)


# Example: Load Gapminder dataset
data("gapminder")


# Example: Load GDP data for countries
data <- getCountryData(mapResolution = "coarse", nameColumnToUse = "UNregion")

############### Additional colours

# Stacked and also line to see if tapering off. Also per capita

color.data <- readxl::read_excel(here::here("Steel", "Shell Scenarios v14.6 2023_06_23.xlsm"), sheet = "Settings",
                                 range = "H10:M270", col_names = TRUE) %>%
  select(-Colour)

color.data.hex <- color.data %>%
  mutate(Hex = paste0("#", Hex))

df_filtered <- color.data.hex %>% 
  filter(str_detect(tolower(Keyword1), paste(tolower(df_topn_names), collapse = "|")) |
           str_detect(tolower(Keyword2), paste(tolower(df_topn_names), collapse = "|")) |
           str_detect(tolower(Keyword3), paste(tolower(df_topn_names), collapse = "|")))

colors.dataframe <- data.frame(Country = c("China", "USA", "Japan", "Russia", "India", "Germany"),
                               Hex = c("#FF0000", "#003399", "#FF9900", "#990066", "#666666", "#FFD700"))

set.seed(1234)
color.remaining <- color.data.hex %>%
  filter(!Hex %in% colors.dataframe[,2]) %>%
  sample_n(n.countries + 1 -length(colors.dataframe$Hex)) %>% #21 for rest of world
  select(Hex)

country.rest <- df_all_ordered %>%
  ungroup() %>%
  filter(!Country %in% colors.dataframe$Country) %>%
  select(Country) %>%
  unique() %>%
  bind_cols(color.remaining) %>%
  bind_rows(colors.dataframe) 
# Partial match of country names to filter colours

df_all_colors <- df_all_ordered %>%
  merge(country.rest, by = c("Country")) %>%
  merge(df_topn_names_rank_select, by = c("Country")) %>%
  ungroup() %>%
  arrange(rank)

df_all_colors$Country <- factor(df_all_colors$Country, levels = unique(df_all_colors$Country)) # Set order according to rank
# Needed for right colours
# Partial match of country names to filter colours

df_all_world_recycled <- stock.data %>% # Calculate world per capita with recycling adjustment
  group_by(run_id, Sector, Year) %>%
  summarise(Population = sum(Population),
            stock = sum(stock)) %>% # To account for recycling
  mutate(stock.pcap = stock / (Population * 1000)) %>%
  ungroup() %>%
  mutate(Country = "World", Hex = "#000000", rank = 101) %>%
  select(Country, run_id, Year, stock, stock.pcap, Hex, rank)

df_all_colors <- rbind(df_all_colors, df_all_world_recycled) %>%
  filter(Country != "World")


plot1 <- plot_ly(df_all_colors %>%
                   filter(Country != "World"), x = ~Year, y = ~stock/1000000000, color = ~Country, type = "bar", colors = ~Hex) %>%
  layout(title = list(text = "Estimated Stock of Steel (bn tonnes) (ES Rebased to IEA 2019)", font = list(size = 16, family = "ShellBold")), 
         xaxis = list(title = "Year"), yaxis = list(title = "Stock"), barmode = "stack") %>%
  layout(
    yaxis = list(
      title = "Stock of Steel (bn tonnes)",
      titlefont = list(size = 14, family = "ShellMedium"),
      tickfont = list(size = 12, family = "ShellMedium")
    ),
    xaxis = list(
      title = "Year",
      titlefont = list(size = 14, family = "ShellMedium"),
      tickfont = list(size = 12, family = "ShellMedium")
    ),
    margin = list(l = 40, r = 40, t = 60, b = 40),
    legend = list(x = 1.05, y = 0.9, font = list(size = 14, family = "ShellMedium")),
    showlegend = FALSE,
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  ) %>% layout(
    showlegend = TRUE,
    legend = list(
      x = 1.05,
      y = 0.5,
      title = "Country",
      font = list(size = 12)
    )
  )


############################################

plot2 <- plot_ly(df_all_colors, x = ~Year, y = ~stock.pcap, color = ~Country, type = "scatter", colors = ~Hex, mode = "lines") %>%
  layout(title = list(text = "Estimated Steel per capita (ES Rebased to IEA 2019)", font = list(size = 16, family = "ShellBold")), 
         xaxis = list(title = "Year"), yaxis = list(title = "Stock of Steel (tonnes/capita)")) %>%
  layout(
    yaxis = list(
      title = "Stock of Steel (tonnes/ capita)",
      titlefont = list(size = 14, family = "ShellMedium"),
      tickfont = list(size = 12, family = "ShellMedium")
    ),
    xaxis = list(
      title = "Year",
      titlefont = list(size = 14, family = "ShellMedium"),
      tickfont = list(size = 12, family = "ShellMedium")
    ),
    margin = list(l = 40, r = 40, t = 60, b = 40),
    legend = list(x = 1.05, y = 0.9, font = list(size = 14, family = "ShellMedium")),
    showlegend = FALSE,
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  ) %>% layout(
    showlegend = FALSE,
    legend = list(
      x = 1.05,
      y = 0.5,
      title = "Country",
      font = list(size = 12)
    )
  )

library(htmltools)

#htmltools::save_html(tagList(plot1, plot2), file = here::here( "Steel", "Plots", "Stock_Steel.html"))

htmltools::save_html(subplot(plot1, plot2, nrows = 1 , shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>% layout(width = 1920, height = 1080), file = here::here( "Steel", "Plots", "Recycled_Stock_Steel_side.html"))


############################################################

fig <- subplot(plot1, plot2, nrows = 1 , shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>% 
  layout(width = 1920, height = 1080, title = list(text = "Stock of Steel", font = list(size = 20, family = "ShellBold")))

fig <- fig %>% add_annotations(
  text = c("Estimated Stock of Non-metallic minerals (bn tonnes) (ES Rebased to IEA 2019)", "Estimated NMM per capita (ES Rebased to IEA 2019)"),
  xref = "paper", yref = "paper",
  x = c(0.15, 0.85), y = 1.05,
  showarrow = FALSE, font = list(size = 16, family = "ShellBold")
)

fig <- fig %>% layout(
  subplot_titles = c("Estimated Stock of NMM (bn tonnes) (ES Rebased to IEA 2019)", "Estimated NMM per capita (ES Rebased to IEA 2019)"),
  margin = list(l = 40, r = 40, t = 100, b = 40)
)

# htmltools::save_html(fig, file = here::here( "Steel", "Plots", "Recycled_Stock_Steel_side.html"))

# Save the interactive HTML file
htmlwidgets::saveWidget(
  widget = fig,
  file = here::here( "Steel", "Plots", "Recycled_Stock_NMM_side.html"),
  selfcontained = TRUE
)

