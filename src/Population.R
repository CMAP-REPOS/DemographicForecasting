# CMAP | Noel Peterson, Mary Weber | 1/15/2021

#This file contains 1990, 2000 and 2010 Decennial Census data

#install.packages(c("tidyverse", "tidycensus", "readxl"))
library(tidyverse)
library(tidycensus)
library(readxl)
load("Output/PopData.Rdata")
#census_api_key("d94fbe16b1b053593223397765874bf147d1ae72", install = TRUE)


# Set parameters ----------------------------------------------------------

YEARS <- c(2000, 2010)  # 1990 not available via API
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")
POP_TABLES <- c(
  `2000` = "PCT0130",
  `2010` = "P0120"
)


# Compile population data from each Census --------------------------------

POP <- list()
for (YEAR in YEARS) {
  
  # Compile list of variables to download
  SF1_VARS <- load_variables(YEAR, "sf1")
  POP_VARS <- SF1_VARS %>%
    filter(str_starts(name, paste0("^", POP_TABLES[[as.character(YEAR)]]))) %>%
    mutate(
      Category = str_replace_all(label, "!!.*?", " "),
      Category = str_replace(Category, ".*? ", "")
    )
  
  # Download data for selected variables in all counties
  POP_DATA <- tibble()
  for (STATE in names(COUNTIES)) {
    TEMP <- get_decennial(geography = "county", variables = POP_VARS$name,
                          county = COUNTIES[[STATE]], state = STATE,
                          year = YEAR, survey = "sf1", cache_table = TRUE)
    POP_DATA <- bind_rows(POP_DATA, TEMP)
  }
  
  # Assemble final table
  POP[[as.character(YEAR)]] <- POP_DATA %>%
    left_join(POP_VARS, by = c("variable" = "name")) %>%
    select(-label, -concept, -variable) %>%
    rename(Population = value,
          Age = Category) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(
      Year = YEAR,
      Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                         State == "Illinois" ~ "External IL",
                         State == "Indiana" ~ "External IN",
                         State == "Wisconsin" ~ "External WI"),
      Sex = str_extract(Age, "^[^\\s]+"),
      Age = str_extract(Age, "\\s.+"),
      Age = case_when(Age %in% c("15 to 17 years", "18 and 19 years") ~ "15 to 19 years",
                  Age %in% c("20 years", "21 years", "22 to 24 years") ~ "20 to 24 years",
                  TRUE ~ Age)) %>%
      drop_na()
}


View(POP[["2000"]])

# Read 1990 data from spreadsheets
#POP[["1990"]] <- read_excel("Input/Pop1990.xlsx")
 

#save(POP, file="Output/PopData.Rdata")
#load("Output/PopData.Rdata")
