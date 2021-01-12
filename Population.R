# CMAP | Mary Weber | 12/8/2020

install.packages(c("tidyverse", "tidycensus", "readxl"))
library(tidyverse)
library(tidycensus)
library(readxl)
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
    select(-label) %>%
    rename(Concept = concept,
           Value = value,
           Variable = variable) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(
      Year = YEAR,
      Category = case_when(str_starts(Category, "^Total") ~ "County Total",
                           Category == "Male" ~ "County Male Total",
                           Category == "Female" ~ "County Female Total",
                           TRUE ~ Category),
      Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                         State == "Illinois" ~ "External IL",
                         State == "Indiana" ~ "External IN",
                         State == "Wisconsin" ~ "External WI")
    )
}

# 2005 and 2015 ACS Data --------------------------------

#set parameters ------------------------------------
YEARS2 <- c(2005, 2015)  # 1995 not available via API

POP_TABLES2 <- c(
  `2005` = "B01001_",
  `2015` = "B01001_"
)

# Compile population data for 2005 and 2015 --------------------------------

POP2 <- list()
for (YEAR in YEARS2) {
  
  # Compile list of variables to download
  ACS_VARS <- load_variables(YEAR, "acs1")
  POP_VARS2 <- ACS_VARS %>%
    filter(str_starts(name, paste0("^", POP_TABLES2[[as.character(YEAR)]]))) %>%
    mutate(
      Category = str_replace_all(label, "!!.*?", " "),
      Category = str_replace(Category, ".*? ", "")
    )

  # Download data for selected variables in all counties
  POP_DATA2 <- tibble()
  for (STATE in names(COUNTIES)) {
     TEMP2 <- get_acs(geography = "county", variables = POP_VARS2$name,
                          county = COUNTIES[[STATE]], state = STATE,
                          year = YEAR, survey = "acs1", cache_table = TRUE)
      POP_DATA2 <- bind_rows(POP_DATA2, TEMP2)
  }
 
  # Assemble final table
  POP2[[as.character(YEAR)]] <- POP_DATA2 %>%
    left_join(POP_VARS2, by = c("variable" = "name")) %>%
    select(-label) %>%
    rename(Concept = concept,
           Value = moe,
           Variable = variable) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(
      Year = YEAR,
      Category = case_when(str_starts(Category, "^Total") ~ "County Total",
                           Category == "Male" ~ "County Male Total",
                           Category == "Female" ~ "County Female Total",
                           TRUE ~ Category),
      Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                         State == "Illinois" ~ "External IL",
                         State == "Indiana" ~ "External IN",
                         State == "Wisconsin" ~ "External WI")
    )
}


#View(POP2[["2005"]])
#View(POP2[["2015"]])


# Read 1990 and 1995 data from spreadsheets
POP[["1990"]] <- read_excel("Pop1990.xlsx")  # Should be adjusted to match 2000/2010 format
POP[["1995"]] <- read_excel("Pop1995.xlsx")  # Should be adjusted to match 2000/2010 format

#save(POP, file="PopData.Rdata")
#load("PopData.Rdata")
