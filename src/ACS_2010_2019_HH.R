# Dec 22, 2021
# Haoyu Shi
# Retrieving population and household data from ACS-2010 to ACS-2019 5-year estimate

library(tidycensus)
library(tidyverse)

# Geography
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

# Years
Years <- c(2010:2019)

######################### Variables ID ###############################
POP_ID <- c("B02001_001", # Population Total
            "B02001_002", # White
            "B02001_003", # Black
            "B02001_005", # Asian
            "B03002_003", # NH_White
            "B03002_012") # Hispanic


HH_ID <- c("B25006_001", # Householder Total
           "B25006_002", # White
           "B25006_003", # Black
           "B25006_005", # Asian
           "B22005H_001", # NH_White
           "B22005I_001") # Hispanic

################ Retrieving data using tidycensus ##################
ACS_POP <- tibble()
ACS_HH <- tibble()

for (YEAR in Years){
  for (STATE in names(COUNTIES)){
    TEMP <- get_acs(geography = "county", variables = POP_ID,
                          county = COUNTIES[[STATE]], state = STATE,
                          year = YEAR, survey = "acs5", cache_table = TRUE) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(variable = case_when(variable == "B02001_001" ~ "All",
                                variable == "B02001_002" ~ "White",
                                variable == "B02001_003" ~ "Black",
                                variable == "B02001_005" ~ "Asian",
                                variable == "B03002_003" ~ "NH_White",
                                variable == "B03002_012" ~ "Hispanic"))
    TEMP$Year = YEAR
    ACS_POP <- bind_rows(ACS_POP, TEMP)
  }
}
