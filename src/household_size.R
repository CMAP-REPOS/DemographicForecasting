# CMAP | Alexis McAdams, Mary Weber | 10/29/2021

library(tidyverse)
library(tidycensus)
library(readxl)

# Set parameters ----------------------------------------------------------

#CENSUS RACE DEFINITIONS: https://www.census.gov/programs-surveys/cps/data/data-tools/cps-table-creator-help/race-definitions.html

Decennial_YEARS <- c(2010) #ideally add 2020 when that data is available
Other_YEARS <- c(`4`=2011, `5`=2012, `6`=2013, `7`=2014, `8`=2015, `9`=2016, `10`=2017, `11`=2018, `12`=2019) # add in 2020 estimates
YEARS <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")
HH_Race <- c('H013002', 'H013003', 'H013004', 'H013005', 'H013006', 'H013007', 'H013008') #household size

# Extract Decennial Census race/ethnicity variables from SF1 ---------------------------------

HH_Size <- tibble()
POP <- list()
for (YEAR in Decennial_YEARS) {
  
  # Compile list of variables to download
  SF1_VARS <- load_variables(YEAR, "sf1")
  POP_VARS <- SF1_VARS %>% filter(name %in% HH_Race) %>%
    mutate(
      Category = str_replace_all(label, "!!.*?", " "),
      Category = str_replace(Category, ".*? ", "")
    )
  
  for (STATE in names(COUNTIES)) {
    TEMP <- get_decennial(geography = "county", variables = POP_VARS$name,
                          county = COUNTIES[[STATE]], state = STATE,
                          year = YEAR, survey = "sf1", cache_table = TRUE)%>%
      separate(NAME, c("County", "State"), sep = "\\, ") %>%
      rename(Population = value) %>%
      mutate(variable = case_when(variable == 'H013002' ~ '1 Person HH', #1
                                  variable == 'H013003' ~ '2 Person HH', #2
                                  variable == 'H013004' ~ '3 Person HH', #3
                                  variable == 'H013005' ~ '4 Person HH', #4
                                  variable == 'H013006' ~ '5 Person HH', #5
                                  variable == 'H013007' ~ '6 Person HH', #6
                                  variable == 'H013008' ~ '7+ Person HH')) #7+
    
    HH_Size <- bind_rows(HH_Size, TEMP)
  }
  
  HH_Size  <- HH_Size %>% group_by(variable, County, State, GEOID) %>%
    summarise(Num_HH = sum(Population)) #Berger lumped together all the 'other' groups into one
  
  HH_Size$Year = '2010'
  
  HH_Size <- HH_Size %>%
    mutate(Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                              State == "Illinois" ~ "External IL",
                              State == "Indiana" ~ "External IN",
                              TRUE ~ "External WI"))
}
