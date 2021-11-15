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
HH_Race <- c('H007003', 'H007004', 'H007005', 'H007006', 'H007007', 'H007008', 'H007009',' H007010') #householder by race

# Extract Decennial Census race/ethnicity variables from SF1 ---------------------------------

Decennial_DATA <- tibble()
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
      mutate(variable = case_when(variable == 'H007003' ~ 'NH_White', #White alone
                                  variable == 'H007004' ~ 'NH_Black', #Black or African American alone
                                  variable == 'H007005' ~ 'NH_AI', #American Indian or Alaskan Native
                                  variable == 'H007006' ~ 'NH_Asian', #Asian alone
                                  variable == 'H007007' ~ 'NH_PI', #Native Hawaiian and Other Pacific Islander
                                  variable == 'H007008' ~ 'NH_Other', #some other race alone
                                  variable == 'H007009' ~ 'NH_Other', #two or more races
                                  variable == 'H007010' ~ 'Hispanic')) #Hispanic or Latino

    Decennial_DATA <- bind_rows(Decennial_DATA, TEMP)
  }

  Decennial_DATA  <- Decennial_DATA %>% group_by(variable, County, State, GEOID) %>%
    summarise(Population = sum(Population)) #Berger lumped together all the 'other' groups into one

  Decennial_DATA$Year = '2010'

  Decennial_DATA <- Decennial_DATA %>%
    mutate(Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                              State == "Illinois" ~ "External IL",
                              State == "Indiana" ~ "External IN",
                              TRUE ~ "External WI"))
}

# Extract PEP race/ethnicity data ---------------------------------------------
  # ACS 1-year estimates, 2014-2019 (only for counties 65k and over...)

PEP_DATA <- tibble()

years_test <- c(2011:2019)

hh_race_variables = c("B25006_001" , "B25006_002", "B25006_003", "B25006_004", "B25006_005", "B25006_006", "B25006_007",
                      "B25006_008", "B25006_009", "B25006_010")

for (STATE in names(COUNTIES)) {
  for (YEAR in years_test){
    PEP_TEMP <- get_acs(geography = "county", variables = hh_race_variables, county = COUNTIES[[STATE]],
                      state = STATE, survey = "acs1", year = YEAR) %>%
      mutate(Year = YEAR) %>%
      separate(NAME, c("County", "State"), sep = "\\, ") %>%
      mutate(Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                         State == "Illinois" ~ "External IL",
                         State == "Indiana" ~ "External IN",
                         State == "Wisconsin" ~ "External WI"))

    PEP_DATA <- bind_rows(PEP_DATA, PEP_TEMP)
  }
}




###### scratchpad code below

test <- get_acs(geography = "county", variables = "B25006_001", county = "Cook",
                state = "IL", survey = "acs1", year = 2011)


popTot <- Decennial_DATA %>% group_by(Year, Region) %>% summarize(sumHH = sum(Population))
