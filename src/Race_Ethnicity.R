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
IDs <- c('P005003', 'P005004', 'P005005', 'P005006', 'P005007','P005008', 'P005009','P005010') #race/ethnicity

HH_Race <- c('H007003', 'H007004', 'H007005', 'H007006', 'H007007', 'H007008', 'H007009',' H007010') #householder by race

#P002


# Extract Decennial Census race/ethnicity variables from SF1 ---------------------------------

Decennial_DATA <- tibble()
POP <- list()
for (YEAR in Decennial_YEARS) {

  # Compile list of variables to download
  SF1_VARS <- load_variables(2010, "sf1")
  POP_VARS <- SF1_VARS %>% filter(name %in% IDs) %>%
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
            mutate(variable = case_when(variable == 'P005003' ~ 'NH_White',
                             variable == 'P005004' ~ 'NH_Black',
                             variable == 'P005005' ~ 'NH_Other',
                             variable == 'P005006' ~ 'NH_Asian',
                             variable == 'P005007' ~ 'NH_Other',
                             variable == 'P005008' ~ 'NH_Other',
                             variable == 'P005009' ~ 'NH_Other',
                             variable == 'P005010' ~ 'Hispanic'))

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

DecennialSummary <- Decennial_DATA %>% group_by(Region, Year, variable) %>% summarize(Population = sum(Population)) %>%
  mutate(RACE = case_when(str_detect(variable, "Asian") ~ "Asian alone",
                          str_detect(variable, "Black") ~ "Black alone",
                          str_detect(variable, "White") ~ "White alone",
                          str_detect(variable, "Other") ~ "NH_Other",
                          TRUE ~ "All races" ) ) %>%
  mutate(HISP = case_when(str_detect(variable, "Hispanic") ~ "Hispanic",
                          TRUE ~ "Non-Hispanic")) %>%
  select(-variable) %>% pivot_wider(names_from = Year, values_from = Population)

# Extract PEP race/ethnicity data ---------------------------------------------

PEP_DATA <- tibble()

for (STATE in names(COUNTIES)) {
  PEP_TEMP <- get_estimates(product="characteristics", geography = "county", year = max(Other_YEARS),
                            county = COUNTIES[[STATE]], state = STATE, breakdown = c("SEX", 'AGEGROUP', "RACE", "HISP"),
                            breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE) %>%
    filter(DATE %in% names(Other_YEARS)) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(Year = Other_YEARS[as.character(DATE)],
           Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                              State == "Illinois" ~ "External IL",
                              State == "Indiana" ~ "External IN",
                              State == "Wisconsin" ~ "External WI")) %>%
          select(-DATE)  #%>%


  PEP_DATA <- bind_rows(PEP_DATA, PEP_TEMP)
}

# Filter for NH-White, NH-Black, and NH-Asian
Non_HISP <- PEP_DATA %>% filter((RACE == 'White alone' & HISP == 'Non-Hispanic') | (RACE == 'Black alone' & HISP == 'Non-Hispanic') | (RACE == 'Asian alone' & HISP == 'Non-Hispanic')) %>%
                         filter(AGEGROUP != 'All ages' & SEX != 'Both sexes')

# Filter for all other NH groups (American Indian and Alaska Native alone, Native Hawaiian and Other Pacific Islander alone)
temp <-  PEP_DATA %>% filter(RACE %in% c('American Indian and Alaska Native alone', 'Native Hawaiian and Other Pacific Islander alone', 'Two or more races') & HISP == 'Non-Hispanic') %>%
                      filter(AGEGROUP != 'All ages' & SEX != 'Both sexes') %>%
                      mutate(RACE = case_when(RACE %in% c('American Indian and Alaska Native alone', 'Native Hawaiian and Other Pacific Islander alone', 'Two or more races') ~ "NH_Other")) %>%
                      group_by(GEOID, County, State, SEX, AGEGROUP, RACE, HISP, Year, Region) %>%
                      summarise(value = sum(value))

# Filter for Hispanic only
HISP <- PEP_DATA %>% filter(HISP == 'Hispanic' & AGEGROUP != 'All ages' & SEX != 'Both sexes' & RACE == 'All races')

# Create final table
RE <- full_join(Non_HISP, HISP)
RE <- full_join(RE, temp)

# Reformat and combine the two tables -----------------------------

summary <- RE %>% group_by(Region, Year, RACE, HISP) %>% summarize(totpop = sum(value)) %>%
  pivot_wider(names_from = Year, values_from = totpop) %>%
  right_join(DecennialSummary, by=c("Region", "RACE", "HISP")) %>%
  relocate(`2010`, .after = HISP)

# Export
#write.csv(summary, file = "C:/Users/amcadams/Documents/R/race_eth_2010-19.csv")

write.csv(summary, file = "/Users/mweber/Desktop/race_eth_2010-19.csv")
