# 12/16/2021
# Household Head Rate by Race and Ethnicity

library(tidyverse)

# Years
Decennial_YEARS <- c(2010) #ideally add 2020 when that data is available
Other_YEARS <- c(`4`=2011, `5`=2012, `6`=2013, `7`=2014, `8`=2015, `9`=2016, `10`=2017, `11`=2018, `12`=2019) # add in 2020 estimates
YEARS <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

# Geography
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

# Variables
IDs <- c('P005003', 'P005004', 'P005005', 'P005006', 'P005007','P005008', 'P005009','P005010') # population by race/ethnicity
HH_Race <- c('H007003', 'H007004', 'H007005', 'H007006', 'H007007', 'H007008', 'H007009','H007010') # householder by race/ethnicity

#####
Decennial_DATA <- tibble()
# POP <- list()

for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = HH_Race,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = 2010, survey = "sf1", cache_table = TRUE)%>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    rename(Householder = value) %>%
    mutate(variable = case_when(variable == 'H007003' ~ 'NH_White',
                                variable == 'H007004' ~ 'NH_Black',
                                variable == 'H007005' ~ 'NH_Other',
                                variable == 'H007006' ~ 'NH_Asian',
                                variable == 'H007007' ~ 'NH_Other',
                                variable == 'H007008' ~ 'NH_Other',
                                variable == 'H007009' ~ 'NH_Other',
                                variable == 'H007010' ~ 'Hispanic'))
}





for (YEAR in Decennial_YEARS) {

  # Compile list of variables to download
  SF1_VARS <- load_variables(YEAR, "sf1")
  POP_VARS <- SF1_VARS %>% filter(name %in% IDs) %>%
    mutate(
      Category = str_replace_all(label, "!!.*?", " "),
      Category = str_replace(Category, ".*? ", "")
    )

  for (STATE in names(COUNTIES)) {
    TEMP <- get_decennial(geography = "county", variables = IDs,
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
