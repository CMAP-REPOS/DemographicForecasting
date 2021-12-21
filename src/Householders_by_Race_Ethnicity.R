# 12/16/2021 Haoyu Shi
# Household Head Rate by Race and Ethnicity

library(tidyverse)
library(tidycensus)

# I didn't use PEP data since there is no race/ethnicity information for housing units

# Years
Decennial_YEARS <- c(2010) #ideally add 2020 when that data is available
Other_YEARS <- c(`4`=2011, `5`=2012, `6`=2013, `7`=2014, `8`=2015, `9`=2016, `10`=2017, `11`=2018, `12`=2019, `13` = 2020) # add in 2020 estimates
YEARS <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

# Geography
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")


##### Extract Decennial Data
# Variables
Decennial_ID <- c('P005003', #NH_White
                      'P005004', #NH_Black
                      'P005005', #NH_AmIn
                      'P005006', #NH_Asian
                      'P005007', #NH_PI              These are population
                      'P005008', #NH_Other
                      'P005009', #NH_More
                      'P005010', #Hisp

                      'H007003', #NH_White
                     'H007004', #NH_Black
                     'H007005', #NH_AmIn
                     'H007006', #NH_Asian
                     'H007007', #NH_PI               These are householders
                     'H007008', #NH_Other
                     'H007009', #NH_More
                     'H007010') #Hisp


Decennial_Data <- tibble()

for (YEAR in Decennial_YEARS) {
  for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = Decennial_ID,
                                county = COUNTIES[[STATE]], state = STATE,
                                year = YEAR, survey = "sf1", cache_table = TRUE)

   Decennial_Data <- bind_rows(Decennial_Data, TEMP)
  }
}

Decennial_Data <- Decennial_Data %>% spread(key = variable,
                                            value = value)







Decennial_Data$person_per_HH <- Decennial_Data$Population/Decennial_Data$Householder



%>%
  mutate(variable = case_when(variable == 'H007003' ~ 'NH_White',
                              variable == 'H007004' ~ 'NH_Black',
                              variable == 'H007005' ~ 'NH_Other',
                              variable == 'H007006' ~ 'NH_Asian',
                              variable == 'H007007' ~ 'NH_Other',
                              variable == 'H007008' ~ 'NH_Other',
                              variable == 'H007009' ~ 'NH_Other',
                              variable == 'H007010' ~ 'Hispanic'))

%>%
  separate(NAME, c("County", "State"), sep = "\\, ") %>%
  rename(Householder = value)




%>%
  mutate(variable = case_when(variable == 'P005003' ~ 'NH_White',
                              variable == 'P005004' ~ 'NH_Black',
                              variable == 'P005005' ~ 'NH_Other',
                              variable == 'P005006' ~ 'NH_Asian',
                              variable == 'P005007' ~ 'NH_Other',
                              variable == 'P005008' ~ 'NH_Other',
                              variable == 'P005009' ~ 'NH_Other',
                              variable == 'P005010' ~ 'Hispanic'))

%>%
  separate(NAME, c("County", "State"), sep = "\\, ") %>%
  rename(Population = value)

##### Extract ACS Data (need to find more data for the ethnicity of householder. ACS only has Race information for householders.
# only has NH-White householders and Hispanic/Latino householders )
# Check the variables for ACS 5-year estimates
ACS_VARS <- load_variables(year = 2019, dataset = "acs5")
# get unique concepts
unique_ACS_Concepts <- ACS_VARS$concept
unique_ACS_Concepts <- as.data.frame(unique(unique_ACS_Concepts))

ACS_POP_ID <- c("B03002_012", #total Hispanic/Latino
                  "B03002_002", #total NH
                  "B03002_003", #NH_White
                  "B03002_004", #NH_Black
                  "B03002_006") #NH_Asian

ACS_HH_ID <- c("B22005I_001", #HISP
               "B22005H_001", #NH_White


)


ACS_HH <- tibble()
ACS_POP <- tibble()

for (YEAR in Other_YEARS) {
  for (STATE in names(COUNTIES)) {
    HH_TEMP <- get_acs(geography = "county", variables = HH_Race,
                             county = COUNTIES[[STATE]], state = STATE,
                             year = YEAR, survey = "sf1", cache_table = TRUE)%>%
      separate(NAME, c("County", "State","Year"), sep = "\\, ") %>%
      rename(Householder = value) %>%
      mutate(variable = case_when(variable == 'H007003' ~ 'NH_White',
                                  variable == 'H007004' ~ 'NH_Black',
                                  variable == 'H007005' ~ 'NH_Other',
                                  variable == 'H007006' ~ 'NH_Asian',
                                  variable == 'H007007' ~ 'NH_Other',
                                  variable == 'H007008' ~ 'NH_Other',
                                  variable == 'H007009' ~ 'NH_Other',
                                  variable == 'H007010' ~ 'Hispanic'))


    POP_TEMP <- get_acs(geography = "county", variables = POP_Race,
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

    Decennial_HH <- bind_rows(Decennial_HH, HH_TEMP)
    Decennial_POP <- bind_rows(Decennial_POP, POP_TEMP)
  }
}
