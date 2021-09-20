# CMAP | Noel Peterson, Mary Weber | 3/16/2021

#This file fetches and formats 1990, 2000 and 2010 Decennial Census data
#This file contains 1995, 2005 and 2011 - 2019 Population Estimates Program (PEP) data

library(tidyverse)
library(tidycensus)
library(readxl)

# Set parameters ----------------------------------------------------------

POP_YEARS <- c(2000, 2010)  # 1990 not available via API
PEP_YEARS <- c(`4`=2011, `5`=2012, `6`=2013, `7`=2014, `8`=2015, `9`=2016, `10`=2017, `11`=2018, `12`=2019) # add in 2020 estimates
YEARS <- c(2000, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
## PEP DATE_CODE documentation: https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html

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

#extra age groupings to remove (PEP includes some overlapping categories)
PEP_remove <- c("Under 18 years", "16 years and over", "18 years and over", "65 years and over",
            "85 years and older", "5 to 13 years", "14 to 17 years", "15 to 44 years",
            "18 to 24 years", "18 to 64 years", "25 to 44 years", "45 to 64 years",
            "Median age", "All ages")


# Extract Decennial Census variables from SF1 and join to decennial pop data ---------------------------------

POP_DATA <- tibble()
POP <- list()
for (YEAR in POP_YEARS) {

  # Compile list of variables to download
  SF1_VARS <- load_variables(YEAR, "sf1")
  POP_VARS <- SF1_VARS %>%
    filter(str_starts(name, paste0("^", POP_TABLES[[as.character(YEAR)]]))) %>%
    mutate(
      Category = str_replace_all(label, "!!.*?", " "),
      Category = str_replace(Category, ".*? ", "")
    )

  for (STATE in names(COUNTIES)) {
    TEMP <- get_decennial(geography = "county", variables = POP_VARS$name,
                          county = COUNTIES[[STATE]], state = STATE,
                          year = YEAR, survey = "sf1", cache_table = TRUE)
    POP_DATA <- bind_rows(POP_DATA, TEMP)
  }

  POP[[as.character(YEAR)]] <- POP_DATA %>%
    left_join(POP_VARS, by = c("variable" = "name")) %>%
    select(-variable, -label, -concept) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    separate(Category, c("Sex", "Age"), sep = " ", extra = "merge") %>%
    rename(Population = value) %>%
    mutate(
      Year = YEAR,
      Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                         State == "Illinois" ~ "External IL",
                         State == "Indiana" ~ "External IN",
                         State == "Wisconsin" ~ "External WI"),
      Age = case_when(Age %in% c("Under 5 years") ~ "0 to 4 years",
                      Age %in% c("15 to 17 years", "18 and 19 years") ~ "15 to 19 years",
                      Age %in% c("20 years", "21 years", "22 to 24 years") ~ "20 to 24 years",
                      Age %in% c("60 and 61 years", "62 to 64 years") ~ "60 to 64 years",
                      Age %in% c("65 and 66 years", "67 to 69 years") ~ "65 to 69 years",
                      TRUE ~ Age)
    ) %>%
    group_by(GEOID, County, State, Sex, Age, Year, Region) %>%
    summarize(Population = sum(Population)) %>%
    drop_na() %>%
    ungroup()
}


# Pull PEP data, save in POP list ------------------------
PEP_DATA <- tibble()

for (STATE in names(COUNTIES)) {
    PEP_TEMP <- get_estimates(product="characteristics", geography = "county", year = max(PEP_YEARS),
                              county = COUNTIES[[STATE]], state = STATE, breakdown = c("SEX", "AGEGROUP"),
                              breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE)%>%
      filter(DATE %in% names(PEP_YEARS), SEX %in% c("Male", "Female")) %>%
      rename(Population = value, Age = AGEGROUP, Sex = SEX) %>%
      separate(NAME, c("County", "State"), sep = "\\, ") %>%
      mutate(Year = PEP_YEARS[as.character(DATE)],
             Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                                State == "Illinois" ~ "External IL",
                                State == "Indiana" ~ "External IN",
                                State == "Wisconsin" ~ "External WI"),
             Age = str_replace_all(Age, "Age ", "")) %>%
      select(-DATE)

    PEP_DATA <- bind_rows(PEP_DATA, PEP_TEMP)
}

for(YEAR in PEP_YEARS) {

  POP[[as.character(YEAR)]] <- PEP_DATA %>%
    filter(Year == YEAR, !(Age %in% PEP_remove)) %>% # Restrict to specific year and age groupings
    arrange(GEOID)
}

# Import Excel files for select years and save final POP data file -------------------------------

POP[["1995"]] <- read_excel("Input/Pop1995.xlsx")
POP[["2005"]] <- read_excel("Input/Pop2005.xlsx")
POP[["2020"]] <- read_excel("Input/censusadjustedPEP2020.xlsx")

POP <- POP[as.character(sort(as.numeric(names(POP))))]


#save(POP, file="Output/POP_PEP.Rdata")

#write.csv(POP, "/Users/mweber/Desktop/POP_New.csv")

