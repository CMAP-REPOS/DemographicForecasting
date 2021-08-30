# CMAP | Noel Peterson, Mary Weber | 3/16/2021

#This file contains 1995, 2005 and 2011 - 2019 Population Estimates Program (PEP) data

#install.packages(c("tidyverse", "tidycensus", "readxl"))
library(tidyverse)
library(tidycensus)
library(readxl)
#load("Output/PopData.Rdata") #must load, following code dependent on POP[[]]

# Set parameters ----------------------------------------------------------

PEP_YEARS <- c(`4`=2011, `5`=2012, `6`=2013, `7`=2014, `8`=2015, `9`=2016, `10`=2017, `11`=2018, `12`=2019, `13`=2019) # add in 2020 estimates
## PEP DATE_CODE documentation: https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

#extra age groupings to remove (PEP includes some overlapping categories)
remove <- c("Under 18 years", "16 years and over", "18 years and over", "65 years and over",
            "85 years and older", "5 to 13 years", "14 to 17 years", "15 to 44 years",
            "18 to 24 years", "18 to 64 years", "25 to 44 years", "45 to 64 years",
            "Median age", "All ages")

PEP_DATA <- tibble()

for (STATE in names(COUNTIES)) {

  PEP_TEMP <- get_estimates(product="characteristics", geography = "county", year = max(PEP_YEARS),
                            county = COUNTIES[[STATE]], state = STATE, breakdown = c("SEX", "AGEGROUP"),
                            breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE) %>%
    filter(DATE %in% names(PEP_YEARS),
           SEX %in% c("Male", "Female")) %>%
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

# Create final table --------------------------------

for(YEAR in PEP_YEARS) {

    POP[[as.character(YEAR)]] <- PEP_DATA %>%
      filter(Year == YEAR, !(Age %in% remove)) %>% # Restrict to specific year and age groupings
      arrange(GEOID)
}

# Upload Excel files and save final POP data file -------------------------------

POP[["1995"]] <- read_excel("Input/Pop1995.xlsx")
POP[["2005"]] <- read_excel("Input/Pop2005.xlsx")
POP[["2020"]] <- read_excel("Input/PEP2020.xlsx")
#POP[["2020"]] <- read_excel("Input/censusadjustedPEP2020.xlsx")

POP <- POP[as.character(sort(as.numeric(names(POP))))]

save(POP, file="Output/PopData.Rdata")
#load("Output/PopData.Rdata")

