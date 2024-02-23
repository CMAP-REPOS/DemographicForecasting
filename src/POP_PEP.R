
# Overview ----------------------------------------------------------------

# CMAP | Noel Peterson, Mary Weber, Alexis McAdams, Alex Bahls | 7/11/2023

#AB -- get estimates isn't updated for 2023 -- hoping to wait for consistency but may need to manually download and incorp

# This script fetches and formats 2000, 2010, and 2020 Decennial Census data AND
# 2021, and 2022 Population Estimates Program (PEP) data.
# Population data is pulled at the county level by age (5-year groupings) and sex.
# Data for 1995, 2005 and 2020 is imported MANUALLY from prepared excel csvs (see end of script).
# Data is combined into a single list object called POP.

# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(readxl)

# Set parameters ----------------------------------------------------------

#COUNTY and GEOid list should be loaded from (full_run.R)

### Years for which to pull data
#Census Population
POP_YEARS <- c(2000, 2010, 2020)  # 1990 not available via API

#Census Population Estimates Program (PEP) -- list of years to pull
PEP_YEARS <- c(`2023`= 2023, `2022` = 2022, `2021` = 2021,
               `4`=2011, `5`=2012, `6`=2013, `7`=2014, `8`=2015, `9`=2016, `10`=2017, `11`=2018, `12`=2019) #2020 is based on 2010 vintage and doesnt use census

#note that including 2023 doesnt result in an error but also the data isn't downloaded later on (yet)

#  note: `#` names in PEP_YEARS are necessary to filter correct years of data from PEP pull (see tidycensus documentation, "time_series" argument)
# PEP DATE_CODE documentation --> "the key for YEAR" pg. 3: https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html

# Table names for Decennial Census data pull
POP_TABLES <- c(
  `2000` = "PCT0130",
  `2010` = "P0120",
  `2020` = "P12_"
)

#extra age groupings to remove (PEP includes some overlapping categories)
PEP_remove <- c(# Children
                "Under 18 years", "5 to 13 years", "14 to 17 years",
                # Adults
                "16 years and over", "18 years and over",
                "18 to 24 years", "18 to 64 years",
                "15 to 44 years", "25 to 44 years", "45 to 64 years",
                # Older
                "65 years and over", "85 years and over",
                # Median all ages
                "Median age", "All ages")

# Extract Decennial Census variables from SF1 and join to decennial pop data ---------------------------------

POP_DATA <- tibble()
POP <- list()
for (YEAR in POP_YEARS) {

  # For 2010 and earlier Decennial years, use Summary File 1 to get Age/Sex data
  if (YEAR <= 2010) {

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
    POP_DATA <- bind_rows(POP_DATA, TEMP) |> distinct()
  }

  POP[[as.character(YEAR)]] <- POP_DATA %>%
    left_join(POP_VARS, by = c("variable" = "name")) %>%
    select(-variable, -label, -concept) %>%
    filter(!is.na(Category) & !(Category  %in% c("Total","Male","Female"))) |> #not necessary but stops warnings later
    separate_wider_delim(NAME, delim = ",", names = c("County", "State")) |> #updated from depreciated command
    separate_wider_delim(Category, delim = " ", names = c("Sex","Age"), too_many = "merge") |>
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

  # For 2020 and later Decennial years, use Demographic and Housing Characteristics (DHC) file to get Age/Sex data
  else {
    DHC_VARS <- load_variables(YEAR, "dhc")
    POP_VARS <- DHC_VARS %>%
      filter(str_starts(name, paste0("^", POP_TABLES[[as.character(YEAR)]]))) %>%
      mutate(
        Category = str_replace_all(label, "!!.*?", " "),
        Category = str_replace(Category, ".*? ", "")
      )

    for (STATE in names(COUNTIES)) {
      TEMP <- get_decennial(geography = "county", variables = POP_VARS$name,
                            county = COUNTIES[[STATE]], state = STATE,
                            year = YEAR, sumfile = "dhc", cache_table = TRUE)
      POP_DATA <- bind_rows(POP_DATA, TEMP)
    }

    POP[[as.character(YEAR)]] <- POP_DATA %>%
      left_join(POP_VARS, by = c("variable" = "name")) %>%
      select(-variable, -label, -concept) %>%
      separate_wider_delim(NAME, delim = ", ",names = c("County", "State")) %>%
      separate_wider_delim(Category, delim = ":", names = c(NA,"Sex","Age") ,too_few = "align_end") %>%
      mutate(Age = case_when(Age == "" ~ "Total",
                             T ~ str_trim(Age)),
             Sex = str_trim(Sex),
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
      filter(Sex != "Total" & Age != "Total") %>% #probably a way to make efficient
      rename(Population = value) %>%
      group_by(GEOID, County, State, Sex, Age, Year, Region) %>%
      summarize(Population = sum(Population)) %>%
      drop_na() %>%
      ungroup()
  }
}

# ## --> QC check POP df to see totals, look over years -----
# check_pop <- POP %>%
#   reduce(bind_rows)
#
# # Take a look at totals by
# check_pop %>%
#   group_by(Region, Year) %>%
#   summarize("Pop" = sum(Population, na.rm = TRUE))
#
# check_pop %>%
#   janitor::tabyl(Region, Year)



# Pull PEP data -----------------------------------------------------------
PEP_DATA <- tibble()

for (STATE in names(COUNTIES)) {

    PEP_TEMP <- get_estimates(product="characteristics", geography = "county",
                              county = COUNTIES[[STATE]], state = STATE, breakdown = c("SEX", "AGEGROUP"),
                              breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE) %>%
      filter(year %in% as.numeric(names(PEP_YEARS)), SEX %in% c("Male", "Female")) %>%
      rename(Population = value, Age = AGEGROUP, Sex = SEX) %>%
      separate(NAME, c("County", "State"), sep = "\\, ") %>%
      mutate(Year = year,
             Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                                State == "Illinois" ~ "External IL",
                                State == "Indiana" ~ "External IN",
                                State == "Wisconsin" ~ "External WI"),
             Age = str_replace_all(Age, "Age ", "")) %>%
      select(-year)

    PEP_TEMP_old <- get_estimates(product="characteristics", geography = "county",
                              county = COUNTIES[[STATE]], state = STATE, breakdown = c("SEX", "AGEGROUP"),
                              breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE, year = 2019) %>% #adding the year gives us the pre-2020 data
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
      names(PEP_TEMP$Year) <- NULL

    PEP_DATA <- bind_rows(PEP_DATA, PEP_TEMP, PEP_TEMP_old)
}



# Combine POP and PEP data in the POP list ----------------

for(YEAR in PEP_YEARS) {

  POP[[as.character(YEAR)]] <- PEP_DATA %>%
    filter(Year == YEAR, !(Age %in% PEP_remove)) %>% # Restrict to specific year and age groupings
    arrange(GEOID)
}

# Import Excel files for select years and save final POP data file -------------------------------

POP[["1995"]] <- read_excel("Input/Pop1995.xlsx")
POP[["2005"]] <- read_excel("Input/Pop2005.xlsx")


# Finalize ------------------------------------

# sort list elements by year
POP <- POP[as.character(sort(as.numeric(names(POP))))]


# ## --> QC check POP df w/ PEP to see totals, look over years -----
# check_tot <- POP %>%
#   reduce(rbind)
#
# # Take a look at totals by region and year
# check_tot %>%
#   janitor::tabyl(Region, Year)
#
# check_tot <- check_tot %>%
#   group_by(Region, Year) %>%
#   summarize("Pop" = sum(Population, na.rm = TRUE)) %>%
#   arrange(Region, Year) %>%
#   pivot_wider(names_from = "Year", values_from = "Pop")





# save POP list to Output folder
save(POP, file="Output/POP_PEP.Rdata")

rm(list=setdiff(ls(), c("POP", "COUNTIES", "CMAP_GEOIDS")))
