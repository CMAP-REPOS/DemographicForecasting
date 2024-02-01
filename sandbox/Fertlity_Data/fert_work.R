library(tidyverse)
library(tidycensus)

#fertility_test

by_race <- read_delim("sandbox\\Fertlity_Data\\Natality, 2016-2022 by race.txt") |>
  janitor::clean_names() |>
  filter(county_of_residence_code %in% cmapgeo::county_fips_codes$cmap) |>
  select(!c("notes","mothers_single_race_6_code")) |>
  group_by(mothers_single_race_6) |>
  summarize(
    total_women = sum(female_population),
    total_births = sum(births),
    fert_rate = (total_births/total_women)*1000
  )


by_race_eth <- read_delim("sandbox\\Fertlity_Data\\Natality, 2016-2022 by race_eth.txt") |>
  janitor::clean_names() |>
  filter(county_of_residence_code %in% cmapgeo::county_fips_codes$cmap,
         mothers_hispanic_origin != "Unknown or Not Stated") |>
  select(!c("notes","mothers_single_race_6_code")) |>
  mutate(race_eth = case_when(
    mothers_hispanic_origin == "Hispanic or Latino" ~ "hispanic",
    T ~ str_c("non-hispanic ", tolower(mothers_single_race_6))
  )) |>
  group_by(race_eth) |>
  summarize(
    total_women = sum(as.numeric(female_population)),
    total_births = sum(as.numeric(births)),
    fert_rate = (total_births/total_women)*1000
  )

acs_vars <- load_variables(2019,"acs5")

acs_query_vars <- c(
  "native_women_18_plus" = "B05003_020",
  "foreign_women_18_plus" = "B05003_021"
)

acs_query <- get_acs(geography = "county",
                     variables = acs_query_vars,
                     year = 2021,
                     survey = "acs5",
                     output = "wide") |>
  filter(GEOID %in% cmapgeo::county_fips_codes$cmap) |>
  summarize(total_native = sum(native_women_18_plusE),
            total_foreign = sum(foreign_women_18_plusE))


by_nativity <- read_delim("sandbox\\Fertlity_Data\\Natality, 2016-2022 by nativity.txt") |>
  janitor::clean_names() |>
  filter(county_of_residence_code %in% cmapgeo::county_fips_codes$cmap,
         mothers_nativity != "Unknown or Not Stated") |>
  select(!c("notes","mothers_nativity_code")) |>
  group_by(mothers_nativity) |>
  summarize(
    total_births = sum(as.numeric(births)),
  ) |>
  mutate(total_population = case_when(
    mothers_nativity == "Born in the U.S. (50 US States)" ~ acs_query$total_native,
    T ~ acs_query$total_foreign
  ),
  birth_rate = (total_births/total_population)*1000)
