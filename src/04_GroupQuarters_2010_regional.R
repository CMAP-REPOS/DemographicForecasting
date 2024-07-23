# CMAP | Mary Weber, Alexis McAdams, Alex Bahls | 5/9/2024


# This script fetches detailed 2010 Group Quarters populations and calculates ratios of
# GQ type to Pop for each Age group and Sex by Region (with the exception of Military).

# we are basically trying to determine GQ counts for the period we have birth data (2014-2018)
# the issue is the 2014-2018 population estimates from the Census do not split out the GQ population
# however, the 2010 and 2020 Census do have group quarters estimates
# this script assumes the GQ proportions (or, in the case of military, absolute counts) are the same
# in 2014-2018 as they were in 2010. There is an argument we should be using 2020, but I think 2010 is better
# COVID causes general issues with the Census and the Census basically admitted they miscounted GQ in Illinois in 2020
# see https://www.illinois.gov/news/press-release.29476.html

library(tidyverse)
library(tidycensus)

# Set parameters ----------------------------------------------------------

load("Output/POP_PEP.Rdata") # POP

CMAP_GEOIDS <- cmapgeo::county_fips_codes$cmap

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

#PCO07 is the sum of 8, 9, and 10 -- since we are pulling all the parts we dont need it
GQ_TABLES <- c("PCO010", "PCO009", "PCO008", "PCO006", "PCO005", "PCO004", "PCO003")

YEAR <- 2010

# Step 1: fetch 2010 GQ data ------------------------------------------

# Compile list of variables to download
sf1_vars <- load_variables(YEAR, "sf1")
gq_vars <- sf1_vars %>%
  filter(str_starts(name, paste0("^", paste(GQ_TABLES, collapse="|^")))) %>%
  mutate(
    Category = str_replace_all(label, ".*\\)!!", ""),
    Category = str_replace_all(Category, "!!", " ")
  )

# Download census GQ data for selected variables in all counties
gq_data <- tibble()
for (STATE in names(COUNTIES)) {
  temp <- get_decennial(geography = "county", variables = gq_vars$name,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = YEAR, survey = "sf1", cache_table = TRUE)
  gq_data <- bind_rows(gq_data, temp)
}


# Step 2 -- process 2010 GQ data -------------------------------------------

# Assemble final table of GQ population data, rename cols and reformat values
gq <- gq_data %>%
  left_join(gq_vars, by = c("variable" = "name")) %>%
  select(-label) %>%
  janitor::clean_names() %>%
  separate_wider_delim(name, delim = ", ",names = c("county", "state")) %>%
  separate_wider_delim(category, delim = " ", names = c("sex","age"), too_many = "merge",
                       too_few = "align_start", cols_remove = F) %>%
  mutate(age = case_when(is.na(age) | age == "" | str_detect(age, "\\)") ~ "Total",
                         age == "Under 5 years" ~ "0 to 4 years",
                         T ~ str_trim(age)),
         sex = case_when(str_detect(sex,"Total") ~ "Total",
                         T ~ str_trim(sex)),
    year = YEAR,
    category = case_when(age == "Total" & sex == "Total" ~ "County Total",
                         age == "Total" & sex == "Male" ~ "County Male Total",
                         age == "Total" & sex == "Female"  ~ "County Female Total",
                         T ~ str_trim(paste0(sex," ",age))),
    region = case_when(
      geoid %in% cmapgeo::county_fips_codes$cmap ~ "CMAP Region",
      state == "Illinois" ~ "External IL",
      state == "Wisconsin" ~ "External WI",
      state == "Indiana" ~ "External IN"
    )
  )

# Remove the rows for Population Totals, sum up the GQ populations by GQ type age and sex, fix 0-4 Age value
new_age_groups_to_remove <- c("Under 20 years","Under 25 years","25 years and over", "65 years and over")

gq_long <- gq %>%
  filter(sex != "Total",
         age != "Total") %>%
  group_by(region, sex, age, concept) %>%
  summarize(population = sum(value)) %>%
  filter(!age %in% new_age_groups_to_remove) %>%
  ungroup()

# Step 3: calculate GQ Ratios ------------------------------------------

# import the 2010 Census Population (these values are the all population, including GQ)
pop_2010 <- POP[["2010"]] %>%
  group_by(region, age, sex) %>%
  summarize(total_pop = sum(population))

# Join the GQ sums to 2010 population, calculate the GQ ratios for every GQ type
# GQ ratio = GQ pop / (totalpop)
gq_ratios <- gq_long |>
  left_join(pop_2010, by=c("region", "age", "sex")) %>%
  mutate(ratio = population / total_pop,
         concept = word(concept, start = 5, end = 6)) %>% #shorten the Concept col values (types of GQs)
  select(!c("population","total_pop")) %>%
  pivot_wider(names_from = concept, values_from = ratio) |>
  janitor::clean_names() |>
  select(!military_quarters) #remove military

# split out Military population total by age and sex - this value is held constant in future projections --
# note that everywhere outside of Lake County (naval base) has been 0 military in past few analyses
gq_military <- gq %>%
  filter(concept == "GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE",
         sex != "Total",
         age != "Total") |>
  group_by(region, sex, age) %>%
  mutate(value = sum(value)) %>%
  select(value, sex, age, region) |>
  unique() %>%
  ungroup()

save(gq, gq_military, gq_ratios, file="Output/GQData2_region.Rdata")


