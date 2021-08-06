library(tidyverse)
library(tidycensus)


# Get PUMS person-level age data ------------------------------------------

pums_il <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "17", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "18", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "55", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)


# Join PUMS data to PUMA region assignments -------------------------------

pums_21co <- bind_rows(pums_il, pums_in) %>%
  bind_rows(pums_wi) %>%
  rename(ExactAge = AGEP) %>%
  mutate(IsHouseholder = SPORDER == 1,
         AgeGroup = case_when(
           ExactAge < 85 ~ paste(floor(ExactAge/5)*5, "to", floor(ExactAge/5)*5+4, "years"),
           TRUE ~ "85 years and over"
         )) %>%
  select(SERIALNO, PUMA, ST, PWGTP, IsHouseholder, ExactAge, AgeGroup)

load("Output/PUMA_region_assignments.Rdata")

pums_21co <- PUMAregions %>%
  select(PUMA, Region) %>%
  inner_join(pums_21co, by= "PUMA")

# Calculate headship rates for each age group by PUMA ---------------------

total_pop <- pums_21co %>%
  group_by(ST, PUMA, AgeGroup) %>%
  summarize(TotalPop = sum(PWGTP))  # Sum person weights

householders <- pums_21co %>%
  filter(IsHouseholder == TRUE) %>%
  group_by(ST, PUMA, AgeGroup) %>%
  summarize(NumHouseholders = sum(PWGTP))  # Sum person weights

HEADSHIP_RATES <- left_join(householders, total_pop) %>%
  mutate(HeadshipRate = NumHouseholders / TotalPop)

## TO DO: Calculate rates by county instead of PUMA. Have to assign PUMAs to
## counties (or counties to PUMAs, in rural areas).
