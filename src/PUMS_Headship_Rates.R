library(tidyverse)
library(tidycensus)


# Get PUMS person-level age data ------------------------------------------

pums_il <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "17", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "18", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "55", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)

load("Output/PumaRegions.Rdata")

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

#load("Output/PUMA_region_assignments.Rdata")


pums_21co <- pums_21co %>%
  right_join(puma_region, by=c("PUMA" = "PUMACE10", "ST" = "STATEFP10")) %>% #added in region here
  select(-SERIALNO, -GEOID10)
# Calculate headship rates for each age group by PUMA ---------------------

total_pop <- pums_21co %>%
  group_by(ST, PUMA, AgeGroup, Region) %>% #only change here was adding in region
  summarize(TotalPop = sum(PWGTP), .groups = 'drop') %>% # Sum person weights
  select(-ST, -PUMA) %>%
  group_by(AgeGroup, Region) %>%
  summarise(TotalPop = sum(TotalPop))

householders <- pums_21co %>%
  filter(IsHouseholder == TRUE) %>%
  group_by(ST, PUMA, AgeGroup, Region) %>%
  summarize(NumHouseholders = sum(PWGTP), .groups='drop') %>%
  select(-ST, -PUMA) %>%
  group_by(AgeGroup, Region) %>%
  summarise(NumHouseholders = sum(NumHouseholders))# Sum person weights

HEADSHIP_RATES <- left_join(householders, total_pop) %>%
  mutate(HeadshipRate = NumHouseholders / TotalPop)

write.csv(HEADSHIP_RATES, "/Users/mweber/Desktop/HSRates.csv")


