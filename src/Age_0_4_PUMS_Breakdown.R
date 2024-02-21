# CMAP | Noel Peterson, Alex Bahls | 7/12

# This script has two parts: first, it spatially relates 2010 PUMAs to each of the four modeling regions.
# Second, it pulls 2021 ACS data (by PUMA) to determine an estimate of 0-1 and 1-4 year-old individuals
# by Region. The result is a ratio of 0-1 year-olds to 0-4 year-olds that is used in Mortality.R

# AB -- this is currently using 2021 PUMS data to avoid having data in two geographies (the 2022 PUMS data has
# both 2010 PUMS and 2020 PUMS geography depending on year) -- this is something that COULD be improved but right now
# I consider the cost of adding another year relatively high (itd take some thinking to code in) and the payoff to be
# quite low (did the ratio of <1 year olds to 1-4 year olds really change much when looking at 2017 vs 2022?)


library(devtools)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(units)

#install_github("CMAP-REPOS/cmapgeo", build_vignettes=TRUE)
library(cmapgeo)

pums_year <- 2021

####### Part 1 : spatially relate 21 counties to PUMAs

# Get 21-county boundaries
cmap_21co_sf <- filter(county_sf, travel_model)

# Define helper function to determine overlaps with CMAP counties
intersects_21co <- function(in_sf) {
  apply(st_overlaps(in_sf, cmap_21co_sf, sparse = FALSE), 1, any) |
  apply(st_covers(in_sf, cmap_21co_sf, sparse = FALSE), 1, any) |
  apply(st_covered_by(in_sf, cmap_21co_sf, sparse = FALSE), 1, any)
}

# Get PUMAs overlapping the 21-counties, and assign each to a model region
puma_21co_sf <- pumas(state = "17", year= 2021) %>% #2021 comes with 2010 GEOids and boundaries
  bind_rows(pumas(state = "18", year= 2021)) %>%
  bind_rows(pumas(state = "55", year= 2021)) %>%
  st_transform(3435) %>%
  filter(intersects_21co(.)) %>%
  select(GEOID10, STATEFP10, PUMACE10, NAMELSAD10) %>%
  arrange(GEOID10)

calc_intersect <- st_intersection(puma_21co_sf, cmap_21co_sf) %>%
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(GEOID10, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry() %>%
  group_by(GEOID10) %>%
  mutate(total_intersect = sum(intersect_area)) %>%
  distinct(GEOID10,total_intersect)

puma_21co_sf <- puma_21co_sf %>%
  left_join(calc_intersect) %>%
  mutate(area = st_area(puma_21co_sf)) %>%
  filter(total_intersect > set_units(1, US_survey_foot^2)) %>% #need over one foot of intersect -- drops 10 PUMAs
  mutate(Region = case_when(
    STATEFP10 == "18" ~ "External IN",
    STATEFP10 == "55" ~ "External WI",
    str_detect(NAMELSAD10,"(?i)Chicago|Cook|DuPage|Kane|Kendall|Lake|McHenry|Will") ~ "CMAP Region",
    T ~ "External IL")
  )


# Plot PUMAs over counties to verify correctness of coverage and region assignment -- not perfect because some PUMAs encompass
# a modelling region county AND a non-modelling region county
p <- ggplot() +
  geom_sf(data=cmap_21co_sf, lwd=2, col="#999999") +
  geom_sf(data=puma_21co_sf, col="#000000", alpha=0.2) +
  #geom_sf_text(data=puma_21co_sf, mapping=aes(label=GEOID10), col="#990000") +
  theme_void()
# p

# Save data frame of PUMA region assignments to join to PUMS data
puma_region <- puma_21co_sf %>%
  as.data.frame() %>%
  select(GEOID10, STATEFP10, PUMACE10, Region)

save(puma_region, file="Output/PumaRegions.Rdata") # puma_region, used in PUMS_Headship_Rates.R and income.R

############ Part 2: pull 2021 ACS population data by PUMA, calculate proportion of 0-4 age group that is 0-1 yrs old

# Get PUMS person-level age data
pums_il <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "17", year = 2021, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "18", year = 2021, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "55", year = 2021, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)

# Join PUMS data to PUMA region assignments
pums_21co <- bind_rows(pums_il, pums_in) %>%
  bind_rows(pums_wi) %>%
  rename(ExactAge = AGEP) %>%
  mutate(AgeGroup = if_else(ExactAge == 0, "Less than 1 year", "1 to 4 years"),
         Sex = if_else(SEX == 1, "Male", "Female")) %>%
  select(SERIALNO, PUMA, ST, PWGTP, ExactAge, AgeGroup, Sex) %>%
  right_join(puma_region, by=c("PUMA" = "PUMACE10", "ST" = "STATEFP10"))

# Summarize PUMS by age and region
AGE_0_4_FREQ <- pums_21co %>%
  group_by(Region, Sex, AgeGroup) %>%
  summarize(Population = sum(PWGTP)) %>%
  mutate(Age_0_4_Share = Population / sum(Population)) %>%
  ungroup() %>%
  filter(!is.na(Sex))

# Save table to output folder
save(AGE_0_4_FREQ, file="Output/Age_0_4_Freq.Rdata")
