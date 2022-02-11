# CMAP | Noel Peterson | 7/21

# This script has two parts: first, it spatially relates 2010 PUMAs to each of the four modeling regions.
# Second, it pulls 2019 ACS data (by PUMA) to determine an estimate of 0-1 and 1-4 year-old individuals
# by Region. The result is a ratio of 0-1 year-olds to 0-4 year-olds that is used in Mortality.R

#install.packages("devtools")

library(devtools)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

#install_github("CMAP-REPOS/cmapgeo", build_vignettes=TRUE)
#library(cmapgeo)

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
puma_21co_sf <- pumas(state = "17") %>%
  bind_rows(pumas(state = "18")) %>%
  bind_rows(pumas(state = "55")) %>%
  st_transform(3435) %>%
  filter(., intersects_21co(.)) %>%
  select(GEOID10, STATEFP10, PUMACE10, NAMELSAD10) %>%
  arrange(GEOID10) %>%
  filter(!(GEOID10 %in% c(  # Exclude PUMAs with only a small overlap
    "1702200", "1702501",
    "1800402", "1800700",
    "5500800", "5501001", "5502400", "5540301", "5570101", "5570201"
  ))) %>%
  mutate(Region = case_when(
    STATEFP10 == "18" ~ "External IN",
    STATEFP10 == "55" ~ "External WI",
    TRUE ~ if_else(
      str_detect(NAMELSAD10,"(?i)Chicago|Cook|DuPage|Kane|Kendall|Lake|McHenry|Will"),
      "CMAP Region",
      "External IL"
    )
  ))

# Plot PUMAs over counties to verify correctness of coverage and region assignment
p <- ggplot() +
  geom_sf(data=cmap_21co_sf, lwd=2, col="#999999") +
  geom_sf(data=puma_21co_sf, mapping=aes(fill=Region), col="#000000", alpha=0.2) +
  #geom_sf_text(data=puma_21co_sf, mapping=aes(label=GEOID10), col="#990000") +
  theme_void()
#p

# Save data frame of PUMA region assignments to join to PUMS data
puma_region <- puma_21co_sf %>%
  as.data.frame() %>%
  select(GEOID10, STATEFP10, PUMACE10, Region)

save(puma_region, file="Output/PumaRegions.Rdata") # puma_region, used in PUMS_Headship_Rates.R and income.R

############ Part 2: pull 2019 ACS population data by PUMA, calculate proportion of 0-4 age group that is 0-1 yrs old

# Get PUMS person-level age data
pums_il <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "17", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "18", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "55", year = 2019, survey = "acs5",
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
  ungroup()

# Save table to output folder
save(AGE_0_4_FREQ, file="Output/Age_0_4_Freq.Rdata")
