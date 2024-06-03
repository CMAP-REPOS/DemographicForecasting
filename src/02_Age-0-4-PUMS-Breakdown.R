# Overview ----------------------------------------------------------------

# CMAP | Noel Peterson, Alex Bahls | 7/12

# This script has two parts: first, it spatially relates 2010 PUMAs to each of the four modeling regions.
# Second, it pulls 2021 ACS data (by PUMA) to determine an estimate of 0-1 and 1-4 year-old individuals
# by Region. The result is a ratio of 0-1 year-olds to 0-4 year-olds that is used in Mortality.R

#we want to keep the PUMS data at 2018 since its used to create mortality rates from death data 2014-2018;
#if we update the mortality data period we need to update PUMS to match

# set up ------------------------------------------------------------------
library(devtools)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(units)
library(janitor)

#install_github("CMAP-REPOS/cmapgeo", build_vignettes=TRUE)
library(cmapgeo)

CMAP_GEOIDS <- cmapgeo::county_fips_codes$cmap

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

#see note above -- should stay at 2018 for now
pums_year <- 2018

#  create puma sf for modelling region -------

#2010 PUMAs for the modelling region should probably be in the V drive but are not currently

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
  mutate(region_cc = case_when(
      STATEFP10 == "18" ~ "External IN",
      STATEFP10 == "55" ~ "External WI",
      str_detect(NAMELSAD10,"Chicago|Cook County") ~ "Cook",
      str_detect(NAMELSAD10,"DuPage County|Dupage County") ~ "DuPage",
      str_detect(NAMELSAD10,"Kane County") ~ "Kane",
      str_detect(NAMELSAD10,"Kendall") ~ "Kendall",
      str_detect(NAMELSAD10,"Lake") ~ "Lake",
      str_detect(NAMELSAD10,"McHenry") ~ "McHenry",
      str_detect(NAMELSAD10,"Will") ~ "Will",
      T ~ "External IL"
    )
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
  select(GEOID10, STATEFP10, PUMACE10, region_cc)

save(puma_region, file="Output/PumaRegions.Rdata") # puma_region, used in PUMS_Headship_Rates.R and income.R


# Pull PUMS Data ----------------------------------------------------------

# https://walker-data.com/census-r/introduction-to-census-microdata.html?q=get_pums#basic-usage-of-get_pums
# Get PUMS person-level age data
pums_il <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "17", year = 2021, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "18", year = 2021, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "55", year = 2021, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)


# analyze PUMS data -------------------------------------------------------

# Join PUMS data to PUMA region assignments
pums_21co <- bind_rows(pums_il, pums_in, pums_wi) %>%
  rename(exact_age = AGEP) %>%
  mutate(age_group = if_else(exact_age == 0, "Less than 1 year", "1 to 4 years"),
         sex = if_else(SEX == 1, "Male", "Female")) %>%
    clean_names() |>
  select(serialno, puma, st, pwgtp, exact_age, age_group, sex = sex_2) %>%
  right_join(puma_region, by=c("puma" = "PUMACE10", "st" = "STATEFP10"))

# Summarize PUMS by age and region
age_0_4_freq_cc <- pums_21co %>%
  group_by(region_cc, sex, age_group) %>%
  summarize(population = sum(pwgtp)) %>%
  mutate(age_0_4_share = population / sum(population)) %>%
  ungroup() %>%
  filter(!is.na(sex))

age_0_4_freq_region <- pums_21co %>%
  mutate(region = case_when(
    str_detect(region_cc, "External") ~ region_cc,
    T ~ "CMAP Region"
  )) |>
  group_by(region, sex, age_group) %>%
  summarize(population = sum(pwgtp)) %>%
  mutate(age_0_4_share = population / sum(population)) %>%
  ungroup() %>%
  filter(!is.na(sex))

# Save table to output folder
save(age_0_4_freq_cc, file="Output/Age_0_4_Freq_cc.Rdata")

save(age_0_4_freq_region, file="Output/Age_0_4_Freq_region.Rdata")


#clean environment
rm(list=setdiff(ls(), c("age_0_4_freq","POP", "COUNTIES", "CMAP_GEOIDS")))
