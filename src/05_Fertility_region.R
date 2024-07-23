# CMAP | Alexis McAdams, Mary Weber, Alex Bahls | 5/9/2024

#note that the table of contents/structure here has more detail than the others; not sure if
#its helpful or too much

library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)

load("Output/POP_PEP.Rdata") # POP
load("Output/GQData2_region.Rdata") # GQ, GQ_Military, GQratios

CMAP_GEOIDS <- cmapgeo::county_fips_codes$cmap

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

# Set parameters ----------------------------------------------------------

GQE_YEARS <- c(2011:2019) #GQ estimate year range
F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")
BASE_YEAR <- 2014 #Base year population, ASFR projections are built off of this year because it is midpoint of our 2010-2018 data

# calc female HOUSEHOULD pop ----------------------------

## Step 1: Calculate 2010 GQ totals by County, excluding military ------
gq_totals_2010 <- gq %>%
  filter(category == 'County Total',
         concept != 'GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE') %>%
  group_by(geoid, county) %>%
  summarise(county_gq_total = sum(value), .groups = "drop") %>%
  select(geoid, county, county_gq_total)

## Step 2: Calculate 2010 Female GQ totals by County, Sex, Age, excluding military ------
female_gq_2010 <- gq %>%
  filter(sex == 'Female',
         concept != 'GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE',
         age %in% F_Groups) %>%
  group_by(geoid, age, region) %>% #region is redundant here but keeping for later use
  summarise(female_gq = sum(value), .groups = "drop")

## Step 3: For each GQ female age group of interest, calculate what proportion of each county's total 2010 GQ population they represent ------
gq_2010 <- female_gq_2010 |>
  left_join(gq_totals_2010) %>%
  mutate(prop_female = female_gq/county_gq_total)

## Step 4: Read in GQ Census estimates by State, County for 2011-2019 -----

#data downloaded from this page: https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html
#table: Population, Population Change, and Estimated Components of Population Change: April 1, 2010 to July 1, 2019 (CO-EST2019-alldata) [<1.0 MB]

#for 2020 onwards you can now access this via the get_estimates() command

gq_pep <- read_csv("Input/co-est2019-alldata.csv")

gq_proc <- gq_pep |>
  mutate(geoid = str_c(STATE,COUNTY)) |>
  filter(geoid %in% setdiff(unlist(cmapgeo::county_fips_codes), c("18073","18111"))) |>  #MSA includes two extra IN counties
  select(geoid, setdiff(starts_with("GQ"), ends_with("2010"))) |> #dont need 2010 since we have Census counts
  pivot_longer(cols = GQESTIMATES2011:GQESTIMATES2019, names_to = c("year")) |>
  mutate(year = parse_number(year)) |>
  rename(population = value) |>
  distinct()

## Step 5: Multiply 2010 proportions by 2011-2019 GQ estimates to get expected number of females ----
gqe_impute <- gq_2010 |>
  left_join(gq_proc, by = "geoid") %>%
  mutate(gqe_pred = round(prop_female*population,0)) %>%
  select(geoid, age, gqe_pred, year, region) #region redundant but used later

## Step 6: Female TOTAL population estimates by County, Age and Sex for 2011-2019 ----
yearly_total_pop <- tibble()
for (YEAR in GQE_YEARS) {

  pop_est <- POP[[as.character(YEAR)]] %>%
    filter(sex == 'Female')  %>%
    filter(age %in% F_Groups) %>%
    group_by(geoid, year, age) %>%
    summarise(county_total = sum(population), .groups = "drop") |>
    select(geoid, year, age, county_total)

  yearly_total_pop <- bind_rows(yearly_total_pop, pop_est)
  rm(pop_est)
}

# lengths(lapply(yearly_total_pop, unique)) # quick check

## Step 7: For 2011-2019 subtract expected GQ population from county Total to get County HH populations -----
hh_population <- gqe_impute |>
  left_join(yearly_total_pop) %>%
  mutate(hh_pop = county_total - gqe_pred) %>%
  select(geoid, age, hh_pop, year, region) #region redundant but used later


#the last version of this implied that the Census data was household counts (i.e. excluded GQ) but the Census variable used (P0120)
#clearly include GQ (universe = "total population") -- https://www.socialexplorer.com/data/C2010/metadata/?ds=SF1&table=P0120

## Step 8: Filter 2010 Decennial Census HH Data for females 15-44 -----
female_hh_2010 <- POP[["2010"]]  %>%
    filter(sex == 'Female') %>%
    filter(age %in% F_Groups) |>
    clean_names()  |>
    left_join(female_gq_2010) |>  #from Step 2
    mutate(hh_pop = population - female_gq) |>
    select(geoid, age, hh_pop, year, region)

## Step 9: Merge 2010 Census HH Population data and 2011-2019 HH Population Estimates to form a complete table of Household Population data -----
f_hh_data <- rbind(female_hh_2010, hh_population)


#was missing one row, manually adding for now with average of 3 previous years
kankakee_estimate_2018 <- read_excel("Input/CMAPBirths_1990-2019.xlsx") |>
  clean_names() |>
  filter(geoid == 17091,
       year %in% 2015:2017,
       age == "30 to 34 years") |>
  mutate(average_births = mean(births),
         year = 2018,
         county = "Kankakee County",
         region = "External IL") |>
  distinct(geoid, county, region, age, year, births = average_births)

# calc number of births in baseline period ------------------
births <- read_excel("Input/CMAPBirths_1990-2019.xlsx") %>%
  filter(Year %in% (2010:2018)) %>% # filtered out incomplete years data (2019 and 2020)
  mutate(Age = case_when(Age %in% c("10 to 14 years") ~ "15 to 19 years", #these groups are small enough we just combine them with other groups
                         Age %in% c("45 to 49 years") ~ "40 to 44 years",
                         TRUE ~ Age)) %>%
  clean_names() |>
  group_by(geoid, county, age, year, region) %>%
  summarize(births = sum(births), .groups = "drop") %>%
  drop_na() |>
  rbind(kankakee_estimate_2018) |>
  mutate(geoid = as.character(geoid)) #for join in next section

# ASFR Calculation  -----
## region asfr -----
asfr <- f_hh_data %>%
  inner_join(births, by = c("geoid", "age", "year")) %>%
  mutate(asfr = round((births/hh_pop)*1000, 2))  ##asfr = birhts per thousand

##this is the ASFR by YEAR/COUNTY/AGE -- next section combines the years and the regions

## Combine population and summed birth data to generate base year ASFRs by region
base_year_pop <- f_hh_data %>%
  filter(year %in% 2010:2018) %>% #filter out 2019 because we don't have 2019 births for all regions
  filter(year == BASE_YEAR) %>%
  group_by(age, region) %>%
  summarise(hh_pop = sum(hh_pop),
            .groups="drop")

base_year_asfr <- births %>%
  group_by(age, region) %>%
  summarise(births=sum(births),
            .groups="drop") %>%
  left_join(base_year_pop, by=c("age","region")) %>%
  mutate(base_asfr = births/hh_pop/9) %>% #9 is the number of years of data we have (2010-2018)
  select(age, region, base_asfr)

#check TFR
#BaseYearASFR %>% group_by(Region)%>% summarise(TFR = sum(baseASFR)*5)


## Census ASFR Projections ----------------------------------------------

# Import 2023 ASFR projections data from Census Bureau
#   Note: could try using the package censusapi to import directly


census_asfrs <- read_csv("Input/np2023_a1.csv") %>%
  filter(GROUP == "0") %>% #keep only the total ASFRs (otherwise divided by race + ethnicity)
  select(!GROUP) %>%
  pivot_longer(!YEAR, names_to = "age", values_to="asfr") |>
  clean_names()

#General Note/ explanation for change in the code:
  # the "Births" file takes all births for women 45-49 years and assigns them to the 40-44 group
  # the "f_hh_data" file, the births denominator, does not include 45-49 year old women in the denominator
        # the overall effect of this as I reason it out depends on the relative sizes of each cohort
        # if we assume that:
                  #1. the birthrate for 45-50 is lower than 40-44 (almost certaintly true)
                  #2. the 45-50 has fewer people than 40-44 (less positive but generally true in data)
          # there are two implications if both are true
              # the calculated birthrate for the 40-44 group will result in the correct number of births, since the numerator ASFR ends up being larger than true but the denominator is smaller than true (since we only include the hh_pop for 40-44)
              # if we simply add the birth rates of the 45-50 group to the 40-44 group, we get an ASFR that is too high (since we are only adjusting the numerator up)

  #this was all a way of thinking ou that I think we should only include the Census age ranges for the actual age group (40-45) and not tack on the older ASFRs
  #if we simply add them the calculated ASFRs would be too high

 #see sandbox/asfr_test.xlsx for example


## Calculate national projected ASFRs for each age group and each year
census_national_asfrs_proj <- census_asfrs %>%
  mutate(age = parse_number(age),
         age_group = case_when(age %in% 15:19 ~ "15 to 19 years",
                             age %in% 20:24 ~ "20 to 24 years",
                             age %in% 25:29 ~ "25 to 29 years",
                             age %in% 30:34 ~ "30 to 34 years",
                             age %in% 35:39 ~ "35 to 39 years",
                             age %in% 40:44 ~ "40 to 44 years"))%>%
  drop_na() %>% #remove the projections for >45
  group_by(year, age_group) %>%
  summarise(national_asfr = sum(asfr)/5) #average the ASFRs for each age group

##Pull out just the projected ASFRs for the Base Year
#not inclued in 2023 datta so need to use older data
census_national_base_year <- read_csv("Input\\projectedbirths_Census2014.csv")  %>%
  filter(group == "0") %>% #keep only the total ASFRs (otherwise divided by race + ethnicity)
  select(!group) %>%
  pivot_longer(!year, names_to = "age", values_to="asfr") |>
  clean_names() |>
  mutate(age = parse_number(age),
         age_group = case_when(age %in% 15:19 ~ "15 to 19 years",
                              age %in% 20:24 ~ "20 to 24 years",
                              age %in% 25:29 ~ "25 to 29 years",
                              age %in% 30:34 ~ "30 to 34 years",
                              age %in% 35:39 ~ "35 to 39 years",
                              age %in% 40:44 ~ "40 to 44 years")) |>
  drop_na() %>% #remove the projections for >45
  group_by(year, age_group) %>%
  summarise(national_asfr = sum(asfr)/5) |>  #average the ASFRs for each age group
  filter(year==BASE_YEAR) %>%
  rename(base_asfr = national_asfr) %>%
  ungroup() %>%
  select(-year)

#Join the Census Base Year ASFR values to the projected ASFR values
#and calculate the ratio (projected ASFR / base year ASFR)
census_national_asfr_join <- census_national_asfrs_proj |>
  left_join(census_national_base_year, by = "age_group") %>%
  ungroup() %>%
  mutate(census_ratio = national_asfr / base_asfr) %>%
  select(year, age_group, census_ratio)


## ASFR projections -- Region ----------------------------------------------

#Apply the Census ratio to our region's base year ASFRs
asfr_projections_region <- census_national_asfr_join |>
  full_join(base_year_asfr, by = c("age_group" = "age"))  %>%
  mutate(asfr_proj = census_ratio * base_asfr) %>%
  rename(age = age_group) %>%
  select(region, age, year, asfr_proj) # %>%
  #filter(Year %% 5 == 0) #select just the 5-year ASFRs

#Calculate ASFR midpoints
asfr_proj_five_year <- asfr_projections_region %>%
  pivot_wider(names_from = "year", values_from="asfr_proj") %>%
  mutate('asfr_2023.5'=rowMeans(across('2023':'2025')), #think about incorporating observed values here
         'asfr_2027.5'=rowMeans(across('2025':'2030')),
         'asfr_2032.5'=rowMeans(across('2030':'2035')),
         'asfr_2037.5'=rowMeans(across('2035':'2040')),
         'asfr_2042.5'=rowMeans(across('2040':'2045')),
         'asfr_2047.5'=rowMeans(across('2045':'2050')),
         'asfr_2052.5'=rowMeans(across('2050':'2055')),
         'asfr_2057.5'=rowMeans(across('2055':'2060')) ) %>%
  rename_with(.fn = ~paste0("asfr",.), .cols=starts_with("2")) %>%
  ungroup()

#export the ASFR projections
save(asfr_proj_five_year, file="Output/ASFR_region.Rdata")


# Birth Ratios ------------------------------------------------------------

###########------------ calculate sex by births ratios for each region

#Import county-level births by sex data (2014-2018)
birth_gender_data <- read_excel("Input/Births_CountyGender.xlsx") %>%
  filter(Year %in% 2014:2018) |>
  clean_names()

#calculate totals by region, year and sex
birth_gender_data_by_region <- birth_gender_data %>%
  group_by(year, region, sex) %>%
  summarise(births_by_gender = sum(births))

#calculate totals by region and year
birth_gender_data_by_region_year <- birth_gender_data %>%
  group_by(year, region) %>%
  summarise(total_births = sum(births))

#calculate the ratios by year, region and sex, and take average across all years
gender_ratios <- birth_gender_data_by_region |>
  left_join(birth_gender_data_by_region_year, by = c("year", "region")) %>%
  mutate(gender_ratio = births_by_gender / total_births) %>%
  group_by(region, sex) %>%
  summarize(average_gender_ratio = mean(gender_ratio)) %>%
  pivot_wider(names_from = "sex", values_from = "average_gender_ratio") |>
  clean_names() |>
  left_join(tibble(county_name = str_c(names(cmapgeo::county_fips_codes$cmap), " County"),
                   county_fips = cmapgeo::county_fips_codes$cmap), #need to translate county name to geoid
            by = c("region" = "county_name")) |>
  select(!county_fips)


#ab -- honetly have no idea what this is doing and I think its vestigal; going to comment
# out for now  --- this file doesn't use PUMS data at all?
# also its output it never written anywhere

#TEMPORARY#
# this part replaces faulty PUMS-derived IN ratios with average of other 3 regions' ratios
# MUST FIX THIS when we have the real IN Births data
# bRatiostemp <- bRatios %>% filter(Region != "External IN") %>%
#   ungroup() %>%
#   summarize(averagef = mean(Female), averagem = mean(Male))
# bRatios[3,2] <- bRatiostemp[1,1]
# bRatios[3,3] <- bRatiostemp[1,2]

# rm(btemp)
# rm(btemp2)
# rm(bdata)
# rm(bRatiostemp)

save(F_Groups, gender_ratios, file="Output/BirthRatios_region.Rdata")

