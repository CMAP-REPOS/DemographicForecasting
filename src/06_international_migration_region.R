library(tidyverse)
library(tidycensus)
library(janitor)
library(cmapgeo)

#remove loop
#purr stuff could be more efficient

options(scipen = 999)

load("Output/POP_PEP.Rdata")

county_list <- POP$`2019` |>  #choice of year here is arbitrary
  distinct(geoid, county, state, region)

# calculate average percentage of total -----------------------------------

#use tidy census to get international migration by county by year

##2010-2019 -------------------------------------------------------------------

pep_2010_2019 <- get_estimates(geography = "county",
                               product = "components",
                               state = state.abb,
                               vintage = 2019,
                               time_series = T) |>
  clean_names()

pep_2010_2019_proc <- pep_2010_2019 |>
  mutate(year = period + 2009 ) |>
  filter(year != 2010, #data is generally weird in 2010, think its a partial year
         variable == "INTERNATIONALMIG") |>
  select(!c(name,period, variable)) |>
  pivot_wider(id_cols = geoid,
               names_from = year,
               values_from = value)



## 2020- -------------------------------------------------------------------

pep_2020_2023 <- get_estimates(geography = "county",
                               product = "components",
                               state = state.abb,
                               vintage = 2023,
                               time_series = T) |>
  clean_names()

pep_2020_2023_proc <- pep_2020_2023 |>
  filter(variable == "INTERNATIONALMIG") |>
  select(!c(name, variable)) |>
  pivot_wider(id_cols = geoid,
              names_from = year,
              values_from = value)

# combine years and calc percentages --------------------------------------

pep_all_years <- pep_2010_2019_proc |>
  left_join(pep_2020_2023_proc) |>
  left_join(county_list, by = "geoid") |>
  clean_names() |>
  mutate(across(where(is.numeric), \(x) ifelse(is.na(x),0,x)),
         county = coalesce(county,"Other"),
         state = coalesce(state, "Other"),
         region = coalesce(region, "Other"),
         county = ifelse(county == "Other",
                         county,
                         str_c(county,"-",state,"-",region))) |>
  group_by(county) |>
  summarise(across(where(is.numeric), list(sum))) |>
  t() |>
  row_to_names(row_number = 1) |>
  as_tibble() |>
  clean_names() |>
  mutate_if(is.character, as.numeric)  |>
  mutate(total = rowSums(across(where(is.numeric)), na.rm = T),
         year = row_number() + 2010) |>
  filter(year != 2020) |>  #COVID
  relocate(year)

#add percent of total column
for (x in setdiff(names(pep_all_years),c("year","total"))) {

  new_name <- str_c(x,"_%")
  name <- x
  pep_all_years[new_name] <-pep_all_years[name] / pep_all_years$total

}

mean_mig <- pep_all_years |>
  select(ends_with("%")) %>%
  map_df(~(data.frame(average = mean(.x))),
                      .id = "variable") |>
  mutate(average = ifelse(average < 0, 0, average)) #only one county and its basically 0, fine to set to 0

rm(list=setdiff(ls(), c("mean_mig","pep_all_years")))

# Demographics from PUMS --------------------------------------------------

#using 2019 5-year to avoid COVID and complications of recent PUMS data that mixes decades

il_pums <- get_pums(
  variables = c("PUMA", "MIG", "MIGPUMA", "MIGSP","AGEP","SEX"),
  state = c("IL","IN","WI"),
  survey = "acs5",
  year = 2019,
  recode = TRUE
)

international_mig_age_sex_groups <- il_pums |>
  mutate(international = ifelse(MIG == 2, 1, 0)) |>
  group_by(SERIALNO) |>
  mutate(total_people = n(),
         total_international = sum(international),
         percent_of_hh = total_international/total_people) |>
  filter(international == 1 | #immigrants
           (MIG == "b" & percent_of_hh >= 0.5)) |>  #babies in HH that are majority immigrants
  mutate(age_group = cut(AGEP, c(seq(0,85, by = 5), 999), include.lowest = T)) |>
  group_by(age_group, SEX_label) |>
  summarize(total_in_group = sum(PWGTP)) |>
  ungroup() |>
  mutate(total_migrants = sum(total_in_group),
         percentage_of_immigrants = total_in_group/total_migrants) |>
  select(age_group, sex = SEX_label, percentage_of_immigrants) |>
  mutate(percentage_str = str_c(round(percentage_of_immigrants*100),"%"))

international_mig_wide <- international_mig_age_sex_groups |>
  pivot_wider(names_from = sex,
              values_from =  c(percentage_of_immigrants, percentage_str))



# grab_new_totals ---------------------------------------------------------

# https://www.census.gov/data/tables/2023/demo/popproj/2023-alternative-summary-tables.html
# B. Projected Components of Population Change [< 1.0 MB]

census_projections <- readxl::read_excel("Input/np2023-b.xlsx") |>
  clean_names() |>
  select(year = projected_components_of_population_change,
         baseline = x5,
         low = x9,
         high = x13,
         zero = x17) |>
  filter(parse_number(year) %in% 2023:2050 & !is.na(baseline)) |>
  mutate_if(is.character, as.numeric) |>
  mutate(across(baseline:zero, \(x) x*1000))


# project future migration ------------------------------------------------


year_list <- 2023:2050

international_mig_list <- list()
mean_migration_year <- list()

for (proj_year in year_list) {

  annual_proj <- census_projections |>
    filter(year == proj_year) |>
    pull(baseline)

  tidy_year <- str_c("x",proj_year) #dont wnat to index to start with a number

  international_mig_list[[tidy_year]] <- international_mig_age_sex_groups |>
    mutate(number_immigrants = percentage_of_immigrants*annual_proj) |>
    select(age_group, sex, number_immigrants)

  mean_migration_year[[tidy_year]] <- mean_mig

}

#make a list of endings to use for cleaning later
ending_list <- c("_illinois_external_il_%","_illinois_cmap_region_%",
                 "_wisconsin_external_wi_%","_indiana_external_in_%")

year_cross_fn <- function(annual_demographics, mean_list) {
  expand_grid(annual_demographics, mean_list) |>
    mutate(new_immigrants = number_immigrants * average) |>
    select(age_group, sex, new_immigrants, County = variable) |>
    mutate(State = case_when(
      str_detect(County,"illinois") ~ "Illinois",
      str_detect(County,"indiana") ~ "Indiana",
      str_detect(County,"wisconsin") ~ "Wisconsin",
      T ~ "Other"
    ),
          County = str_remove(County,str_c(ending_list, collapse = "|")),
          County = str_to_title(str_replace_all(County,"_"," ")),
          County = case_when(
            str_count(County, " ") > 1 ~ str_remove(County," "),
            T ~ County
          ),
    Region = case_when(
      County %in% str_c(names(cmapgeo::county_fips_codes$cmap), " County") & State == "Illinois" ~ "CMAP Region", #remember Lake County, IN
      State == "Illinois" ~ "External IL",
      State == "Indiana" ~ "External IN",
      State == "Wisconsin" ~ "External WI"
      )
    ) |>
    filter(County != "Other %")
}

new_list <- map2(international_mig_list, mean_migration_year, year_cross_fn)

#can optimize
new_list_five_year <- list()

new_list_five_year$x2022 <- rbind(new_list$x2023, new_list$x2024)

new_list_five_year$x2025 <- rbind(new_list$x2025, new_list$x2026, new_list$x2027, new_list$x2028, new_list$x2029)

new_list_five_year$x2030 <- rbind(new_list$x2030, new_list$x2031, new_list$x2032, new_list$x2033, new_list$x2034)

new_list_five_year$x2035 <- rbind(new_list$x2035, new_list$x2036, new_list$x2037, new_list$x2038, new_list$x2039)

new_list_five_year$x2040 <- rbind(new_list$x2040, new_list$x2041, new_list$x2042, new_list$x2043, new_list$x2044)

new_list_five_year$x2045 <- rbind(new_list$x2045, new_list$x2046, new_list$x2047, new_list$x2048, new_list$x2049)


# new_list_five_year$x2025 <- new_list_five_year$x2025 |>
#   group_by(age_group, sex, County, Region, State) |>
#   summarize(new_immigrants = sum(new_immigrants))

combine_years <- function(df) {
  df <- df |>
    group_by(age_group, sex, County, Region) |>
    summarize(new_immigrants = sum(new_immigrants)) |>
    clean_names()

  return(df)
}

intl_imm_project_five_year <- map(new_list_five_year, combine_years)

save(intl_imm_project_five_year, file="Output/International_mig_proj_region.Rdata")

