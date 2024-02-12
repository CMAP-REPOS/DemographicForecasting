library(tidyverse)
library(tidycensus)
library(janitor)
library(cmapgeo)

#remove loop
#purr stuff could be more efficient

options(scipen = 999)

load("Output/POP_PEP.Rdata")

county_list <- POP$`2019` |>  #choice of year here is arbitrary
  distinct(GEOID, County, State, Region)

# calculate average percentage of total -----------------------------------

pep_2010_ftp <- read_csv("Input/pep_2020_2010.csv")
pep_2020_ftp <- read_csv("Input/pep_2022_2020.csv")

pep_2010_ftp_proc <- pep_2010_ftp |>
  filter(COUNTY != 0) |> #these are whole states
  mutate(new_state = case_when(
    nchar(STATE) == 1 ~ str_c("0",as.character(STATE)),
    T ~ as.character(STATE)
  ),
  new_county = case_when(
    nchar(COUNTY) == 1 ~ str_c("00",as.character(COUNTY)),
    nchar(COUNTY) == 2 ~ str_c("0",as.character(COUNTY)),
    T ~ as.character(COUNTY)
  ),
  county_fips = str_c(new_state, new_county)) |>
  select(county_fips, starts_with("INTERNATIONALMIG"))

pep_2020_ftp_proc <- pep_2020_ftp |>
  filter(COUNTY != 0) |> #these are whole states
  mutate(new_state = case_when(
    nchar(STATE) == 1 ~ str_c("0",as.character(STATE)),
    T ~ as.character(STATE)
  ),
  new_county = case_when(
    nchar(COUNTY) == 1 ~ str_c("00",as.character(COUNTY)),
    nchar(COUNTY) == 2 ~ str_c("0",as.character(COUNTY)),
    T ~ as.character(COUNTY)
  ),
  county_fips = str_c(new_state, new_county)) |>
  select(county_fips, starts_with("INTERNATIONALMIG"))


# combine years and calc percentages --------------------------------------

pep_all_years <- pep_2010_ftp_proc |>
  left_join(pep_2020_ftp_proc) |>
  janitor::clean_names() |>
  mutate(internationalmig2021 = ifelse(is.na(internationalmig2021),0,internationalmig2021),
         internationalmig2022 = ifelse(is.na(internationalmig2022),0,internationalmig2022)) |>
  left_join(county_list, by = c("county_fips" = "GEOID")) |>
  mutate(County = coalesce(County,"Other"),
         State = coalesce(State, "Other"),
         Region = coalesce(Region, "Other"),
         County = ifelse(County == "Other",County,
                         str_c(County,"&&",State,"&&",Region))) |>
  group_by(County) |>
  summarise(across(where(is.numeric), list(sum))) |>
  select(!c("internationalmig2021_1","internationalmig2022_1","internationalmig2010_1")) |>
  t() |>
  row_to_names(row_number = 1) |>
  as_tibble() |>
  clean_names() |>
  mutate_if(is.character, as.numeric) |>
  mutate(total = rowSums(across(where(is.numeric))),
         year = row_number() + 2010) |>
  filter(year != 2020) #weird year

#add percent of total column
for (x in setdiff(names(pep_all_years),c("year","total"))) {

  new_name <- str_c(x,"_%")
  name <- x
  pep_all_years[new_name] <-pep_all_years[name] / pep_all_years$total

}

mean_mig <- pep_all_years |>
  select(ends_with("%")) %>%
  map_df(~(data.frame(average = mean(.x))),
                      .id = "variable")

rm(list=setdiff(ls(), c("mean_mig","pep_all_years")))

# Demographics from PUMS --------------------------------------------------

il_pums <- get_pums(
  variables = c("PUMA", "MIG", "MIGPUMA", "MIGSP","AGEP","SEX"),
  state = "IL",
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
           MIG == "b" & percent_of_hh >= 0.5) |>  #babies in HH that are majority immigrants
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



year_list <- 2023:2050

international_mig_list <- list()
mean_migration_year <- list()

for (proj_year in year_list) {

  annual_proj <- census_projections |> filter(year == proj_year) |> pull(baseline)

  tidy_year <- str_c("x",proj_year) #dont wnat to index to start with a number

  international_mig_list[[tidy_year]] <- international_mig_age_sex_groups |>
    mutate(number_immigrants = percentage_of_immigrants*annual_proj) |>
    select(age_group, sex, number_immigrants)

  mean_migration_year[[tidy_year]] <- mean_mig

}

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
          Region = case_when(
            str_detect(County,"external_il") ~ "External IL",
            str_detect(County,"external_in") ~ "External IN",
            str_detect(County,"external_wi") ~ "External WI",
            str_detect(County,"cmap_region") ~ "CMAP Region",
            T ~ "Other"
          ),
          County = str_remove(County,str_c(ending_list, collapse = "|")),
          County = str_to_title(str_replace_all(County,"_"," ")),
          County = case_when(
            str_count(County, " ") > 1 ~ str_remove(County," "),
            T ~ County
          )
    ) |>
    filter(County != "Other %")
}

new_list <- map2(international_mig_list, mean_migration_year, year_cross_fn)

#can optimize
new_list_five_year <- list()

new_list_five_year$x2025 <- rbind(new_list$x2023, new_list$x2024, new_list$x2025)

new_list_five_year$x2030 <- rbind(new_list$x2026, new_list$x2027, new_list$x2028, new_list$x2029, new_list$x2030)

new_list_five_year$x2035 <- rbind(new_list$x2031, new_list$x2032, new_list$x2033, new_list$x2034, new_list$x2035)

new_list_five_year$x2040 <- rbind(new_list$x2036, new_list$x2037, new_list$x2038, new_list$x2039, new_list$x2040)

new_list_five_year$x2045 <- rbind(new_list$x2041, new_list$x2042, new_list$x2043, new_list$x2044, new_list$x2045)

new_list_five_year$x2050 <- rbind(new_list$x2046, new_list$x2047, new_list$x2048, new_list$x2049, new_list$x2050)


# new_list_five_year$x2025 <- new_list_five_year$x2025 |>
#   group_by(age_group, sex, County, Region, State) |>
#   summarize(new_immigrants = sum(new_immigrants))

combine_years <- function(df) {
  df <- df |>
    group_by(age_group, sex, County, Region, State) |>
    summarize(new_immigrants = sum(new_immigrants))

  return(df)
}

five_year_combined <- map(new_list_five_year, combine_years)

save(five_year_combined, file="Output/International_mig_proj.Rdata")

# scraps ------------------------------------------------------------------


#
# pep_all_years <- pep_2010_ftp_proc |>
#   left_join(pep_2020_ftp_proc) |>
#   janitor::clean_names() |>
#   mutate(internationalmig2021 = ifelse(is.na(internationalmig2021),0,internationalmig2021),
#          internationalmig2022 = ifelse(is.na(internationalmig2022),0,internationalmig2022),
#          cmap = case_when(county_fips %in% cmapgeo::county_fips_codes$cmap ~ "cmap",
#                           T ~ "not_cmap")) |>
#   select(!county_fips) |>
#   group_by(cmap) |>
#   summarise(across(everything(), list(sum))) |>
#   select(!c("internationalmig2021_1","internationalmig2022_1","internationalmig2010_1")) |>
#   t() |>
#   row_to_names(row_number = 1) |>
#   as_tibble() |>
#   mutate(year = row_number() + 2010,
#          cmap = parse_number(cmap),
#          not_cmap = parse_number(not_cmap),
#          percent_cmap = round((cmap*100)/(cmap+not_cmap),3))

#
# pep_all_years <- pep_2010_ftp_proc |>
#   left_join(pep_2020_ftp_proc) |>
#   janitor::clean_names() |>
#   mutate(internationalmig2021 = ifelse(is.na(internationalmig2021),0,internationalmig2021),
#          internationalmig2022 = ifelse(is.na(internationalmig2022),0,internationalmig2022),
#          cmap = case_when(county_fips %in% cmapgeo::county_fips_codes$cmap ~ "cmap",
#                           T ~ "not_cmap")) |>
#   select(!county_fips) |>
#   group_by(cmap) |>
#   summarise(across(everything(), list(sum))) |>
#   select(!c("internationalmig2021_1","internationalmig2022_1","internationalmig2010_1")) |>
#   t() |>
#   row_to_names(row_number = 1) |>
#   as_tibble() |>
#   mutate(year = row_number() + 2010,
#          cmap = parse_number(cmap),
#          not_cmap = parse_number(not_cmap),
#          percent_cmap = round((cmap*100)/(cmap+not_cmap),3))


#
# |>
#   mutate(baseline_cmap = (as.numeric(baseline)*1000)*(mean_mig/100),
#          low_cmap = (as.numeric(low)*1000)*(mean_mig/100),
#          high_cmap = (as.numeric(high)*1000)*(mean_mig/100),
#          zero_cmap = (as.numeric(zero)*1000)*(mean_mig/100)) |>
#   select(year, ends_with("cmap"))
#
# pep_for_append <- pep_all_years |>
#   select(year, baseline_cmap = cmap) |>
#   mutate(low_cmap = NA,
#          high_cmap = NA,
#          zero_cmap = NA)
#
# trends <- rbind(census_projections, pep_for_append) |> arrange(year)


#
# |>
#   mutate(baseline_cmap = (as.numeric(baseline)*1000)*(mean_mig/100),
#          low_cmap = (as.numeric(low)*1000)*(mean_mig/100),
#          high_cmap = (as.numeric(high)*1000)*(mean_mig/100),
#          zero_cmap = (as.numeric(zero)*1000)*(mean_mig/100)) |>
#   select(year, ends_with("cmap"))
#
# pep_for_append <- pep_all_years |>
#   select(year, baseline_cmap = cmap) |>
#   mutate(low_cmap = NA,
#          high_cmap = NA,
#          zero_cmap = NA)
#
# trends <- rbind(census_projections, pep_for_append) |> arrange(year)
#
# rm(list=setdiff(ls(), c("census_projections")))
