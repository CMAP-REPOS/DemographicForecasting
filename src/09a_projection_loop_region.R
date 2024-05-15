# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

#this file is not intended to be run by itself but is relied upon by 07_projection_control.R


# set params for current cycle --------------------------------------------

#load in variables from projection_control
baseyr = as.character(baseyear)    #"2020"
startyr = as.character(projstart)  #"2020"
midpointyr = as.character(projmidpoint)  #"2022.5"
endyr = as.character(projend)  #"2025"
cycleyears = projyears # c(2020,2021,2022,2023,2024)
lastyear = as.character(max(cycleyears))


#FIRST: check if this is the first cycle.
  # If first cycle, import 2020 base pop and 2014-2018 net migration rates
  # If NOT first cycle, import previous cycle's pop and net migration rates

if(startyr == baseyr){
  print(paste("GENERATING", baseyr, "PROJECTION"))

  #Import the baseyear population data (2020) -- the pop file is by county, this makes it by region
  base_year_pop_table <- POP[[baseyr]] %>%
    group_by(age, region, sex) %>%
    summarise(total_pop = sum(population), .groups = "drop")

}else{
  print(paste("GENERATING", max(cycleyears)+1, "PROJECTION"))

  #Load in population data

  base_year_pop_table <- POPPROJ[[startyr]]
  #sort the pop table (may not be necessary, but it's here just in case)
  base_year_pop_table <- base_year_pop_table %>%
    mutate(sex = str_to_title(sex),
      x = as.numeric(str_split_fixed(age, " ", 2)[,1])) %>%
    arrange(x) %>%
    select(-x)

}

# Begin Projection Calculations ---------------------------------------------------------

# Step 1: Grab Age-Sex Specific Survival Rate: Midpoint of 5-year Projection Period

# Step 2: Grab Age Specific Fertility Rate (ASFR): Midpoints of 5-year Projection Period

# project births ----------------------------------------------------------

asfr_midpoint_cycle <- asfr_proj_five_year %>%
  mutate(asfr2022 = asfr_2023.5) |> # need 2020 ASFR for forecast but Census only projects 2023 on and don't have 2022 birth data
  select(c(1:2) | contains(midpointyr) | num_range("asfr", cycleyears))

# Step 3: Special Handling for Calculating Predicted Births and Infant Survival (ages 0-4)

# Step 3 Part 1: Calculate projected Births by Age Cohort and Region in 1-year intervals

#grab possible Mother population (females 15-44) and join to ASFRs
projected_asfr <- base_year_pop_table %>%
  mutate(sex = str_to_lower(sex)) |>
  filter(sex == "female", age %in% F_Groups) %>%
  full_join(asfr_midpoint_cycle, by = c("age", "region"))

projected_births <- projected_asfr
#this probably should be a function

for (year in cycleyears) {
  projected_births[str_c("births_",year)] <-  projected_births[str_c("asfr",year)]*projected_births$total_pop
}

#calculate expected births by one year increments by multiplying base population * ASFR for year of interest, then summarize total births by Region and Year
projected_births_long <- projected_births |>
  select(region, starts_with("birth")) |>
  pivot_longer(cols = starts_with("births"), names_to = "year", values_to = "births_by_age") |>
  mutate(year = str_remove_all(year,"births_")) |>
  group_by(year, region) |>
  summarize(total_births = sum(births_by_age))

# Step 3 part 2: Calculate the number of Births by Region and by Sex of Child
projected_births_long_by_sex <- projected_births_long %>%
  left_join(gender_ratios, by="region") %>%
  mutate(female_births = total_births*female,
         male_births = total_births*male, .keep = "unused")

# Step 3 part 3: calculate survivors by sex and year, then sum for total number of survivors by Region

# project infant deaths -------------------------------------------------
mort_midpoint_cycle <- mort_proj_midpoints %>%
  select(c(1:3) | ends_with(midpointyr))

#pull and rearrange 0-1 and 1-4 Survival Rates by sex and Region from Mort_MidPoint
# Survival rates for 0-1 and 1-4 are applied based on David ER's cohort method
mort_0_4 <- mort_midpoint_cycle %>%
  filter(age %in% c("0 to 1 years","1 to 4 years")) |>
  pivot_wider(names_from = c("sex","age"), values_from = starts_with("mort")) |>
  clean_names()

# end_year_births <- projected_births_long_by_sex$year %>% str_subset(lastyear)

# Calculate the expected number of infants to survive to the next projection cycle
# For those expected to be born in the last year of the projection cycle (aka Babies) they are only exposed to the 0-1 survival rates
# Infants born in the first four years of the projection cycle are first exposed to the 0-1 survival rates and then the 1-4 rates

projected_births_surviving_0_4 <- projected_births_long_by_sex %>%
  left_join(mort_0_4, by="region") %>%
  mutate(surving_female_births = case_when(year == lastyear ~ female_births * female_0_to_1_years,
                                TRUE ~ female_births * female_0_to_1_years * female_1_to_4_years),
         surving_male_births = case_when(year == lastyear ~ male_births  * male_0_to_1_years ,
                                TRUE ~ male_births * male_0_to_1_years * male_1_to_4_years)) %>%
  group_by(region) %>%
  summarize(female = round(sum(surving_female_births),0), male = round(sum(surving_male_births),0)) %>%
  pivot_longer(cols=c("female","male"), names_to = "sex", values_to = "projected_surviving_births") %>%
  mutate(age = "0 to 4 years")

# Calculate expected number of deaths for age 0-4 during the cycle
# Subtract the expected 0-4 population at the end of the cycle from the total births expected during the cycle to estimate
# how many infants born in the current 5 year period will not live to see the first year of the next cycle

early_deaths <- projected_births_long_by_sex %>%
  pivot_longer(cols=c(3:4), names_to = "sex", values_to = "births") %>%
  group_by(region, sex) %>%
  summarize(total_births = sum(births), .groups = 'drop') %>%
  mutate(sex = case_when(sex == "female_births" ~ "female",
                         TRUE ~ "male")) %>%
  left_join(projected_births_surviving_0_4, by=c("region","sex")) %>%
  mutate(early_deaths = round(total_births - projected_surviving_births ,0)) %>%
  select(region, sex, age, early_deaths)

# Step 4: Apply Survival Rates and calculate Expected population for the end year of the current cycle

# project non-infant deaths ----------------------------------------------------------

pop_survival_rate_join <- base_year_pop_table %>%
  mutate(age = case_when(
    age == "85 years and older" ~ "85 years and over",
    T ~ age
         )
        ) |>
  left_join(mort_midpoint_cycle, by = c('region', 'age','sex')) %>%
  ungroup() |>
  rename(mort = 5) #have to rename based on column number as variable name changes based on run


# predict pop w/o immigration ---------------------------------------------

expected_pop <- pop_survival_rate_join %>%
  mutate(age_sorts_well = case_when(
                              age == "5 to 9 years" ~ "05 to 9 years",
                              T ~ age)
          ) |>
  arrange(region, sex, age_sorts_well) %>%
  mutate(projected_pop = case_when(!age %in% c("0 to 4 years", "85 years and over") ~ (lag(total_pop) * mort), #to calculate projected pop: multiply prior age group population by survival rate for current age group
                                  age == '85 years and over' ~ (total_pop + lag(total_pop))* mort,
                                  TRUE ~ NA_real_),
         sex = str_to_lower(sex)) %>% #for merging
  select(!c("mort","age_sorts_well")) %>%
  left_join(projected_births_surviving_0_4, by = c("region","sex","age")) %>%
  mutate(projected_pop = case_when(is.na(projected_pop) ~ projected_surviving_births,
                                  TRUE ~ projected_pop), .keep = "unused")

older_deaths <- expected_pop %>%
  mutate(deaths = case_when(age == "0 to 4 years" ~ 0.0,
                            age == "85 years and over" ~ (total_pop + lag(total_pop)) - projected_pop,
                            TRUE ~ lag(total_pop) - projected_pop)) %>%
  select(age, region, sex, deaths)


# add international migration ---------------------------------------------

international_mig_cycle <- intl_imm_project_five_year[[str_c("x",baseyear)]] |>
  select(age = age_group, sex, region, new_immigrants) |>
  group_by(age, sex, region) |>
  summarise(new_immigrants_region = sum(new_immigrants)) |>
  mutate(sex = str_to_lower(sex),
         age = case_when(
           age == "(85,999]" ~ "85 years and over",
           age == "[0,5]" ~ "0 to 4 years",
           age == "(5,10]" ~ "5 to 9 years",
           T ~ str_c(parse_number(str_sub(age, 2, 3)), " to ", parse_number(str_sub(age, -3, -2)) - 1, " years")
         )
  )

expected_pop_international_mig <- expected_pop |>
  left_join(international_mig_cycle) |>
  mutate(pop_with_intl_mig = projected_pop + new_immigrants_region)



# add domestic migration --------------------------------------------------

#LFPRs for forecast period
cbo_proc_forecast_period <- cbo_proc_forecast_ages |>
  filter(year_group == baseyear) |>
  select(!year_group)

#GQ ratios to determine HH pop
gq_ratio_inst <- gq_ratios |>
  select(!c("college_university_student","other_institutional")) |>
  mutate(total_inst_gq_ratio = rowSums(across(where(is.numeric))),
         sex = str_to_lower(sex)) |>
  select(age, sex, region, total_inst_gq_ratio)

#exclude people not in households and use LFPR to determine number in labor force
civ_non_inst <- expected_pop_international_mig |>
  left_join(gq_military |>
              mutate(sex = str_to_lower(sex)) |>
              rename(gq_mil = value)) |>
  left_join(gq_ratio_inst) |>
  mutate(hh_pop = (pop_with_intl_mig*(1-total_inst_gq_ratio)) - gq_mil, #military not included in the inst. gq calc
         pct_non_inst_civilian_over_16 = case_when(
           age %in% c("0 to 4 years","5 to 9 years","10 to 14 years")  ~ 0,
           age == "15 to 19 years" ~ 0.8, #doesnt include 15 y/o olds -- CBO projections are for 16+
           T ~ 1
         ),
         available_for_lf = hh_pop * pct_non_inst_civilian_over_16,
         age_for_cbo_join = case_when(
           age %in% c("80 to 84 years", "85 years and over") ~ "80 to 89 years",
           age %in% c("70 to 74 years","75 to 79 years") ~ "70 to 79 years",
           T ~ age
         )) |>
  left_join(cbo_proc_forecast_period, by = c("age_for_cbo_join" = "age", "sex")) |>
  mutate(average_lfpr = coalesce(average_lfpr,0),
         num_in_lf = available_for_lf * average_lfpr) |>
  select(age, region, sex, pop_with_intl_mig, num_in_lf, new_immigrants_region)

#what is regionwide labor force for each region
labor_force_regionwide <- civ_non_inst |>
  group_by(region) |>
  summarize(labor_supply = sum(num_in_lf))

#just CMAP for now
civ_non_inst_region <- civ_non_inst |>
  group_by(age, sex, region) |>
  summarize(pop_with_intl_mig = sum(pop_with_intl_mig),
            net_intl_mig = sum(new_immigrants_region)) |>
  ungroup()

#jobs forecast
lf_demand <- read_excel("Input/cc_labor_force.xlsx") |>
  filter(year == projend) |>
  select(!c(average_unemployment, year))

#balance supply and demand
lf_demand_proc <- lf_demand |>
  t() |>
  as_tibble() |>
  cbind(region_cc = names(lf_demand)) |>
  mutate(region = case_when(
    str_detect(region_cc,"17") ~ "CMAP Region",
    str_detect(region_cc,"_in") ~ "External IN",
    str_detect(region_cc,"_il") ~ "External IL",
    str_detect(region_cc,"_wi") ~ "External WI"
  )) |>
  group_by(region) |>
  summarize(labor_demand = sum(V1)) |>
  left_join(labor_force_regionwide) |>
  mutate(workers_needed = labor_demand - labor_supply)


#labor forece multiplier is from domestic_migration.R script and says how many people to expect for each person that moves here for a job
# each opening in the labor force causes ~1.4 people to move
balanced_migration_number <-  lf_demand_proc$workers_needed * labor_force_multiplier

#turn the number into demographic breakdowns
domestic_migration <- labor_force_movers_demo |>
  mutate(domestic_movers = percentage*balanced_migration_number) |>
  select(age, sex, domestic_movers)


# combine all -------------------------------------------------------------

final_projection <- civ_non_inst_region |>
  left_join(domestic_migration) |>
  mutate(total_pop = pop_with_intl_mig + domestic_movers) |>
  select(age, sex, region, total_pop)

