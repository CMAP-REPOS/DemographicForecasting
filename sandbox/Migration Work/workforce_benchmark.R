#### THIS SCRIPT IS DESIGNED TO BE RUN AS PART OF Moodys_Totals.R and does NOT stand alone


#Census Population Estimates Program (PEP)
PEP_YEARS <- c(`2021` = 2021, `2022` = 2022, `3` = 2010, `4`=2011, `5`=2012, `6`=2013, `7`=2014, `8`=2015, `9`=2016, `10`=2017, `11`=2018, `12`=2019) #2020 is based on 2010 vintage and doesnt use census

#  note: `#` names in PEP_YEARS are necessary to filter correct years of data from PEP pull (see tidycensus documentation, "time_series" argument)
# PEP DATE_CODE documentation: https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html

PEP_remove <- c("Under 18 years", "16 years and over", "18 years and over", "65 years and over",
                "5 to 13 years", "14 to 17 years", "15 to 44 years",
                "18 to 24 years", "18 to 64 years", "25 to 44 years", "45 to 64 years",
                "Median age", "All ages", "85 years and over")

region_counties <- c(county_fips_codes$cmap, county_fips_codes$xil, county_fips_codes$xin, county_fips_codes$xwi)

il_2020_2022 <- get_estimates(
  geography = "county",
  state = "IL",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  time_series = TRUE
) %>% mutate(region = case_when(GEOID %in% county_fips_codes$cmap ~ "CMAP",
                            T ~ "ExIL"))

in_2020_2022 <- get_estimates(
  geography = "county",
  state = "IN",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  time_series = TRUE
) %>% mutate(region = "IN")

wi_2020_2022 <- get_estimates(
  geography = "county",
  state = "WI",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  time_series = TRUE
) %>% mutate(region = "WI")


proc_2020 <- rbind(il_2020_2022,in_2020_2022,wi_2020_2022) %>%
  filter(GEOID %in% region_counties,
         AGEGROUP != "All ages") %>%
  rename(Population = value, Age = AGEGROUP, Sex = SEX) %>%
  select(-NAME)




il_2010_2019 <- get_estimates(
  geography = "county",
  state = "IL",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  time_series = TRUE,
  year = 2019
) %>% mutate(region = case_when(GEOID %in% county_fips_codes$cmap ~ "CMAP",
                                T ~ "ExIL"))

in_2010_2019 <- get_estimates(
  geography = "county",
  state = "IN",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  time_series = TRUE,
  year = 2019
)  %>% mutate(region = "IN")

wi_2010_2019 <- get_estimates(
  geography = "county",
  state = "WI",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  time_series = TRUE,
  year = 2019
) %>% mutate(region = "WI")

proc_2010 <- rbind(il_2010_2019,in_2010_2019,wi_2010_2019) %>%
  filter(GEOID %in% region_counties) %>%
  filter(DATE %in% names(PEP_YEARS)) %>%
  mutate(year = DATE + 2007) %>%
  rename(Population = value, Age = AGEGROUP, Sex = SEX) %>%
  select(-c(NAME,DATE)) %>%
  filter(Sex != "Both sexes",
         !Age %in% all_of(PEP_remove))

# il_2010_proc <- il_2010_2019 %>%
#   filter(GEOID %in% cmapgeo::county_fips_codes$cmap) %>%
#   filter(DATE %in% names(PEP_YEARS)) %>%
#   mutate(year = DATE + 2007) %>%
#   rename(Population = value, Age = AGEGROUP, Sex = SEX) %>%
#   select(-c(NAME,DATE)) %>%
#   filter(Sex != "Both sexes",
#          !Age %in% all_of(PEP_remove))
#
# il_2020_proc <- il_2020_2022 %>%
#   filter(GEOID %in% cmapgeo::county_fips_codes$cmap,
#          AGEGROUP != "All ages") %>%
#   rename(Population = value, Age = AGEGROUP, Sex = SEX) %>%
#   select(-NAME)

all_data <- rbind(proc_2010,proc_2020)

#total pop
#crude workforce

total_pop <- all_data %>%
  group_by(year, region) %>%
  mutate(total_pop = sum(Population)) %>%
  distinct(year, total_pop)


wf_pop <- all_data %>%
  filter(!Age %in% c("Age 0 to 4 years","Age 5 to 9 years","Age 10 to 14 years","Age 15 to 19 years",
                     "Age 65 to 69 years","Age 70 to 74 years","Age 75 to 79 years","Age 80 to 84 years","Age 85 years and older")) %>%
  group_by(year, region) %>%
  mutate(crude_workforce = sum(Population)) %>%
  distinct(year, crude_workforce)

long_combo <- total_pop %>%
  left_join(wf_pop)

wide_combo <- long_combo %>%
  pivot_wider(values_from = c("total_pop","crude_workforce"),
              names_from = region)

#adapted from workforce.r
load("Output/GQData2.Rdata") # named GQratios
kiddos <- c("0 to 4 years", "5 to 9 years", "10 to 14 years")

LFPRs <- read.csv("Input/LFPRs.csv")

unemp <- read.csv("Input/unemploymentrates.csv") %>%
  mutate(Year = as.character(Year))

load("Output/POP_PEP.Rdata")  # named POP

#create POPrecent list
POPrecent <- list()
POPrecent[["2010"]] <- POP[["2010"]]
POPrecent[["2015"]] <- POP[["2015"]]
POPrecent[["2020"]] <- POP[["2020"]]
for(i in names(POPrecent)){
  POPrecent[[i]] <- POPrecent[[i]] %>%
    group_by(Region, Sex, Age) %>%
    summarize(Population = sum(Population), .groups = "drop")
}


#combine the two lists created above into one list
laborforce <- POPrecent

#slight reformat to GQratios (sum partial GQ ratios to get Total GQ ratio)
GQratios2 <- GQratios %>%
  mutate_at(c(3:9), ~replace_na(.,0)) %>%
  mutate(GQratio = GQ_NonInst_College + GQ_Inst_Corr + GQ_Inst_Juv + GQ_Inst_Nurs + GQ_Inst_Other + GQ_NonInst_Other)

#join the GQ ratios to the population for every year
for(i in names(laborforce)){
  laborforce[[i]] <- laborforce[[i]] %>%
    mutate(Age = case_when(Age == "85 years and older" ~ "85 years and over",
                           T ~ Age)) %>%
    left_join(GQratios2, by=c("Region","Age","Sex")) %>%
    mutate(GQpop = Population * GQratio,
           NonGQpop = round(Population - GQpop,0)) %>%
    filter(!Age %in% kiddos)
}

#join the LFPRs to each of the tables in laborforce
laborforce[["2010"]] <- laborforce[["2010"]] %>% left_join(LFPRs[c(1:2,3)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2010)
laborforce[["2015"]] <- laborforce[["2015"]] %>% left_join(LFPRs[c(1:2,4)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2015)
laborforce[["2020"]] <- laborforce[["2020"]] %>% left_join(LFPRs[c(1:2,7)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2020)

#multiply NonGQpop * LFPR for each year
for(i in names(laborforce)){
  laborforce[[i]] <-  laborforce[[i]] %>%
    ungroup() %>%
    mutate(laborforce = NonGQpop * (LFPR/100))
}

#condense laborforce list into single tibble
workers <- tibble()
i=1
for(item in laborforce){
  #print(item)
  temp <- item
  temp$year <- names(laborforce)[i]
  workers <- bind_rows(workers, temp)
  i <- i + 1
}

#total up labor force, join with unemployment rate, calculate number of workers
workers <- workers %>%
  #select(-starts_with("LFPR")) %>% #filter out to select an employment forecast
  group_by(Region, year) %>%
  summarize(totlaborforce = sum(laborforce), .groups = "drop") %>%
  left_join(unemp, by=c("year" = "Year")) %>%
  mutate(workers = totlaborforce * (1 - (Unemployment.Rate / 100))) %>%
  select(-Unemployment.Rate) %>%
  mutate(workers = round(workers * 1.00113,0), # constant to account for jobs held by people commuting from out of region (Berger's #)
         year = as.numeric(year))

workers_reshape <- workers %>%
  pivot_wider(values_from = c(totlaborforce,workers),
              names_from = c(Region))

all_wf_data <- wide_combo %>%
  left_join(workers_reshape)
