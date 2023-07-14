# CMAP | Mary Weber, Alexis McAdams, Alex Bahls | 7/14/2023

#why dont we pull PC07?
#getting diff nubmer of results, confirm why

##### GroupQuarters.R

# This script fetches detailed 2010 Group Quarters populations and calculates ratios of
# GQ type to Pop for each Age group and Sex by Region (with the exception of Military).
#

library(tidyverse)
library(tidycensus)

# Set parameters ----------------------------------------------------------

load("Output/POP_PEP.Rdata") # POP

# import helpers
# load("Output/importhelpers.Rdata") # COUNTIES and CMAP_GEOIDS, see setup_control.R for details

GQ_TABLES <- c("PCO10", "PCO9", "PCO8", "PCO6", "PCO5", "PCO4", "PCO3")

YEAR <- 2020

# Step 1: fetch historical GQ data ------------------------------------------

# Compile list of variables to download
DHC_VARS <- load_variables(YEAR, "dhc")
GQ_VARS <- DHC_VARS %>%
  filter(str_starts(name, paste0("^", paste(GQ_TABLES, collapse="|^")))) %>%
  mutate(
    Category = str_replace_all(label, ".*\\)!!", ""),
    Category = str_replace_all(Category, "!!", " ")
  )

# Download census GQ data for selected variables in all counties
GQ_DATA <- tibble()
for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = GQ_VARS$name,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = YEAR, sumfile = "dhc", cache_table = TRUE)
  GQ_DATA <- bind_rows(GQ_DATA, TEMP)
}

# Assemble final table of GQ population data, rename cols and reformat values
GQ <- GQ_DATA %>%
  left_join(GQ_VARS, by = c("variable" = "name")) %>%
  select(-label) %>%
  rename(Concept = concept,
         Value = value,
         Variable = variable) %>%
  separate_wider_delim(NAME, delim = ", ",names = c("County", "State")) %>%
  separate_wider_delim(Category, delim = ":", names = c(NA,"Sex","Age") ,too_few = "align_end") %>%  #might want to remove the last command
  mutate(Age = case_when(Age == "" ~ "Total",
                         T ~ str_trim(Age)),
         Sex = case_when(str_detect(Sex,"Total") ~ "Total",
                         T ~ str_trim(Sex)),
    Year = YEAR,
    Category = case_when(Age == "Total" & Sex == "Total" ~ "County Total",
                         Age == "Total" & Sex == "Male" ~ "County Male Total",
                         Age == "Total" & Sex == "Female"  ~ "County Female Total",
                         TRUE ~ paste0(Sex," ",Age)),
    Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                       State == "Illinois" ~ "External IL",
                       State == "Indiana" ~ "External IN",
                       State == "Wisconsin" ~ "External WI")
  )

# Adds columns for Sex and Age, but retain Category column
GQ <- GQ %>%
  separate(Category, into=c("Sex", "Age"), sep = " ", extra = "merge", remove = FALSE) %>%
  mutate(
    Sex = case_when(Sex == "County" & str_starts(Category, "County Male") ~ "Male",
                    Sex == "County" & str_starts(Category, "County Female") ~ "Female",
                    Sex =="County" ~ "All",
                    TRUE ~ Sex))

# Remove the rows for Population Totals, sum up the GQ populations by GQ type age and sex, fix 0-4 Age value

new_age_groups <- c("Under 20 years","Under 25 years","25 years and over", "65 years and over")

GQlong <- GQ %>% filter(Sex != "All") %>% filter(!Age %in% c('Male Total', 'Female Total')) %>%
  group_by(Region, Sex, Age, Concept) %>%
  summarize(Population = sum(Value)) %>%
  mutate(Age = case_when(Age == "Under 5 years" ~ "0 to 4 years",
                         TRUE ~ Age),
         Age = gsub("\u00A0", " ", Age)) %>% #somehow a utf charecter space got in; I blame the API for now
  filter(!Age %in% new_age_groups) %>%
  ungroup()


# Step 2: calculate GQ Ratios ------------------------------------------

# import the 2010 Census Population (these values are the NON GQ population)
pop2020 <- POP[["2020"]] %>%
  group_by(Region, Age, Sex) %>%
  summarize(nonGQpop = sum(Population))

# Join the GQ sums to 2020 population, calculate the GQ ratios for every GQ type
# GQ ratio = GQ pop / (GQ pop + nonGQ pop)
GQratios <- left_join(GQlong, pop2020, by=c("Region", "Age", "Sex")) %>%
  mutate(ratio = Population / (Population + nonGQpop)) %>%
  mutate(Concept = word(Concept, start = 5, end = 6 )) %>% #shorten the Concept col values (types of GQs)
  select(-Population, -nonGQpop) %>%
  pivot_wider(names_from = Concept, values_from = ratio)
names(GQratios) <- make.names(names(GQratios)) #fix column names

# Clean up names, remove Military population ratio
GQratios <- GQratios %>%
  rename(GQ_Inst_Corr = CORRECTIONAL.FACILITIES,
         GQ_Inst_Juv = JUVENILE.FACILITIES,
         GQ_Inst_Nurs = NURSING.FACILITIES.SKILLED.NURSING,
         GQ_Inst_Other = OTHER.INSTITUTIONAL,
         GQ_NonInst_College = COLLEGE.UNIVERSITY.STUDENT,
         GQ_NonInst_Other = OTHER.NONINSTITUTIONAL) %>%
  select(-MILITARY.QUARTERS)

# split out Military population total by age and sex - this value is held constant in future projections -- note that everywhere outside of CMAP region has been 0 mliitary in past few analyses
GQ_Military <- GQ %>%
  filter(Concept == "GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE") %>%
  filter(Sex != "All") %>% filter(!Age %in% c('Male Total', 'Female Total')) %>%
  mutate(Age = case_when(Age == "Under 5 years" ~ "0 to 4 years",
                       TRUE ~ Age)) %>%
  group_by(Region, Sex, Age) %>%
  mutate(Value = sum(Value)) %>%
  select(Value, Sex, Age, Region) %>%
  #filter(!Age %in% c('0 to 4 years', '5 to 9 years', '10 to 14 years')) %>% # Decided to keep these age groups in for consistency (Alexis note)
  unique() %>%
  ungroup()

save(GQ, GQ_Military, GQratios, file="Output/GQData2.Rdata")


