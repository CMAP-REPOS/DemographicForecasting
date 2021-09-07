# CMAP | Mary Weber | 8/18/2021

library(tidyverse)
library(tidycensus)
library(readxl)

# This code calculates adjusted headship rates to use for head of household
# projections 2025-2060.

# Parameters ---------------------------------------------------------

load("Output/Migration_Projections.Rdata") #Mig_Proj
load("Output/PopData.Rdata") #POP
load("Output/GQData2.Rdata") #GQratios
load("Output/PUMS_HeadshipRates.Rdata") #HEADSHIP_RATES from PUMS data

# Lines 15-22 are duplicate from PEP code....should consolidate
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

HH_Year = 2019 #base year

# Extract 2019 household totals from 2019 ACS
household_totals <- tibble()

for (STATE in names(COUNTIES)) {
  household_temp <- get_acs(geography = "county", table="B25002", year = HH_Year, county = COUNTIES[[STATE]], state = STATE, survey='acs1',
                            breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE, output='wide') %>%
    rename(Households = B25002_001E) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                              State == "Illinois" ~ "External IL",
                              State == "Indiana" ~ "External IN",
                              State == "Wisconsin" ~ "External WI")) %>%
    select(County, State, Households, Region)

  household_totals <- bind_rows(household_totals, household_temp)

}

HH_total <- household_totals %>% group_by(Region) %>% summarise(Households_2019PUMS = sum(Households))

# Pull and reformat base year population data (these numbers INCLUDE GQ population)
Base_Year <- POP[[as.character(HH_Year)]] %>%
  group_by(Region, Sex, Age) %>%
  summarize(Population = sum(Population)) %>%
  ungroup()

#sort the PEP data
Base_Year  <- Base_Year  %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>% select(-x) %>% arrange(Region, desc(Sex))

# Join base year population data and GQ ratios, apply the ratios to the population, and sum by category (Institutional and Non-Institutional)
# with the EXCEPTION of Military
GQ_Pop <- full_join(Base_Year, GQratios, by=c("Sex", "Age", "Region")) %>%
  mutate(across(starts_with("GQ"), ~.*Population)) %>% #multiply every GQ column by Population
  left_join(GQ_Military, by=c("Region", "Sex", "Age")) %>% rename(GQ_NonInst_Military = Value) %>% #join the Military values
  rowwise() %>%
  mutate(Inst_GQ = round(sum(across(starts_with("GQ_Inst"))), 0),
         nonInst_GQ = round(sum(across(starts_with("GQ_NonInst"))),0) ) %>%
  mutate(totalGQ = sum(across(ends_with("GQ"))))

#subtract the GQ populations from the Total population. Result is Household population
HouseholdPop <- GQ_Pop %>% select(Age, Sex, Region, Population, totalGQ) %>%
  mutate(HH_Pop = Population - totalGQ)

#Pivot table so Sex is on same row for each Region and Age group, then join HEADSHIP_RATES (from 2019 PUMS data)
Head_of_HH <- HouseholdPop %>%
  select(-totalGQ) %>%
  group_by(Age, Region) %>%
  pivot_wider(names_from = "Sex", values_from=c("Population", "HH_Pop")) %>%
  left_join(HEADSHIP_RATES, by=c("Age", "Region")) %>%
  mutate(HeadshipRate = case_when(is.na(HeadshipRate) ~ 0.0,
                                  TRUE ~ HeadshipRate)) %>%
  mutate(Head_HH = round((HH_Pop_Male*HeadshipRate)+(HH_Pop_Female*HeadshipRate),0))#HEADSHIP_RATES not available at Sex level, so it is applied to sum of Male and Female HH Pop

HH_sumtotal <- Head_of_HH %>% # sum up the number of Heads (aka number of Households) for each region, save in separate table
  group_by(Region) %>%
  summarize(HH_sumtotal = sum(Head_HH))

#join tables, calculate adjusted Headship ratio
Head_of_HH <- Head_of_HH %>%
  left_join(HH_sumtotal, by = "Region") %>% # join in summed Households and 2019 PUMS Household totals
  left_join(HH_total, by = "Region" ) %>%
  mutate(Head_HH_Adjust = round((Head_HH/HH_sumtotal)*Households_2019PUMS,0) ) %>% # calculate adjusted number of Heads of Household
  mutate(Ratio_Adj = Head_HH_Adjust/(HH_Pop_Male + HH_Pop_Female)) # calculate adjusted Headship ratio

#select only required columns
Headship <- Head_of_HH %>%
  select(Age, Region, Ratio_Adj)

#save(Headship, Head_of_HH, file="Output/Head_of_HH.Rdata")


