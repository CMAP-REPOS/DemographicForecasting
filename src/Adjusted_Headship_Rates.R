# CMAP | Mary Weber | 8/18/2021

library(tidyverse)
library(tidycensus)
library(readxl)

# This code calculates adjusted headship rates to use for head of household
# projections 2025-2060


# Parameters ---------------------------------------------------------

load("Output/Migration_Projections.Rdata") #Mig_Proj
load("Output/PopData.Rdata") #POP
load("Output/GQData.Rdata") #GQratios
load("Output/PUMS_HeadshipRates.Rdata") #HEADSHIP_RATES

#lines 15-22 are duplicate from PEP code....should consolidate
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)


HH_Year = 2019

# Extract 2019 household totals estimates
household_totals <- tibble()


for (STATE in names(COUNTIES)) {
  #use variables argument
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

HH_total <- household_totals %>% group_by(Region) %>% summarise(Households = sum(Households))

# Extract 2019 population estimates
Base_Year <- POP[[as.character(HH_Year)]] %>% group_by(Region, Sex, Age) %>% mutate(Population = sum(Population)) %>%
  select(-County, -State, -GEOID) %>% unique()

Base_Year  <- Base_Year  %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>% select(-x) %>% arrange(Region, desc(Sex))

# Load GQ ratios
GQ_Ratios <- GQratios

Headship_Rates <- full_join(Base_Year, GQ_Ratios, by=c("Sex", "Age", "Region")) %>%
  mutate(GQ_Estimates = round(Population*GQratio,0)) %>%
  mutate(HH_Pop = Population-GQ_Estimates)

# Pull in 2019 PUMS headship rate calculations
Headship_Rates <- Headship_Rates %>% right_join(HEADSHIP_RATES, by=c("Age", "Region"))

#only need for base year
Headship_Rates <- Headship_Rates %>% group_by(Age, Region) %>% pivot_wider(names_from = "Sex", values_from=c("Population", "GQratio", "GQ_Estimates", "HH_Pop", "HeadshipRate")) %>%
  rename(Headship_Rate = HeadshipRate_Male) %>% select(-HeadshipRate_Female) %>%
  mutate(Head_HH = round((HH_Pop_Male*Headship_Rate)+(HH_Pop_Female*Headship_Rate),0)) %>%
  left_join(HH_total, by='Region')

Head_HH_Total <- sum(Headship_Rates$Head_HH)

Head_of_HH <- Headship_Rates %>% mutate(Head_HH_Adjust = round((Head_HH/Head_HH_Total)*Households,0)) %>%
  mutate(Ratio_Adj = Head_HH_Adjust/(HH_Pop_Male + HH_Pop_Female))

#save(Head_of_HH, file="Output/Head_of_HH.Rdata")
