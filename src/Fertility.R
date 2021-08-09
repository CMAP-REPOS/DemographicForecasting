# CMAP | Alexis McAdams, Mary Weber | 7/7/2021

library(tidyverse)
library(tidycensus)
library(readxl)

load("Output/PopData.Rdata")
load("Output/GQData.Rdata")


# Set parameters ----------------------------------------------------------

GQE_YEARS <- c(2011:2019) #GQ estimate year range
F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")
BASE_YEAR <- 2014 #Base year population, ASFR projections are built off of this year because it is midpoint of our 2010-2018 data

# Remove estimates of females in GQ from each year ----------------------------

# Step 1: Calculate 2010 GQ totals by County, excluding military
GQ_totals_2010 <- GQ %>%
  filter(Category == 'County Total')  %>%
  filter(Concept != 'GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE') %>%
  group_by(GEOID, County, State, Year, Region, Age, Sex) %>%
  summarise(County_GQtotal = sum(Value), .groups = "drop") %>%
  select(GEOID, County, County_GQtotal)

# Step 2: Calculate 2010 Female GQ totals by County, Sex, Age, excluding military
FGQ_2010 <- GQ %>%
  filter(Sex == 'Female')  %>%
  filter(Concept != 'GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE') %>%
  filter(Age %in% F_Groups) %>%
  group_by(GEOID, County, State, Year, Region, Age, Sex) %>%
  summarise(Female_GQ = sum(Value), .groups = "drop")

# Step 3: For each GQ female age group of interest, calculate what proportion of each county's total 2010 GQ population they represent
GQ2010 <- left_join(FGQ_2010, GQ_totals_2010) %>% mutate(Prop_Female = Female_GQ/County_GQtotal)

# Step 4: Read in GQ Census estimates by State, County for 2011-2019
GQE <- read_excel("Input/GQE.xlsx")
GQE$GEOID <- as.character(GQE$GEOID)

# Step 5: Multiply 2010 proportions by 2011-2019 GQ estimates to get expected number of females
GQE_projec <- left_join(GQ2010, select(GQE, c(GEOID, Year, Population)), by='GEOID') %>%
  mutate(GQE_pred = round(Prop_Female*Population),0) %>%
  rename(Year = Year.y) %>%
  select(GEOID, County, State, Sex, Age, GQE_pred, Year,Region)

# Step 6: Female HH population estimates by County, Age and Sex for 2011-2019
TEMP_DATA <- tibble()
for (YEAR in GQE_YEARS) {
  Pop_Est <- POP[[as.character(YEAR)]] %>%
  filter(Sex == 'Female')  %>%
  filter(Age %in% F_Groups) %>%
  group_by(GEOID, County, State, Year, Region, Age) %>%
  summarise(County_Total = sum(Population), .groups = "drop")
  TEMP_DATA <- bind_rows(TEMP_DATA, Pop_Est)
}

# Step 7: For 2011-2019 subtract expected GQ population from county Total to get County HH populations
HH_Pop <- left_join(GQE_projec, select(TEMP_DATA, c(GEOID, Year, Age, County_Total)), by=c('GEOID', 'Age', 'Year')) %>%
  mutate(Population = County_Total - GQE_pred) %>%
  select(GEOID, County, State, Sex, Year, Age, Population, Year, Region)

# Step 8: Filter 2010 Decennial Census HH Data for females 15-44
Female_HH_2010 <- POP[["2010"]]  %>%
    filter(Sex == 'Female') %>%
    filter(Age %in% F_Groups)

# Step 9: Merge 2010 Census HH Population data and 2011-2019 HH Population Estimates to form a complete table of Household Population data
F_HH_Data <- bind_rows(Female_HH_2010, HH_Pop)


# Birth data - add pre-age-15 births to 15-19 group, add 45+ age births to 40-44 group ------------------
Births <- read_excel("Input/CMAPBirths_1990-2019.xlsx") %>%
  filter(Year %in% (2010:2018)) %>% # filtered out incomplete years data (2019 and 2020)
  mutate(Age = case_when(Age %in% c("10 to 14 years") ~ "15 to 19 years",
                         Age %in% c("45 to 49 years") ~ "40 to 44 years",
                         TRUE ~ Age)) %>%
  group_by(GEOID, County, State, Age, Year, Region) %>%
  summarize(Births = sum(Births), .groups = "drop") %>%
  drop_na()

Births$GEOID <- as.character(Births$GEOID)


# ASFR Calculation - # of live births per 1,000 women ------------------

ASFR <- F_HH_Data %>% inner_join(Births, by = c("GEOID", "State", "Age", "Year")) %>%
  mutate(ASFR = round((Births/Population)*1000, 2)) %>%
  select(State, GEOID, County.x, Sex, Year, Age, Region.x, Births, Population, ASFR) %>%
  rename(County = County.x) %>%
  rename(Region = Region.x)

# Combine population and summed birth data to generate base year ASFRs by region
BaseYearPop <- F_HH_Data %>%
  filter(Year %in% 2010:2018) %>% #filter out 2019 because we don't have 2019 births for all regions
  filter(Year == BASE_YEAR) %>%
  group_by(Age,Region)%>%
  summarise(Population = sum(Population),
            .groups="drop")

BaseYearASFR <- Births %>%
  group_by(Age,Region)%>%
  summarise(Births=sum(Births),
            .groups="drop") %>%
  left_join(BaseYearPop, by=c("Age","Region")) %>%
  mutate(baseASFR = Births/Population/9) %>% #9 is the number of years of data we have (2010-2018)
  select(Age, Region, baseASFR)

#check TFR
#BaseYearASFR %>% group_by(Region)%>% summarise(TFR = sum(baseASFR)*5)

# Import 2014 ASFR projections data from Census Bureau
#   Note: could try using the package censusapi to import directly
CensusASFRs <- read.csv("Input/projectedbirths_Census2014.csv", header=TRUE) %>%
  filter(group == "0") %>% #keep only the total ASFRs (otherwise divided by race + ethnicity)
  select(!group) %>%
  pivot_longer(!year, names_to = "age", values_to="ASFR")

# Calculate national projected ASFRs for each age group and each year
#  Exceptions: 14 included in 15-19, and 45 included in 40-44
CensusASFRs <- CensusASFRs %>%
  mutate(age = as.integer(str_sub(age, -2))) %>%
  mutate(agegroup = case_when(age %in% 14:19 ~ "15 to 19 years",
                             age %in% 20:24 ~ "20 to 24 years",
                             age %in% 25:29 ~ "25 to 29 years",
                             age %in% 30:34 ~ "30 to 34 years",
                             age %in% 35:39 ~ "35 to 39 years",
                             age %in% 40:54 ~ "40 to 44 years")) %>%
  drop_na() %>% #remove the projections for >45
  group_by(year, agegroup) %>%
  summarise(natlASFR = sum(ASFR)/5) #average the ASFRs for each age group

#Pull out just the projected ASFRs for the Base Year
CensusBaseYear <- CensusASFRs %>%
  filter(year==BASE_YEAR) %>%
  rename(baseASFR = natlASFR) %>%
  ungroup() %>%
  select(-year)

#Join the Census Base Year ASFR values to the projected ASFR values
#and calculate the ratio (projected ASFR / base year ASFR)
CensusASFRs <- left_join(CensusASFRs, CensusBaseYear, by = "agegroup") %>%
  ungroup() %>%
  mutate(CensusRatio = natlASFR / baseASFR) %>%
  select(year, agegroup, CensusRatio)

#Apply the Census ratio to our region's base year ASFRs
ASFR_projections <- full_join(CensusASFRs, BaseYearASFR, by = c("agegroup" = "Age")) %>%
  mutate(ASFR_proj = CensusRatio * baseASFR) %>%
  rename(Age = agegroup, Year = year) %>%
  select(Region, Age, Year, ASFR_proj) %>%
  filter(Year %% 5 == 0) #select just the 5-year ASFRs

#export the ASFR projections
save(ASFR_projections, file="Output/ASFR.Rdata")

###########------------ calculate sex by births ratios for each region

#Import births by sex data (2014-2018)
bdata <- read_excel("Input/Births_CountyGender.xlsx") %>% filter(Year %in% 2014:2018)

#calculate totals by region, year and sex
btemp <- bdata %>%
  group_by(Year, Region, Sex) %>%
  summarise(Births = sum(Births))

#calculate totals by region and year
btemp2 <- bdata %>%
  group_by(Year, Region) %>%
  summarise(totBirths = sum(Births))

#calculate the ratios by year, region and sex, and take average across all years
bRatios <- left_join(btemp, btemp2, by = c("Year", "Region")) %>%
  mutate(bRatio = Births / totBirths) %>%
  group_by(Region, Sex) %>%
  summarize(avgRatio = mean(bRatio)) %>%
  pivot_wider(names_from = "Sex", values_from = "avgRatio")
rm(btemp)
rm(btemp2)
rm(bdata)
save(bRatios, file="Output/BirthRatios.Rdata")

