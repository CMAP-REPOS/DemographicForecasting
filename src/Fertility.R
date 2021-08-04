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
  mutate(baseASFR = Births/Population/9) #9 is the number of years of data we have (2010-2018)

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
  summarise(natlASFR = sum(ASFR)/5) %>%
  filter(year >= 2019) #filter out projections from 2014-2018

# Calculate ratio of BaseYearASFR to each year's projected ASFR by age group
#  Broken out by Region (?)
Fertility <- list()
for(REGION in unique(BaseYearASFR$Region)){
  Fertility[[REGION]] <- BaseYearASFR %>%
    filter(Region == REGION) %>%
    select(-Births, -Population, -Region) %>%
    left_join(x=CensusASFRs, by=c("agegroup" = "Age")) %>%
    mutate(ASFRratio = natlASFR / baseASFR) %>%
    mutate(ProjectedASFR = ASFRratio * baseASFR)%>%
    mutate(ProjectedASFRper1000 = round(ProjectedASFR*1000, 2))
}
# unList the projections
Projections <- tibble()
for(REGION in names(Fertility)){
  temp <- Fertility[[REGION]] %>%
    mutate(Region = REGION) %>%
    ungroup()
  Projections <- bind_rows(temp, Projections)
}


#----------------------------------

#Adding in weighted ASFRs by state, year and age group
ASFR <- ASFR %>% group_by(Age, State, Year, Region) %>%
  mutate(sum = sum(Population)) %>%
  mutate(weight = Population/sum) %>%
  mutate(Weighted_Avg = sum(ASFR*weight)) %>%
  select(-sum, -weight) %>%
  ungroup()

Projections <- read_xlsx("Input/ASFR_Projections.xlsx")

#multiply by 1000
Final <- bind_rows(ASFR, Projections) %>%
  mutate(Projected_ASFR = round(as.numeric(Projected_ASFR)*1000, 2)) %>%
  mutate(ASFRs = coalesce(Weighted_Avg, Projected_ASFR)) %>%
  select(-GEOID, -County, -Births, -Population, -ASFR, -Weighted_Avg, -Projected_ASFR)

Final <- distinct(Final)

#write.csv(Final, "/Users/maryweber/Desktop/ASFR2010-2060.csv")


#save(Final, file="Output/ASFR.Rdata")
#load("Output/ASFR.Rdata", "/Users/maryweber/Desktop/ASFR_HealthDep.csv")


# sample plots ---------

ASFR %>%
  filter(Year %in% c(2010:2019)) %>%
  filter(State == "Indiana") %>%
  filter(Region == 'External IN') %>% #only use for IL, change accordingly
  ggplot(aes(x=Year, y=Weighted_Avg, group=Age, color=Age)) +
  labs(y = 'Weighted ASFRs') +
  scale_x_continuous(breaks = c(2010:2019)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  ggtitle("2010-2018 External IN Weighted ASFRs") +
  geom_line() +
  geom_point()

Final %>%
  filter(Year %in% c(2010:2060)) %>%
  filter(State == "Illinois") %>%
  filter(Region == 'CMAP Region') %>% #only use for IL, change accordingly
  ggplot(aes(x=Year, y=ASFRs, group=Age, color=Age)) +
  labs(y = 'ASFRs') +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)) +
  scale_y_continuous(breaks = c(0, 15, 25, 50, 75, 100, 125, 150)) +
  ggtitle("2010-2060 IL CMAP Region ASFR Projections") +
  geom_line() +
  geom_point()



#Graphs to compare imported ASFRs (Final) and calculated ASFRs (Fertility)
  # WI region only. Calculated ASFRs removed GQ from total population

Final %>%
  filter(Year %in% c(2019:2060)) %>%
  filter(State == "Wisconsin") %>%
  ggplot(aes(x=Year, y=ASFRs, group=Age, color=Age)) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)) +
  scale_y_continuous(breaks = c(0, 15, 25, 50, 75, 100, 125, 150), limits=c(0,130)) +
  geom_line() +
  geom_point()

Fertility[[1]] %>%
  ggplot(aes(x=year, y=ProjectedASFRper1000, group=agegroup, color=agegroup)) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)) +
  scale_y_continuous(breaks = c(0, 15, 25, 50, 75, 100, 125, 150), limits=c(0,130)) +
  geom_line() +
  geom_point()

Fertility[[1]] %>%
  filter(year %% 5 == 0 ) %>%
  ggplot(aes(x=agegroup, y=ProjectedASFR, color=as.factor(year), group=year)) +
  geom_point()+
  geom_line()

