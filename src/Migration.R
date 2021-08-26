# CMAP | Alexis McAdams, Mary Weber | 7/12/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

source("src/Mortality.R")

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata") #POP
load("Output/ASFR.Rdata") #ASFR_projections
load("Output/LifeTables.Rdata") #LifeTable

MIG_YEARS <- c(2013:2014, 2018:2019)

# Population by 5-Year Age Group, Sex and Region; 2013-14 and 2018-19 Averaged  ---------------------------------------------------------


MIG_POP <- tibble()

for (YEAR in MIG_YEARS) {
  MIG_POP <- bind_rows(MIG_POP, POP[[as.character(YEAR)]])
}

MIG_POP <- MIG_POP %>% select(-GEOID, -State) %>% group_by(Region, Year, Age, Sex) %>%
                              summarise(Population = sum(Population), .groups="drop") %>% #remember to remove this filter
                              mutate(Year2 = case_when(Year %in% 2013:2014  ~ 2014,
                                                       Year %in% 2018:2019 ~ 2018)) %>% select(-Year)

MIG_POP <- MIG_POP %>% group_by(Age, Sex, Region, Year2) %>% mutate(Pop_Avg = case_when(Year2 == 2014 ~ round(mean(Population),0),
                                                                                       Year2 == 2018 ~ round(mean(Population),0))) %>%
                                ungroup() %>% select(Region, Age, Sex, Year2, Pop_Avg) %>% distinct(Age, Sex, Region, Year2, .keep_all = TRUE) %>%
                                rename(Year = Year2) %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>% select(-x)


# Births by Region 2014-2018 ---------------------------------------------------------


Births <- read_excel("Input/Births_CountyGender.xlsx") %>% filter(Year %in% 2014:2018) %>% select(-County, -State, -Year)

Births <- Births %>% group_by(Region, Sex) %>% summarise(Births = sum(Births)) %>%
          add_column(Age = '0 to 4 years')


# Abridged Life Tables (do not separate 0-4 age group) ---------------------------------------------------------


Deaths_Abg <- Deaths %>% mutate(Age =  case_when(Age %in% c('0 to 1 years', '1 to 4 years') ~ '0 to 4 years',
                                                  TRUE ~ Age)) %>%
              group_by(GEOID, Sex, Age, Year, Region) %>%
              summarize(Mortality = sum(Mortality), .groups = "drop") %>%
              drop_na()

LT_Age <- tibble(Age = unique(Deaths_Abg$Age)) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>%
  add_column(Ax = c(0.34,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))


# Join pop to abridged deaths data

MORT_DATA2 <- MORT_POP %>%
  full_join(Deaths_Abg, by=c('GEOID', 'Age', 'Sex', 'Year', 'Region')) %>%
  group_by(Age, Sex, Region) %>%
  summarise(Population = sum(Population),
            Mortality = sum(Mortality),
            .groups = "drop")

LT_Abg <- MORT_DATA2 %>%
  left_join(LT_Age, by="Age") %>%
  select(Region, Sex, Age, Mortality, Population, x, Ax) %>%
  arrange(Region, desc(Sex), x) %>%
  group_by(Region, Sex) %>%
  mutate(Mx = (Mortality/Population),
         n = case_when(Age == '85 years and over' ~ 2/Mx,
                       TRUE ~ 5),
         Qx = ifelse(Age == '85 years and over', 1,  # 85+ should always be 1
                     (n*Mx/(1+n*(1-Ax)*Mx))),
         Px = (1-Qx),
         Ix = head(accumulate(Px, `*`, .init=500000), -1), # 0-4 should always be 500000
         Dx = (ifelse(Age == '85 years and over', Ix, Ix -lead(Ix))),
         Lx = (ifelse(Age == '85 years and over', n*((Ax*Dx))/5, n*(lead(Ix)+(Ax*Dx))/5)),
         temp = ifelse(Age == '85 years and over', Lx, 0),
         Tx = (ifelse(Age == '85 years and over', Lx, accumulate(Lx, `+`, .dir = "backward"))),
         Ex = (Tx/Ix)*5,
         Sx = case_when(Age == '0 to 4 years' ~ Lx/Ix,
                        Age == '85 years and over' ~ Lx/(Lx +lag(Lx)),
                        TRUE ~ Lx/lag(Lx))
  ) %>%
  select(-temp) %>%
  relocate(n, .after = x) %>%
  ungroup()

# Base Period Migration Rates Table ---------------------------------------------------------


Base_Mig <- MIG_POP %>% filter(Year == '2014') %>% select(Region, Age, Sex, Year) %>%
  left_join(Births, by=c("Age", "Region", "Sex")) %>% arrange(Region, desc(Sex))

# Pull in 2014 population counts
Base_Mig <- Base_Mig %>% left_join(MIG_POP, by=c('Region', 'Age', 'Sex', 'Year')) %>%
                         mutate(Births = case_when(Age != c('0 to 4 years', '85 years and over') ~ lag(Pop_Avg),
                                                   Age == '85 years and over' ~ Pop_Avg+lag(Pop_Avg),
                                                   TRUE ~ Births)) %>% select(-Pop_Avg) %>% rename(Pop2014 = Births)

# Pull in Survival Rates and calculate Expected Population for 2018
Base_Mig <- Base_Mig %>% left_join(LT_Abg, by =c('Region', 'Sex', 'Age')) %>% select(Region, Age, Sex, Year, Pop2014, Sx) %>%
                                  rename(SurvivalRate = Sx) %>% mutate(ExpectedPop2018 = Pop2014*SurvivalRate) %>% select(-Year)

# Pull in 2018 population counts
Base_Mig <- MIG_POP %>% filter(Year == '2018') %>%
  left_join(Base_Mig, by=c("Age", "Region", "Sex")) %>% select(Region, Age, Sex, Pop2014, SurvivalRate, ExpectedPop2018, Pop_Avg) %>% rename(PopActuals2018 = Pop_Avg)


# Calculate Surviving Migrants for 2018 & Net Migration rates, 2014-2018
Base_Mig <- Base_Mig %>% mutate(SurvMigrants2018 = (PopActuals2018 - ExpectedPop2018), NetRates = SurvMigrants2018/ExpectedPop2018)


#save(Base_Mig, file="Output/Base_Migration.Rdata")

write.csv(Base_Mig, "/Users/mweber/Desktop/BaseMigRates.csv")
