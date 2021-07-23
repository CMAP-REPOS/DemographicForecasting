# CMAP | Alexis McAdams, Mary Weber | 7/12/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

source("src/Mortality.R")

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
load("Output/ASFR.Rdata")
load("Output/LifeTables.Rdata")

MIG_YEARS <- c(2013:2014, 2018:2019)

# Population by 5-Year Age Group, Sex and Region; 2013-14 and 2018-19 Averaged  ---------------------------------------------------------

MIG_POP <- tibble()

for (YEAR in MIG_YEARS) {
  MIG_POP <- bind_rows(MIG_POP, POP[[as.character(YEAR)]])
}

MIG_POP <- MIG_POP %>% select(-GEOID, -State) %>% group_by(Region, County, Year, Age, Sex) %>%
                              summarise(Population = sum(Population), .groups="drop") %>% filter(Region == 'External WI') %>% #remember to remove this filter
                              mutate(Year2 = case_when(Year %in% 2013:2014  ~ 2014,
                                                       Year %in% 2018:2019 ~ 2019)) %>% select(-Year)

MIG_POP <- MIG_POP %>% group_by(Age, Sex, Region, County, Year2) %>% mutate(Pop_Avg = case_when(Year2 == 2014 ~ round(mean(Population),0),
                                                                                       Year2 == 2019 ~ round(mean(Population),0))) %>%
                                ungroup() %>% select(Region, County, Age, Sex, Year2, Pop_Avg) %>% distinct(Age, Sex, Region, County, Year2, .keep_all = TRUE) %>%
                                rename(Year = Year2)
View(MIG_POP)


write.csv(MIG_POP, "/Users/mweber/Desktop/mig_pop.csv")



# Births by Region 2014-2018 ---------------------------------------------------------
Births <- read_excel("Input/Births_CountyGender.xlsx") %>%
  filter(Year %in% 2014:2018)


# Abridged Life Tables (do not separate 0-4 age group) ---------------------------------------------------------

Deaths_Abg <- Deaths %>% mutate(Age =  case_when(Age %in% c('0 to 1 years', '1 to 4 years') ~ '0 to 4 years',
                                                  TRUE ~ Age)) %>%
              group_by(GEOID, Sex, Age, Year, Region) %>%
              summarize(Mortality = sum(Mortality), .groups = "drop") %>%
              drop_na()

#View(Deaths_Abg)

LT_Abg <- tibble(Age = unique(Deaths$Age)) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>%
  add_column(Ax = c(0.34,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))

a_abg <- MORT_DATA %>%
  left_join(LT, by="Age") %>%
  select(Region, Sex, Age, Mortality, Population, x, Ax) %>%
  arrange(Region, desc(Sex), x) %>%
  group_by(Region, Sex) %>%
  mutate(Mx = (Mortality/Population),
         n = case_when(Age == '85 years and over' ~ 2/Mx,
                       TRUE ~ 5),
         Qx = ifelse(Age == '85 years and over', 1,  # 85+ should always be 1
                     (n*Mx/(1+n*(1-Ax)*Mx))),
         Px = (1-Qx),
         Ix = head(accumulate(Px, `*`, .init=500000), -1), # 0-4 should always be 100000
         Dx = (ifelse(Age == '85 years and over', Ix, Ix -lead(Ix))),
         Lx = (ifelse(Age == '85 years and over', Ix/Mx, n*(lead(Ix)+(Ax*Dx)))),
         temp = ifelse(Age == '85 years and over', Lx, 0),
         Tx = (accumulate(Lx, `+`, .dir = "backward")),
         Ex = (Tx/Ix),
         Sx = case_when(Age == '0 to 4 years' ~ Lx/Ix,
                        Age == '85 years and over' ~ Lx/(Lx +lag(Lx)),
                        TRUE ~ Lx/lag(Lx))
  ) %>%
  select(-temp) %>%
  relocate(n, .after = x) %>%
  ungroup()

#View(a_abg)

#








