# CMAP | Mary Weber | 6/3/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
Deaths <- read_excel("Input/CMAPMortality1990-2019.xlsx")
MORT_YEARS <- c(2014:2018)


# Create mortality age groups ---------------------------------------------------------

MORT_DATA <- tibble()

for (YEAR in MORT_YEARS) {

   Mort_Pop <- POP[[as.character(YEAR)]]
   MORT_DATA <- bind_rows(MORT_DATA, Mort_Pop)
}

MORT_DATA$GEOID <- as.double(MORT_DATA$GEOID)


Deaths <- Deaths %>% filter(Year > 2013 & Year < 2019) %>%
                     mutate(Age = case_when(Age %in% c("85 to 89 years", "90 to 94 years", "95 years and over") ~ "85 years and over",
                                                        TRUE ~ Age)) %>%
                    group_by(GEOID, County, State, Sex, Age, Year, Region) %>%
                    summarize(Mortality = sum(Mortality), .groups = "drop")

temp1 <- MORT_DATA %>%
  filter(Age == '0 to 4 years') %>%
  mutate(Age = '0 to 1 years',
         Population = Population * 1/5)

temp2 <- MORT_DATA %>%
  filter(Age == '0 to 4 years') %>%
  mutate(Age = '1 to 4 years',
         Population = Population * 4/5)

MORT_DATA <- MORT_DATA %>%
  bind_rows(temp1) %>%
  bind_rows(temp2) %>%
  arrange(GEOID, Age, Year) %>%
  left_join(Deaths, ., by=c('GEOID', 'Age', 'Year', 'Sex', 'Region', 'State')) %>%
  select (-County.x, -County.y, -Year, -State) %>%
  group_by(Age, Sex, Region) %>%
  summarise(Mortality = sum(Mortality), Population = sum(Population)) %>%
  arrange(desc(Region), desc(Sex))

#View(MORT_DATA)
#a <- MORT_DATA %>% filter(State == 'Wisconsin' & Sex == 'Female' & Age == '85 years and over') %>%
# mutate(Population = sum(Population))
#View(a)

# Life Tables Calculations ---------------------------------------------------------

LT <- tibble(Age = unique(Deaths$Age))

LT <- LT %>% add_column(x = c(0,1,5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)) %>%
  add_column(n = c(1,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,12)) %>%
  add_column(Ax = c(0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))

a <- left_join(MORT_DATA, LT, by=c("Age" = "Age")) %>% select(Age, x, n, Ax, Sex, Region, Mortality, Population)
View(a)

a <- a %>% add_column(Mx = Deaths/Population)

%>%
  add_column(n = c(1,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,12)) %>%
  add_column(Ax = c(0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))


