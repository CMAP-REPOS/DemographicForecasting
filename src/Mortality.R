# CMAP | Mary Weber | 6/3/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
MORT_YEARS <- c(2014:2018)
DEATHS_XLSX <- "Input/CMAPMortality1990-2019.xlsx"


# Create mortality age groups ---------------------------------------------------------

# Load population data
MORT_POP <- tibble()

for (YEAR in MORT_YEARS) {
   MORT_POP <- bind_rows(MORT_POP, POP[[as.character(YEAR)]])
}

# Split age 0-4 into 0-1 & 1-4
temp1 <- MORT_POP %>%
  filter(Age == '0 to 4 years') %>%
  mutate(Age = '0 to 1 years',
         Population = Population * 1/5)

MORT_POP <- MORT_POP %>%
  mutate(Age = case_when(Age == '0 to 4 years' ~ '1 to 4 years',
                         TRUE ~ Age),
         Population = case_when(Age == '1 to 4 years' ~ Population * (4/5),
                                TRUE ~ Population)) %>%
  bind_rows(temp1) %>%
  arrange(GEOID, Sex, Age, Year) %>%
  select (-County, -State)

rm(temp1)

# Load deaths data
Deaths <- read_excel(DEATHS_XLSX) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  filter(Year >= min(MORT_YEARS) & Year <= max(MORT_YEARS)) %>%
  mutate(Age = case_when(Age %in% c("85 to 89 years", "90 to 94 years", "95 years and over") ~ "85 years and over",
                         TRUE ~ Age)) %>%
  group_by(GEOID, Sex, Age, Year, Region) %>%
  summarize(Mortality = sum(Mortality), .groups = "drop")

# Join pop to deaths
MORT_DATA <- Deaths %>%
  left_join(MORT_POP, by=c('GEOID', 'Age', 'Year', 'Sex', 'Region')) %>%
  group_by(Age, Sex, Region) %>%
  summarise(Mortality = sum(Mortality),
            Population = sum(Population),
            .groups = "drop") %>%
  arrange(Region, desc(Sex))

#View(MORT_DATA)
#a <- MORT_DATA %>% filter(State == 'Wisconsin' & Sex == 'Female' & Age == '85 years and over') %>%
#  mutate(Population = sum(Population))
#View(a)

# Life Tables Calculations ---------------------------------------------------------

LT <- tibble(Age = unique(Deaths$Age)) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>%
  add_column(n = c(1,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,12)) %>%
  add_column(Ax = c(0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))

a <- MORT_DATA %>%
  left_join(LT, by="Age") %>%
  select(Region, Sex, Age, Mortality, Population, x, n, Ax) %>%
  arrange(Region, desc(Sex), x) %>%
  group_by(Region, Sex) %>%
  mutate(
    Mx = round(Mortality/Population, digits = 6),
    Qx = ifelse(Age == '85 years and over', 1,  # 85+ should always be 1
                round(n*Mx/(1+n*(1-Ax)*Mx), digits=6)),
    Px = round(1-Qx, digits = 6),
    Ix = round(head(accumulate(Px, `*`, .init=100000), -1),0), # 0-1 should always be 100000
    Dx = (Ix -lead(Ix)),
    Lx = round((n*(lead(Ix)+(Ax*Dx))),0)
  ) %>%
  ungroup()

View(a)
