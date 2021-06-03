# CMAP | Mary Weber | 6/3/2021


library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)


# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
Mort_YEARS <- c(2014:2018)

MORT_DATA <- tibble()

for (YEAR in Mort_YEARS) {
  Mort_Pop <- POP[[as.character(YEAR)]]
  MORT_DATA <- bind_rows(MORT_DATA, Mort_Pop)
}


MORT_DATA$GEOID <- as.double(MORT_DATA$GEOID)
Deaths <- read_excel("Input/CMAPMortality1990-2019.xlsx")
Deaths <- rename(Deaths, Age=Category)
Deaths <- rename(Deaths, DeathCount=Value)
Deaths <- Deaths %>% filter(Year > 2013 & Year < 2019)

#i need to keep in the 0-4 age group to calculate population counts for 0-1 and 1-4

m <- full_join(Deaths, select(MORT_DATA, c(GEOID, Year, Age, Population)), by=c('GEOID', 'Age', 'Year')) %>%
      mutate(Population2 = case_when(Age == '0 to 1 years' ~ Population*(1/5), #age.x is 0-1 and age.y is 1-4 then multiple population by 1/5
                                   Age == '1 to 4 years' ~ Population*(4/5),
                                    TRUE ~ Population))

m <- full_join(Deaths, MORT_DATA) %>%
  mutate(Population2 = case_when(Age == '0 to 1 years' ~ Population*(1/5),
                                 Age == '1 to 4 years' ~ Population*(4/5),
                                 TRUE ~ Population))
View(m)



#create life table
