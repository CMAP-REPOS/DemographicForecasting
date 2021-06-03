# CMAP | Mary Weber | 6/3/2021


library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)


# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
Mort_YEARS <- c(2014:2018)


for (YEAR in Mort_YEARS) {
  Mort_Pop <- POP[[as.character(YEAR)]]
}

Mort_Pop$GEOID <- as.double(Mort_Pop$GEOID)
Deaths <- read_excel("Input/CMAPMortality1990-2019.xlsx")
Deaths <- rename(Deaths, Age=Category)
Deaths <- rename(Deaths, DeathCount=Value)
Deaths <- Deaths %>% filter(Year > 2013 & Year < 2019)


m <- left_join(Deaths, Mort_Pop) %>% #i need population in here
      mutate(Population = case_when(Age == '0 to 1 years' ~ DeathCount*(1/5), #this should be population
                                   Age == '1 to 4 years' ~ DeathCount*(4/5),
                                    TRUE ~ DeathCount))


View(m)



#create life table
