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


m <- left_join(Deaths, select(Mort_Pop, c(GEOID, Age, Population, Year)), by=c('GEOID', 'Year', 'Age'))

View(m)


#0-1 is 1/5 population of 0-4
#1-4 is 4/5 population of 0-4



#create life table
