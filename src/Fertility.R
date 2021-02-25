# CMAP | Mary Weber | 2/10/2021

library(tidyverse)
library(tidycensus)
library(readxl)
load("Output/PopData.Rdata")

# Set parameters ----------------------------------------------------------

F_YEARS <- c(2010, 2015:2019) #will eventually include all years 2010 through 2020

# Filter data to only include female population within child-bearing years (15-44) ----------------------

F_DATA <- tibble()

for (YEAR in F_YEARS){
  
  TEMP_DATA <- POP[[as.character(YEAR)]] %>%
    filter(Sex == 'Female' & State == 'Indiana') %>%
    filter(Age %in% c("15 to 19 years", "20 to 24 years", "F25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years"))
    F_DATA <- bind_rows(F_DATA, TEMP_DATA)
  }

View(F_DATA)

# Birth per age cohort (add pre-age-15 births to the 15-19 group, add 45+ age births to 40-44 group) ------------------
Births <- tibble() 
Births <- read_excel("Input/Vital Stats IN.xlsx")








# ASFR Calculation (IN and 2005 as starting point) ------------------


#need to remove the GQ???

#for each year in list (i to ?) where year starts at i and increases i+1 each time, check that pop year matched birth year
#then calculate for each cohort the (number of births in cohort / number of females in household population in that age group)
#add to new tibble called ASFR()

















