# CMAP | Mary Weber | 2/4/2021

library(tidyverse)
library(tidycensus)
library(readxl)
load("Output/PopData.Rdata")

# Set parameters ----------------------------------------------------------

F_YEARS <- c(2010, 2015) 

POP[['2010']][,"Sex"] <- NA
POP[['2010']][,"Category2"] <- NA


# Changes needed to PEP format to match Decennial Census format --------------------------------

POP[['2015']] <- POP[['2015']] %>% rename(Category = AGEGROUP, Value = value)
POP[['2015']][,"Category2"] <- NA

POP[["2015"]] <- POP[["2015"]] %>%
  filter(Sex == 'Female') %>%
  mutate(Category = case_when(Category %in% c("Age 15 to 19 years") ~ "Female 15 to 19 years",
                               Category %in% c("Age 20 to 24 years") ~ "Female 20 to 24 years",
                               Category %in% c("Age 25 to 29 years") ~ "Female 25 to 29 years",
                               Category %in% c("Age 30 to 34 years") ~ "Female 30 to 34 years",
                               Category %in% c("Age 35 to 39 years") ~ "Female 35 to 39 years",
                               Category %in% c("Age 40 to 44 years") ~ "Female 40 to 44 years",
                               TRUE ~ Category))


# Starting with IN Data as practice ------------------

F_DATA <- tibble()

for (YEAR in F_YEARS){
  
  TEMP_DATA <- POP[[as.character(YEAR)]] %>%
    mutate(Sex = replace(Sex, str_starts(Category, 'Female'), 'Female')) %>%
    filter(Sex == 'Female' & State == 'Indiana') %>%
    mutate(Category2 = case_when(Category %in% c("Female 15 to 17 years", "Female 18 and 19 years") ~ "Female 15 to 19 years",
                                 Category %in% c("Female 20 years", "Female 21 years", "Female 22 to 24 years") ~ "Female 20 to 24 years",
                                 TRUE ~ Category))  %>%
    filter(Category2 %in% c("Female 15 to 19 years", "Female 20 to 24 years", "Female 25 to 29 years", "Female 30 to 34 years", "Female 35 to 39 years", "Female 40 to 44 years"))
    F_DATA <- bind_rows(F_DATA, TEMP_DATA)
  }

View(F_DATA)

#need to remove the GQ???
#QUESTION: add 10-14 births to 15-19, add 45 & over births to 40-44 OR reduce fertile female range to 15-4 - David ER



# ASFR Calculation ------------------



#for each year in list (i to ?) where year starts at i and increases i+1 each time, calculate per each item in the
#named list (number of births in cohort / number of females in household population in that age group)

#graph to make sure things look like they're on track

#talk to David ER about projection



F_Years <- c(2015) #add in 2010 later
F <- list()

for (YEAR in F_Years) {
temp <- POP[[as.character(YEAR)]] %>%
  filter(State == 'Indiana' & SEX == 'Female') 

  
}

#County, state, value, year, region, category2














