# CMAP | Mary Weber | 2/10/2021

library(tidyverse)
library(tidycensus)
library(readxl)
load("Output/PopData.Rdata")

# Set parameters ----------------------------------------------------------

F_YEARS <- c(2010, 2015) 

POP[['2010']][,"Sex"] <- NA
POP[['2010']][,"Category2"] <- NA


# Changes to match PEP format with Decennial Census format --------------------------------

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


# Filter data to only include female population within child-bearing years (15-44) ----------------------

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

F_DATA <- select(F_DATA, -c("Concept", "Variable", "Category"))

View(F_DATA)

# Birth per age cohort of females inn child-bearing years ------------------
Births <- tibble()
Births <- read_excel("Input/Vital Stats IN.xlsx")










# ASFR Calculation (IN and 2005 as starting point) ------------------


#need to remove the GQ???

#for each year in list (i to ?) where year starts at i and increases i+1 each time, check that pop year matched birth year
#then calculate for each cohort the (number of births in cohort / number of females in household population in that age group)
#add to new tibble called ASFR()


#talk to David ER about projection

















