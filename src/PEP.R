# CMAP | Mary Weber | 1/20/2021

#This file contains 1995, 2005 and 2015 - 2019 Population Estimates Program (PEP) data

#install.packages(c("tidyverse", "tidycensus", "readxl"))
library(tidyverse)
library(tidycensus)
library(readxl)
load("Output/PopData.Rdata") #must load, following code dependent on the POP[[]] 
#census_api_key("d94fbe16b1b053593223397765874bf147d1ae72", install = TRUE)


# Set parameters ----------------------------------------------------------
YEAR2 <- c(2015:2019)
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

#extra age groupings to remove
remove <- c("Under 18 years", "5 to 13 years", "14 to 17 years", "18 to 64 years", "18 to 24 years", "25 to 44 years", 
            "45 to 64 years", "65 years and over", "85 years and over", "16 years and over", "18 years and over", 
            "15 to 44 years", "Median age")

# Compile population data from each Census. Always confirm that get_estimates() is pulling from latest vintage. 
# Use ?get_estimates to see what the default vintage year is. If it's not the most recent year, update version of tidycensus (if a more recent version is available) ---------------

PEP_DATA <- tibble()
for(YEAR in YEAR2){
  
  for (STATE in names(COUNTIES)) {
    
    TEMP2 <- get_estimates(product="characteristics", geography = "county", year = YEAR,
                           county = COUNTIES[[STATE]], state = STATE, breakdown = c("SEX", "AGEGROUP"), 
                           breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE) %>%
      filter(DATE == 8, SEX %in% c("Male", "Female")) %>%
      separate(NAME, c("County", "State"), sep = "\\, ")
    
    TEMP2$Year <- YEAR
    
    PEP_DATA <- bind_rows(PEP_DATA, TEMP2)
  }
}

# Create final table --------------------------------

colnames(PEP_DATA)[5] <- "Sex"
PEP_DATA <- select(PEP_DATA, -c(7))
PEP_DATA$Region <- ''
PEP_DATA <- PEP_DATA[-which(PEP_DATA$AGEGROUP %in% remove), ] #removes additional, unwanted age groupings

for(YEAR in YEAR2) { 
  
  POP[[as.character(YEAR)]] <- PEP_DATA %>%
    mutate(AGEGROUP = case_when(AGEGROUP == "All ages" & Sex == 'Male' ~ "County Male Total",
                                AGEGROUP == "All ages" & Sex == 'Female' ~ "County Female Total",
                                TRUE ~ as.character(AGEGROUP)),
           Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                              State == "Illinois" ~ "External IL",
                              State == "Indiana" ~ "External IN",
                              State == "Wisconsin" ~ "External WI")) 
  
}


# Upload Excel files and save Output file (only necessary if changes to code/Excel data are made) -------------------------------

POP[["1995"]] <- read_excel("Input/Pop1995.xlsx") 
POP[["2005"]] <- read_excel("Input/Pop2005.xlsx") 

save(POP, file="Output/PopData.Rdata")
load("Output/PopData.Rdata")
                                         



























