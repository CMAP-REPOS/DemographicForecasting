# CMAP | Mary Weber | 12/8/2020

#install.packages(c("tidyverse", "tidycensus", "readxl"))
library(tidyverse)
library(tidycensus)
library(readxl)
#census_api_key("d94fbe16b1b053593223397765874bf147d1ae72", install = TRUE)


# Set parameters ----------------------------------------------------------
YEAR2 <- 2015
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

# Compile population data from each Census --------------------------------

PEP_DATA <- tibble()
  for (STATE in names(COUNTIES)) {
    
    TEMP2 <- get_estimates(product="characteristics", geography = "county", 
                        county = COUNTIES[[STATE]], state = STATE, breakdown = c("SEX", "AGEGROUP"), 
                        breakdown_labels = TRUE, time_series=TRUE, show_call=TRUE) %>%
                        filter(DATE == 8, SEX %in% c("Male", "Female")) %>%
                        separate(NAME, c("County", "State"), sep = "\\, ") 
      
    PEP_DATA <- bind_rows(PEP_DATA, TEMP2)
  }

# Create final table --------------------------------

PEP_DATA$Year <- 2015
PEP_DATA$Region <- ''
PEP_DATA <- PEP_DATA[-which(PEP_DATA$AGEGROUP %in% remove), ]
  
POP[[as.character(YEAR2)]] <- PEP_DATA %>%
  mutate(AGEGROUP = case_when(AGEGROUP == "All ages" & SEX == 'Male' ~ "County Male Total",
                              AGEGROUP == "All ages" & SEX == 'Female' ~ "County Female Total",
                              TRUE ~ as.character(AGEGROUP)),
         
        Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
              State == "Illinois" ~ "External IL",
              State == "Indiana" ~ "External IN",
              State == "Wisconsin" ~ "External WI"))                               
                                    

                                         

                                         
                                         
                                         

View(POP[["2015"]])

                                         