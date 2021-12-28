# Dec 22, 2021
# Haoyu Shi
# Retrieving population and household data from ACS-2010 to ACS-2019 5-year estimate

library(tidycensus)
library(tidyverse)
library(tidyr)

# Geography
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

# Years
Years <- c(2010:2019)

######################### Variables ID ###############################
POP_ID <- c(POP_Total = "B02001_001", # Population Total
            POP_White = "B02001_002", # White
            POP_Black = "B02001_003", # Black
            POP_Asian = "B02001_005", # Asian
            POP_NH_White = "B03002_003", # NH_White
            POP_HISP = "B03002_012") # Hispanic


HH_ID <- c(HH_Total = "B25006_001", # Householder Total
           HH_White = "B25006_002", # White
           HH_Black = "B25006_003", # Black
           HH_Asian = "B25006_005", # Asian
           HH_NH_White = "B22005H_001", # NH_White
           HH_HISP = "B22005I_001") # Hispanic

################ Retrieving data using tidycensus ##################
ACS_Data <- tibble()

for (YEAR in Years){
  for (STATE in names(COUNTIES)){
    TEMP <- get_acs(geography = "county", variables = c(POP_ID,HH_ID),
                          county = COUNTIES[[STATE]], state = STATE,
                          year = YEAR, survey = "acs5", cache_table = TRUE, output = "wide") %>%
      select(-ends_with("M")) %>%
      separate(NAME, c("County", "State"), sep = "\\, ")

    TEMP$Year = YEAR
    ACS_Data <- bind_rows(ACS_Data, TEMP)
  }
}
##### Calculating Data
ACS_Data_Processed <- ACS_Data %>%
  mutate(POP_Other = POP_TotalE - POP_WhiteE - POP_BlackE - POP_AsianE,
         POP_NH = POP_TotalE -POP_HISPE,
         HH_Other = HH_TotalE - HH_WhiteE - HH_BlackE - HH_AsianE,
         HH_NH = HH_TotalE - HH_HISPE)

##### Making it into a longer table
ACS_Pop <- ACS_Data_Processed %>% select("GEOID","County", "State", "Year", starts_with("POP"))
names(ACS_Pop) <- c("GEOID","County", "State", "Year","All", "White", "Black", "Asian", "NH_White", "Hispanic", "Other", "NH")

ACS_HH <- ACS_Data_Processed %>% select("GEOID", "County", "State", "Year", starts_with("HH"))
names(ACS_HH) <- c("GEOID","County", "State", "Year","All", "White", "Black", "Asian", "NH_White", "Hispanic", "Other", "NH")

POP_longer <- ACS_Pop %>%
  pivot_longer(
    cols = c("All", "White", "Black", "Asian", "NH_White", "Hispanic", "Other", "NH"),
    names_to = "Race",
    values_to = "Population"
  )

HH_longer <- ACS_HH %>%
  pivot_longer(
    cols = c("All", "White", "Black", "Asian", "NH_White", "Hispanic", "Other", "NH"),
    names_to = "Race",
    values_to = "Householder"
  )

ACS_longer <- right_join(POP_longer, HH_longer)
ACS_longer$head.rate <- ACS_longer$Householder/ACS_longer$Population

write.csv(ACS_longer, "C:/Users/hshi/Desktop/Demographic_Prediction/ACS-5_head_rate_2010_to_2019.csv")
