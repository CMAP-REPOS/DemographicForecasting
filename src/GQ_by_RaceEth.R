# GQ_RaceEth.R
# This script estimates and projects the percentage of Group Quarter population
# by Race/Ethnicity groupings (Census 2010)
# Calculation of the projected populations occurs in
# race_ethnicity_projection.R
#
# Alexis McAdams | 16 Nov 2021

library(tidyverse)
library(ggplot2)
library(tidycensus)

# set up parameters
Decennial_YEARS <- c(2010) #ideally add 2020 when that data is available

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

#pull the variables needed and reformat the name (Category)
GQREvariables <- c("PCT020001", #total
                   "PCT020I001", #white alone, not hisp/latino
                   "PCT020B001", #black alone
                   "PCT020D001", #asian alone
                   "PCT020H001"  #hispanic/latino
)

# pull the data from Census
GQ_RaceEth <- tibble()
#GQRE <- list()

#Decennial_DATA <- tibble()
#POP <- list()

for (YEAR in Decennial_YEARS) {

  for (STATE in names(COUNTIES)) {
    TEMP <- get_decennial(geography = "county", variables = GQREvariables,
                          county = COUNTIES[[STATE]], state = STATE,
                          year = YEAR, survey = "sf1", cache_table = TRUE)%>%
      separate(NAME, c("County", "State"), sep = "\\, ") %>%
      rename(Population = value) %>%
      mutate(variable = case_when(variable == "PCT020001" ~ "total_GQ",
                                  variable == "PCT020I001" ~ "NH_White",
                                  variable == "PCT020B001" ~ "NH_Black",
                                  variable == "PCT020D001" ~ "NH_Asian",
                                  variable == "PCT020H001" ~ "Hispanic") )

    GQ_RaceEth <- bind_rows(GQ_RaceEth, TEMP)
  }

  GQ_RaceEth$Year = '2010'

  GQ_RaceEth <- GQ_RaceEth %>%
    mutate(Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                              State == "Illinois" ~ "External IL",
                              State == "Indiana" ~ "External IN",
                              TRUE ~ "External WI"))
}

#create summary table
GQRE_summary <- GQ_RaceEth %>%
  group_by(Region, Year, variable) %>%
  summarize(Population = sum(Population))

# calculate NH_Other
NH_others <- GQRE_summary %>%
  pivot_wider(names_from = variable, values_from = Population) %>%
  rowwise() %>%
  mutate(NH_Other = total_GQ - Hispanic - NH_white - NH_black - NH_Asian) %>%
  select(Region, NH_Other) %>%
  rename(Population = NH_Other) %>%
  mutate(variable = "NH_Other")

#add NH_Other to summary table
GQRE_summary <- bind_rows(GQRE_summary, NH_others)

#pivot out the GQ totals
GQ_totals <- GQRE_summary %>% ungroup() %>%
  filter(variable == "total_GQ") %>%
  rename(totalGQ = Population) %>%
  select(totalGQ, Region)

GQRE_perc <- GQRE_summary %>% filter(variable != "total_GQ") %>%
  rowwise() %>%
  mutate(GQ_perc = Population / totalGQ) %>%
  select(Region, variable, GQ_perc)

save(GQRE_perc, file="Output/GQRE_rates.Rdata")
