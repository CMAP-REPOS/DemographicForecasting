# Data Cleaning for IPUMS NHGIS data
# By Haoyu Shi
# On 1/6/2021

# Detailed notes for data: https://www.nhgis.org/sites/www.nhgis.org/files/nhgis_time_series_tables.pd
# code book: Input/nhgis0001_csv/

library(tidyverse)

raw <- read.csv("Input/nhgis0001_csv/nhgis0001_ts_nominal_county.csv")
# this countains more data than we need to use (variables, states, and time)


# select data from 1990 (which are not available via tidycensus)
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

raw_select_states_and_1990 <- raw %>% filter(STATEFP == 17| # IL
                                   (STATEFP == 55 & COUNTYFP %in% COUNTIES$WI)| #WI
                                   STATEFP == 18 & COUNTYFP %in% COUNTIES$IN) %>% select( #IN
  GISJOIN,
  STATE,
  STATEFP,
  COUNTY,
  COUNTYFP,
  ends_with(c("1990")))

rm(raw) # remove raw data set to save memory

renamed <- raw_select_states_and_1990 %>% rename(tot_pop = A00AA1990,
                                      White = AA0AA1990,
                                      Black = AA0AB1990,
                                      Asian = AA0AD1990,
                                      Native = AA0AC1990,
                                      Other = AA0AE1990, # in 1990, there was no "more than two races" option
                                      Hisp = A35AA1990,
                                      NH = A36AA1990,
                                      NH_White = CY6AA1990,
                                      NH_Black = CY6AB1990,
                                      NH_Asian = CY7AD1990,
                                      NH_Native = CY7AC1990,
                                      NH_Other = CY7AE1990,
                                      # Household
                                      HH_tot = A41AA1990,
                                      HH_HISP = CZ3AB1990,
                                      HH_NH_White = CZ7AA1990,
                                      HH_NH_Black = CZ7AB1990,
                                      HH_NH_Native = CZ7AC1990,
                                      HH_NH_Asian = CZ7AD1990,
                                      HH_NH_Other = CZ7AE1990,
                                      HH_NH = CZ3AA1990
                                      )

cleaned <- renamed %>% select(-ends_with("1990")) # only select the variables we want to use

##### verify the data that all races added up to the total population
cleaned$total_NH_and_HISP <- cleaned$tot_pop  == cleaned$Hisp + cleaned$NH
cleaned$total_pop_by_race <- cleaned$tot_pop == cleaned$White + cleaned$Black + cleaned$Asian + cleaned$Native + cleaned$Other
# verified
cleaned <- cleaned %>% select(-c("total_NH_and_HISP", "total_pop_by_race"))

# verify that NH added up by race together equal to NH
cleaned$NH_addedup <- cleaned$NH == cleaned$NH_Asian + cleaned$NH_Black + cleaned$NH_Native + cleaned$NH_Other + cleaned$NH_White
# verified
cleaned <- cleaned %>% select(-NH_addedup)

# verify total HH
cleaned$HH_NH_and_HISP <- cleaned$HH_tot > cleaned$HH_NH + cleaned$HH_HISP
cleaned$HH_NH_addedup <- cleaned$HH_NH == cleaned$HH_NH_Asian + cleaned$HH_NH_Black + cleaned$HH_NH_Native + cleaned$HH_NH_Other + cleaned$HH_NH_White
# While the total NH households (occupied) equal to the HH_NH by race, the sum for HH_NH and HH_HISP < HH_tot. Housing units may be unoccupied
cleaned <- cleaned %>% select(-c(HH_NH_and_HISP, HH_NH_addedup))
cleaned$HH_tot <- cleaned$HH_HISP + cleaned$HH_NH # get the occupied housing units

##### Data Manipulation
Manip <- cleaned
Manip$GEOID <- substring(Manip$GISJOIN, 2, 7)
Manip <- Manip %>% select(-c(GISJOIN, STATEFP, COUNTYFP, # no need for final product
                                           White, Black, Asian, Native, Other)) # race for both NH and HISP is not available for householder, which is different from ACS

Manip_POP <- Manip %>% select(-starts_with("HH"))
Manip_POP$NH_Other <- Manip_POP$NH_Other + Manip_POP$NH_Native # put Native and Other into one category
Manip_POP <- Manip_POP %>% select(GEOID,
                                  COUNTY,
                                  STATE,
                                  tot_pop,
                                  Hisp,
                                  NH_White,
                                  NH_Black,
                                  NH_Asian,
                                  NH_Other)

names(Manip_POP) <-c("GEOID", "County", "State", "All", "Hispanic", "NH_White", "NH_Black", "NH_Asian", "NH_Other")

POP_longer <- Manip_POP %>%
  pivot_longer(
    cols = c("All", "Hispanic", "NH_White", "NH_Black", "NH_Asian", "NH_Other"),
    names_to = "Race",
    values_to = "Population"
  )

# Householder:
Manip_HH <- Manip%>% select(GEOID,
                            COUNTY,
                            STATE,
                            HH_tot,
                            HH_HISP,
                            HH_NH_White,
                            HH_NH_Black,
                            HH_NH_Asian,
                            HH_NH_Other)

names(Manip_HH) <-c("GEOID", "County", "State", "All", "Hispanic", "NH_White", "NH_Black", "NH_Asian", "NH_Other")

HH_longer <- Manip_HH %>%
  pivot_longer(
    cols = c("All", "Hispanic", "NH_White", "NH_Black", "NH_Asian", "NH_Other"),
    names_to = "Race",
    values_to = "Householder"
  )

# join two tables
full_longer <- left_join(POP_longer, HH_longer)
full_longer$head.rate <- full_longer$Householder / full_longer$Population
full_longer$Year <- 1990
full_longer <- full_longer %>% select(c("GEOID", "County", "State", "Year", "Race", "Population", "Householder", "head.rate"))

# export csv file
write.csv(full_longer,"C:/Users/hshi/Desktop/Demographic_Prediction/Census_1990_head_rate.csv", row.names = FALSE)

