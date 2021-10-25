# This script pulls in the projected Race/Ethnicity rates (projected in an Excel workbook)
# and applies it to the projected population totals.

library(tidyverse)
library(tidycensus)
library(readxl)



load("Output/Migration_Projections.Rdata") #Mig_Proj

#Import projected rates by race and ethnicity groupings (see Race_Ethnicity.R)
re_rates <- read_excel(path = "Input/raceethrates.xlsx") %>%
  pivot_longer(starts_with("2"), names_to = "Year", values_to = "Proportion")

# join rates and projected population totals
pop_summary <- Mig_Proj %>% group_by(year, Region) %>% summarize(TotPopulation = sum(ProjectedPop_final))

# calculate total population by race and ethnicity group
raceeth_proj <- full_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  filter(Year >=2025) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  select(-Proportion, -TotPopulation) %>% pivot_wider(names_from = Year, values_from = calcPop)

#export results

write.csv(raceeth_proj, file = "C:/Users/amcadams/Documents/R/race_eth_projections.csv")
