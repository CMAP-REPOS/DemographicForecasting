#
# This script pulls in the projected Race/Ethnicity rates (performed in an
# Excel workbook and saved in Input/raceethrates.xlsx)
# and applies them to the projected population totals in order to produce
# an estimate on the total projected population by race/ethnicity.
#
# Alexis McAdams, Mary Weber, November 2021

library(tidyverse)
library(tidycensus)
library(readxl)

# Import population projection by age/sex
load("Output/Migration_Projections.Rdata") #Mig_Proj

#Import projected population rates by race and ethnicity groupings (from Race_Ethnicity.R)
re_rates <- read_excel(path = "Input/raceethrates.xlsx") %>%
  pivot_longer(starts_with("2"), names_to = "Year", values_to = "Proportion")

# Join R/E rates and projected population totals
pop_summary <- Mig_Proj %>% group_by(year, Region) %>% summarize(TotPopulation = sum(ProjectedPop_final))

# Calculate total population by race and ethnicity grouping
raceeth_proj <- full_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  filter(Year >=2025) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  select(-Proportion, -TotPopulation)

# Save results (used in HH_Control.R)
save(raceeth_proj, file = "Output/totalPop_RE_projection.R")

# Create alternate summary version
raceeth_proj_summary <- raceeth_proj %>%
  pivot_wider(names_from = Year, values_from = calcPop)

# write summary version to disk
#write.csv(raceeth_proj, file = "C:/Users/amcadams/Documents/R/race_eth_projections.csv")
#write.csv(raceeth_proj_summary, file = "/Users/mweber/Desktop/raceeth_proj.csv")


# double-check that population totals match (compare to pop_summary abve)
pop_summary2 <- raceeth_proj_summary <- full_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  filter(Year >=2025) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  group_by(Region, Year) %>% summarize(totREPop = sum(calcPop))
