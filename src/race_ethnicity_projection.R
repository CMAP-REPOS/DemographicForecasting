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

#combine known population and projected population totals
load("Output/POP_PEP.Rdata")  # named POP
POP[["2010"]] <- read.csv("Input/adjustedCensus2010_ExtIL.csv") %>% mutate(GEOID = as.character(GEOID)) # ADJUSTMENT for Ext IL!
POP[["2015"]] <- read.csv("Input/adjustedPEP2015_ExtIL.csv") %>% mutate(GEOID = as.character(GEOID))

knownpop <- bind_rows(POP[['2015']], POP[['2020']] %>% mutate(GEOID = as.character(GEOID))) %>%
  group_by(Year, Region) %>% summarize(TotPopulation = sum(Population)) %>% ungroup() %>%
  mutate(year = as.character(Year)) %>% select(-Year)

# Join R/E rates and projected population totals
pop_summary <- Mig_Proj %>% group_by(year, Region) %>% summarize(TotPopulation = sum(ProjectedPop_final)) %>%
  ungroup() %>% bind_rows(knownpop)


# Calculate total population by race and ethnicity grouping
raceeth_proj <- full_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  #filter(Year >=2025) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  select(-Proportion, -TotPopulation) %>%
  filter(!is.na(calcPop)) %>% ungroup()

# Save results (used in HH_Control.R)
save(raceeth_proj, file = "Output/totalPop_RE_projection.R")

# Create alternate summary version
raceeth_proj_summary <- raceeth_proj %>%
  pivot_wider(names_from = Year, values_from = calcPop)

# write summary version to disk
write.csv(raceeth_proj, file = "C:/Users/amcadams/Documents/R/extILadj/race_eth_projections.csv")
#write.csv(raceeth_proj_summary, file = "/Users/mweber/Desktop/raceeth_proj.csv")


# double-check that population totals match (compare to pop_summary abve)
pop_summary2 <- raceeth_proj_summary <- full_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  group_by(Region, Year) %>% summarize(totREPop = sum(calcPop))

# plot a graph, because of course
library(ggplot2)
p <- raceeth_proj %>%
  mutate(Category = paste(RACE, HISP, sep = "_")) %>%
  filter(RACE != "White alone") %>%
  ggplot(aes(x= Year, y=calcPop, group = Category, color = Category)) + geom_line() + facet_wrap(~ Region, scales = "free")
#p
