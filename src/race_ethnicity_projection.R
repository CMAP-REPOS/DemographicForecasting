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
#load("Output/Migration_Projections.Rdata") ########### Mig_Proj
load(file="Output/PopProj.Rdata") # POPPROJ
#unlist projection results (formerly called export)
results <- tibble()
i=1
for(item in POPPROJ){
  #print(item)
  temp <- item
  temp$year <- names(POPPROJ)[i]
  results <- bind_rows(results, temp)
  i <- i + 1
}

# Import projected population rates by race and ethnicity groupings (from Race_Ethnicity.R)
re_rates <- read_excel(path = "Input/raceethrates.xlsx") %>%
  pivot_longer(starts_with("2"), names_to = "Year", values_to = "Proportion")

# combine known population and projected population totals
load("Output/POP_PEP.Rdata")  # named POP

# apply EXTIL adjustment (if necessary)
if(EXTIL == 1){
  POP[["2010"]] <- read.csv("Input/adjustedCensus2010_ExtIL.csv") %>%
    select(-X) %>%
    mutate(GEOID = as.character(GEOID)) %>% as_tibble()
  POP[["2015"]] <- read.csv("Input/adjustedPEP2015_ExtIL.csv") %>%
    select(-X) %>%
    mutate(GEOID = as.character(GEOID)) %>% as_tibble()
  POP[["2020"]] <- read_excel("Input/censusadjustedPEP2020_ExtILadj.xlsx") %>% # partial LOL counties
    mutate(GEOID = as.character(GEOID))
} else if (EXTIL == 0){
  # no modification to POP file.
  print("ExtIL Area Adjustment override NOT implemented.")
}else {
  print("ERROR! Improper EXTIL value supplied. Modify and run again.")
}

knownpop <- bind_rows(POP[['2015']], POP[['2020']]) %>%
  group_by(Year, Region) %>%
  summarize(TotPopulation = sum(Population), .groups = "drop") %>%
  mutate(year = as.character(Year)) %>%
  select(-Year)

# Join R/E rates and projected population totals
pop_summary <- results %>%
  group_by(year, Region) %>%
  summarize(TotPopulation = sum(ProjectedPop_final), .groups = "drop") %>%
  bind_rows(knownpop) %>% arrange(year)

# Calculate total population by race and ethnicity grouping
raceeth_proj <- inner_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  select(-Proportion, -TotPopulation) %>% ungroup()

# Save results (used in HH_Control.R)
save(raceeth_proj, file = "Output/totalPop_RE_projection.Rdata")
