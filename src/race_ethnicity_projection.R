#
# This script pulls in the projected Race/Ethnicity rates (performed in an
# Excel workbook and saved in Input/raceethrates.xlsx)
# and applies it to the projected population totals.
#
# Alexis McAdams, Mary Weber, November 2021

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
  select(-Proportion, -TotPopulation)

#export results
save(raceeth_proj, file = "Output/totalPop_RE_projection.R")

#create alternate summary version
raceeth_proj_summary <- raceeth_proj %>%
  pivot_wider(names_from = Year, values_from = calcPop)

#write.csv(raceeth_proj_summary, file = "C:/Users/amcadams/Documents/R/race_eth_projections.csv")

#write.csv(raceeth_proj_summary, file = "/Users/mweber/Desktop/raceeth_proj.csv")

#check that population totals match (compare to pop_summary)
pop_summary2 <- raceeth_proj_summary <- full_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  filter(Year >=2025) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  group_by(Region, Year) %>% summarize(totREPop = sum(calcPop))


#

#import GQ populations by Race/Ethnicity (performed in HH_Control.R)
load("Output/GQRE_projections_RE.data") #GQRE_proj

#join the projected GQ populations by race to the Total populations by race,
  # and subtract to get household population by race.
HHRE_proj <-


# reformat the total population by R/E projections
GQRE_proj <- full_join(re_rates, pop_summary, by=c("Year" = "year", "Region")) %>%
  filter(Year >=2025) %>%
  rowwise() %>%
  mutate(calcPop = round(TotPopulation * Proportion, 0)) %>%
  select(-Proportion, -TotPopulation) %>%
  mutate(variable = case_when(HISP == "Hispanic" ~ "Hispanic",
                              RACE == "Asian alone" ~ "NH_Asian",
                              RACE == "Black alone" ~ "NH_Black",
                              RACE == "White alone" ~ "NH_White",
                              RACE == "NH_Other" ~ "NH_Other",
                              TRUE ~ "99999999")) %>%
  left_join(GQRE_perc, by = c("Region", "variable")) %>%
  rename(totPop = calcPop) %>%
  select(Region, Year, variable, totPop, GQ_perc) %>%
  rowwise() %>%
  mutate(GQ_Pop = round(totPop * GQ_perc,0),
         HH_Pop = totPop - GQ_Pop)

#join and apply the GQRE percentages to the Total Populations by Race/Ethnicity



