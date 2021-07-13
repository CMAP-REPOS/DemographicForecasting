# CMAP | Alexis McAdams, Mary Weber | 7/12/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
load("Output/ASFR.Rdata")
load("Output/LifeTables.Rdata")

MIG_YEARS <- c(2013:2014, 2018:2019)

# Population by 5-Year Age Group, Sex and Region; 2013-14 and 2018-19 Averaged  ---------------------------------------------------------

MIG_POP <- tibble()

for (YEAR in MIG_YEARS) {
  MIG_POP <- bind_rows(MIG_POP, POP[[as.character(YEAR)]])
}

MIG_POP <- MIG_POP %>% select(-County, -GEOID, -State) %>% group_by(Region, Year, Age, Sex) %>%
                              summarise(Population = sum(Population), .groups="drop") %>% filter(Region == 'External WI') %>% #remember to remove this filter
                              mutate(Group = case_when(Year %in% 2013:2014  ~ 1,
                                                       Year %in% 2018:2019 ~ 2)) %>% select(-Year)

MIG_POP <- MIG_POP %>% group_by(Age, Sex, Region, Group) %>% mutate(Pop_Avg = case_when(Group == 1 ~ round(mean(Population),0),
                                                                                       Group == 2 ~ round(mean(Population),0))) %>%
                                ungroup() %>% select(Region, Age, Sex, Group, Pop_Avg) %>% distinct(Age, Sex, Region, Group, .keep_all = TRUE)
View(MIG_POP)


# Births by Region 2014-2018 ---------------------------------------------------------

# Did not request sex of baby, potential improvement for next iteration

MIG_Births <- ASFR %>% filter(Year %in% 2014:2018) %>% distinct(State, Year, Age, Region, Births) %>% group_by(Sex, Region) %>% mutate(Births = sum(Births))

# filter(Region == 'External WI') #remember to remove this filter

View(MIG_Births)


# Abridged Life Tables (group 0-4 back together) ---------------------------------------------------------











