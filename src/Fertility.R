# CMAP | Mary Weber | 2/10/2021

library(tidyverse)
library(tidycensus)
library(readxl)

load("Output/PopData.Rdata")

# Set parameters ----------------------------------------------------------

F_YEARS <- c(2010:2019) #will eventually include all years 2010 through 2020
F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")


# Filter data to only include female population within child-bearing years (15-44) ----------------------
F_DATA <- tibble()
for (YEAR in F_YEARS) {
  TEMP_DATA <- POP[[as.character(YEAR)]] %>%
    filter(Sex == 'Female') %>%
    filter(Age %in% F_Groups)
  F_DATA <- bind_rows(F_DATA, TEMP_DATA)
}

# Remove estimate of females in GQ from each year 

# GQ totals by county for 2010, excluding military

GQ_2010 <- GQ %>%  # Lake, IL #s are funky
  filter(Category == 'County Total')  %>% 
  filter(Concept != 'GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE') %>% 
  group_by(GEOID, County, State, Year, Region) %>%
  summarise(total_GQ = sum(Value)) 


# for each county, find the proportion of females in each age group using 2010 data
Female_2010 <- POP[["2010"]] %>%
  filter(Sex == 'Female') %>%
  filter(Age %in% F_Groups)












# Birth data - add pre-age-15 births to 15-19 group, add 45+ age births to 40-44 group ------------------
Births <- read_excel("Input/Vital Stats IN.xlsx") %>% 
          mutate(Age = case_when(Age %in% c("10 to 14 years") ~ "15 to 19 years",
                                 Age %in% c("45 to 49 years") ~ "40 to 44 years",
                                 TRUE ~ Age))

# ASFR Calculation - # of live births per 1,000 women ------------------

F_DATA <- F_DATA %>% group_by(State, Age, Year) %>% summarise(Population = sum(Population), .groups="drop")

Births <- Births %>% group_by(State, Age, Year) %>% summarise(Births = sum(Births), .groups="drop")

ASFR <- F_DATA %>% inner_join(Births, by = c("State", "Age", "Year")) %>% mutate(ASFR = round((Births/Population)*1000, 0))


# sample plots ---------

ASFR %>% 
  filter(Year %in% c(2010:2019)) %>%
  ggplot(aes(x=Year, y=ASFR, group=Age, color=Age)) +  
  scale_x_continuous(breaks = c(2010:2019)) +
  ggtitle("2010-2019 ASFRs") + 
  geom_line() + 
  geom_point()



