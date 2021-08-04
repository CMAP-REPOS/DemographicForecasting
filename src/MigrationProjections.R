# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

#load("Output/PopData.Rdata")

load("Output/Mort_Proj.Rdata")
load("Output/ASFR.Rdata")
load("Output/Base_Migration.Rdata")


# Step 1: Age-Sex Specific Survival Rates, 2020-2050, Midpoints of 5-year Intervals

Mort_Proj <- Mort_Proj %>% mutate('2022.5'=rowMeans(across('2020':'2025')),
                                  '2027.5'=rowMeans(across('2025':'2030')),
                                  '2032.5'=rowMeans(across('2030':'2035')),
                                  '2037.5'=rowMeans(across('2035':'2040')),
                                  '2042.5'=rowMeans(across('2040':'2045')),  #we don't need to go out to 2060 but we have the data to do so
                                  '2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(4:12))

#write.csv(Mort_Proj, "/Users/mweber/Desktop/mort_proj.csv")
#View(Mort_Proj)


# Step 2: Age Specific Fertility Rate Projections, Midpoints of 5-year Intervals, 2020-2050

#Reminder: different states start ASFR porjections at different years, that's why there's no IN data for 2018-19


ASFR <- ASFR_projections %>% select(State, Age, Sex, Year, Region, ASFRs) %>% pivot_wider(names_from = "Year", values_from="ASFRs") %>% select(-c(5:14))


# Step 3: Net Migration -------------------------------------------------



temp <- tibble(Period = as.character(), Region = as.character(), NetMigrants = as.numeric()) #

# amend total 2014-18 net migration to Berger values

m <- Base_Mig %>% select(Region, SurvMigrants2018) %>% group_by(Region) %>% mutate(NetMigrants = sum(SurvMigrants2018)) %>% select(-SurvMigrants2018) %>% distinct()
m$Period <- '2014-2018'

temp %>% add_row(m)

View(m)

# Step 2: Calculate weighted Net Migration -------------------------------------------------

temp <- group_by(Region) %>% mutate() #case when and calculate by period range


