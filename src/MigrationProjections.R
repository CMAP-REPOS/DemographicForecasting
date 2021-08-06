# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

library(dplyr)
library(tidyverse)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
load("Output/Mort_Proj.Rdata")
load("Output/ASFR.Rdata")
load("Output/Base_Migration.Rdata")

# Step 1: Age-Sex Specific Survival Rates, 2020-2050, Midpoints of 5-year Intervals

Mort_MidPoint <- Mort_Proj %>% mutate('2022.5'=rowMeans(across('2020':'2025')),
                                  '2027.5'=rowMeans(across('2025':'2030')),
                                  '2032.5'=rowMeans(across('2030':'2035')),
                                  '2037.5'=rowMeans(across('2035':'2040')),
                                  '2042.5'=rowMeans(across('2040':'2045')),  #we don't need to go out to 2060 but we have the data to do so
                                  '2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(4:13)) %>% filter(Region == 'CMAP Region')


# Step 2: Age Specific Fertility Rate Projections, Midpoints of 5-year Intervals, 2020-2050

ASFR_MidPoint <- ASFR_projections %>% pivot_wider(names_from = "Year", values_from="ASFR_proj") %>% select(-c(3))

ASFR_MidPoint <- ASFR_MidPoint %>% mutate('2022.5'=rowMeans(across('2020':'2025')),
                                  '2027.5'=rowMeans(across('2025':'2030')),
                                  '2032.5'=rowMeans(across('2030':'2035')),
                                  '2037.5'=rowMeans(across('2035':'2040')),
                                  '2042.5'=rowMeans(across('2040':'2045')),
                                  '2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(3:11))
#add in special calculation to combine 0-1 and 1-4 (David ER)

#Step 3: Pull in 2020 PEP data (not available via Census API yet)

PEP2020 <- read_excel("Input/PEP2020.xlsx") %>% select(-County) %>%
  group_by(Age, Region, Sex) %>% summarise(Population = sum(Population))



# Step 3: Pull in Berger Net Migration values and calculate flat average; should automate 2014-18 data

NetMig <- read_excel("Input/NetMigration_Berger.xlsx")

NetMig <- NetMig %>% filter(Age == 'Total' & Sex == 'Both') %>% select(-Period) %>%
  group_by(Region) %>% summarise(NetMigration = round (mean(NetMigration),-3)) #round to nearest thousand

#m <- Base_Mig %>%
#  select(Region, Sex, SurvMigrants2018) %>%
#  group_by(Region, Sex) %>%
#  mutate(SurvMigrants2018 = sum(SurvMigrants2018)) %>% unique()



# all numbers up to this point match Alexis' excel model :)


# Step 4: get projected birth for 0-1 age group

#multiply 2020 PEP by 2022.5 ASFRs to get expected births by age group, 2020-2025
#sum expected births and multiply by average fertility ratio (not sure how this is calculated)


# Step 5: calculate expected 2025 population


#2020 PEP * 2022.5 ASFRs - births calculation comes from step above

