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

Mort_MidPoint <- Mort_Proj %>% mutate('2022.5'=rowMeans(across('2020':'2025')),
                                  '2027.5'=rowMeans(across('2025':'2030')),
                                  '2032.5'=rowMeans(across('2030':'2035')),
                                  '2037.5'=rowMeans(across('2035':'2040')),
                                  '2042.5'=rowMeans(across('2040':'2045')),  #we don't need to go out to 2060 but we have the data to do so
                                  '2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(4:13)) %>% filter(Region == 'CMAP Region')

# numbers match Alexis' excel model :)


# Step 2: Age Specific Fertility Rate Projections, Midpoints of 5-year Intervals, 2020-2050

ASFR_MidPoint <- ASFR_projections %>% pivot_wider(names_from = "Year", values_from="ASFR_proj") %>% select(-c(3))

ASFR_MidPoint <- ASFR_MidPoint %>% mutate('2022.5'=rowMeans(across('2020':'2025')),
                                  '2027.5'=rowMeans(across('2025':'2030')),
                                  '2032.5'=rowMeans(across('2030':'2035')),
                                  '2037.5'=rowMeans(across('2035':'2040')),
                                  '2042.5'=rowMeans(across('2040':'2045')),
                                  '2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(3:11))

# numbers match Alexis' excel model :)


#Step 3: Pull in 2020 PEP data








# Step 3: Net Migration -------------------------------------------------

#read in net migration values (includes the 2014-2018 data as well)
NetMig <- read_excel("Input/NetMigration_Berger.xlsx")





# Step 2: Calculate weighted Net Migration -------------------------------------------------

temp <- group_by(Region) %>% mutate() #case when and calculate by period range


