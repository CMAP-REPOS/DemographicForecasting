# CMAP | Alexis McAdams | 9/20/2021
#
# This script imports previous Net Migration data and calculates
# total net migration sums for different age categories, which are defined in rows 21-26 below.
# This process directly informs the K-factor calculations in the
# Projection.R (formerly Mary_working.R) script.
#

# Import Berger's data (copied by AM from Berger's "Forecast Model_v09-Trans+Edu Employment.xlsx" spreadsheet)
# This file contains 5-year net migration *values* by Region, age and sex from 1991 to 2005 (4 periods of data)
pastNetMig <- read_excel("Input/NetMigration_Berger_Full_sexandage.xlsx")

# import recent base migration rates (from recentMigration.R, formerly Migration.R)
load("Output/Base_Migration.Rdata") # named recent_Base_Mig

recentNetMig <- recent_Base_Mig %>%  #reshaping data, saving data source in Source column
  select(Region, Age, Sex, SurvMigrants2018) %>%
  mutate(Period = "2014-2018",
         Source = "CMAP") %>%
  rename(NetMigration = SurvMigrants2018)

pastNetMig <- bind_rows(pastNetMig, recentNetMig) %>% # append reshaped recent Net Migration values to pastNetMig table
  mutate(endyear = substr(Period, 6,9)) %>%
  mutate(agegroup = "TEMP") # placeholder

# define 4 major age groups
agegroups <- list( c('0 to 4 years', '5 to 9 years', '10 to 14 years', '15 to 19 years', '20 to 24 years'),
                   c('25 to 29 years', '30 to 34 years', '35 to 39 years'),
                   c('40 to 44 years', '45 to 49 years', '50 to 54 years', '55 to 59 years', '60 to 64 years', '65 to 69 years'),
                   c('70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over') )

names(agegroups) <- c("0 to 24 years", "25 to 39 years", "40 to 69 years", "70 years and older")

# assign the major age grouping to the pastNetMig dataset, then calculate sum totals by major age group, Region and sex
i <- 1
for(item in agegroups){
  pastNetMig <- pastNetMig %>%
    mutate(agegroup = case_when(Age %in% item ~ names(agegroups[i]),
                                TRUE ~ agegroup) )
  i <- i+1
}

netMigSums <- pastNetMig %>% group_by(Region, Period, Sex, Source, agegroup) %>%
  summarize(NetMigration = sum(NetMigration)) %>%
  select(Period,Region,NetMigration, Sex, agegroup, Source) %>%
  mutate(NetMigration = round(NetMigration, 0)) %>%
  rename(Age = agegroup) %>%
  ungroup()

# export
save(agegroups, netMigSums, file="Output/pastMigration_ageGroupSums.Rdata")

