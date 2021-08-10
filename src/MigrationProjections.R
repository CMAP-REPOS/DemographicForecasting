# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

library(dplyr)
library(tidyverse)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
load("Output/Mort_Proj.Rdata")
load("Output/ASFR.Rdata")
load("Output/Base_Migration.Rdata")
load("Output/BirthRatios.Rdata")


# Step 1: Age-Sex Specific Survival Rates, 2020-2050, Midpoints of 5-year Intervals

Mort_MidPoint <- Mort_Proj %>% mutate('2022.5'=rowMeans(across('2020':'2025')),
                                  '2027.5'=rowMeans(across('2025':'2030')),
                                  '2032.5'=rowMeans(across('2030':'2035')),
                                  '2037.5'=rowMeans(across('2035':'2040')),
                                  '2042.5'=rowMeans(across('2040':'2045')),  #we don't need to go out to 2060 but we have the data to do so
                                  '2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(4:13)) # %>% filter(Region == 'CMAP Region')


# Step 2: Age Specific Fertility Rate Projections, Midpoints of 5-year Intervals, 2020-2050

ASFR_MidPoint <- ASFR_projections %>% pivot_wider(names_from = "Year", values_from="ASFR_proj") %>%
                          mutate('ASFR2022.5'=rowMeans(across('2020':'2025')),
                                  'ASFR2027.5'=rowMeans(across('2025':'2030')),
                                  'ASFR2032.5'=rowMeans(across('2030':'2035')),
                                  'ASFR2037.5'=rowMeans(across('2035':'2040')),
                                  'ASFR2042.5'=rowMeans(across('2040':'2045')), #if want to add midpoints for 2050-60, add mutates here
                                  'ASFR2047.5'=rowMeans(across('2045':'2050'))) %>%
                          rename_with(.fn = ~paste0("ASFR",.), .cols=starts_with("2")) %>%
                          ungroup()
                                  #select(c(1:2) | ends_with(".5"))
#add in special calculation to combine 0-1 and 1-4 (David ER)

#Step 3: Pull in 2020 PEP data

PEP2020 <- POP[["2020"]] %>% select(-County,-State) %>%
  group_by(Age, Region, Sex) %>% summarise(Pop2020 = sum(Population))

# Step 3: Pull in Berger Net Migration values and calculate flat average; should automate 2014-18 data

NetMig <- read_excel("Input/NetMigration_Berger.xlsx")

NetMig <- NetMig %>% filter(Age == 'Total' & Sex == 'Both') %>% select(-Period) %>%
  group_by(Region) %>% summarise(NetMigration = round (mean(NetMigration),-3)) #round to nearest thousand

#m <- Base_Mig %>%
#  select(Region, Sex, SurvMigrants2018) %>%
#  group_by(Region, Sex) %>%
#  mutate(SurvMigrants2018 = sum(SurvMigrants2018)) %>% unique()



# all numbers up to this point match Alexis' excel model :)


# Step 4 part 1: Calculate projected Births by age cohort and Region in 1-year intervals

F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")

projyears <- c("2020","2021","2022","2023","2024")

projectedBirths <- PEP2020 %>%
  filter(Sex == "Female", Age %in% F_Groups) %>%
  full_join(select(ASFR_MidPoint, c(1:2,contains(all_of(projyears)))), by = c("Age", "Region")) %>%
  mutate(Births2020 = Pop2020 * ASFR2020,
         Births2021 = Pop2020 * ASFR2021,
         Births2022 = Pop2020 * ASFR2022,
         Births2023 = Pop2020 * ASFR2023,
         Births2024 = Pop2020 * ASFR2024) %>%
  select(c(1:2, starts_with("Births"))) %>%
  pivot_longer(cols=starts_with("Births"), names_to = "Year", values_to = "totBirths") %>%
  group_by(Region, Year) %>%
  summarise(totBirths = sum(totBirths))

# Step 4 part 2: Calculate the number of Births (by Sex and Region) that survive the projection period.
# Survival rates for 0-1 and 1-4 are applied based on David ER's cohort method
projectedBirths_bySex <- projectedBirths %>%
  left_join(bRatios, by="Region")%>%
  mutate(fBirths = totBirths*Female,
         mBirths = totBirths*Male, .keep = "unused")

#pull and rearrange 0-1 and 1-4 Survival Rates by sex and Region from Mort_MidPoint
Mort_0to4 <- Mort_MidPoint %>%
  filter(Age == "0 to 1 years" | Age == "1 to 4 years") %>%
  select(1:3 | "2022.5") %>%
  pivot_wider(names_from = c("Sex","Age"), values_from = "2022.5")
names(Mort_0to4) <- make.names(names(Mort_0to4))

# Step 4 part 3: calculate survivors by sex and year, then sum for total number of survivors by Region
projectedBirths_0to4surviving <- projectedBirths_bySex %>%
  left_join(Mort_0to4, by="Region") %>%
  mutate(fSurvivors = case_when(Year == "Births2024" ~ fBirths * Female_0.to.1.years,
                                TRUE ~ fBirths * Female_0.to.1.years * Female_1.to.4.years),
         mSurvivors = case_when(Year == "Births2024" ~ mBirths * Male_0.to.1.years,
                                TRUE ~ mBirths * Male_0.to.1.years * Male_1.to.4.years)) %>%
  group_by(Region) %>%
  summarize(Female = round(sum(fSurvivors),0), Male = round(sum(mSurvivors),0)) %>%
  pivot_longer(cols=c("Female","Male"), names_to = "Sex", values_to = "Pop2025") %>% mutate(Age = "0 to 4 years") #preps table for cbind into ExpectedPop2025

# Step 5: calculate expected 2025 population
expectedpop25 <- PEP2020 %>%
  rename(Age2020 = Age) %>%
  ungroup()






#2020 PEP * 2022.5 ASFRs - births calculation comes from step above

