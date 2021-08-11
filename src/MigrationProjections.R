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

Mort_MidPoint <- Mort_Proj %>% mutate('Mort2022.5'=rowMeans(across('2020':'2025')),
                                  'Mort2027.5'=rowMeans(across('2025':'2030')),
                                  'Mort2032.5'=rowMeans(across('2030':'2035')),
                                  'Mort2037.5'=rowMeans(across('2035':'2040')),
                                  'Mort2042.5'=rowMeans(across('2040':'2045')),  #we don't need to go out to 2060 but we have the data to do so
                                  'Mort2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(4:13))


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

#Step 3: Pull in 2020 PEP data

PEP2020 <- POP[["2020"]] %>% select(-County,-State) %>%
  group_by(Age, Region, Sex) %>% summarise(Pop2020 = sum(Population))

agefactors <- unique(POP[["2020"]]$Age) %>% factor(ordered=TRUE) %>% fct_relevel("5 to 9 years", after = 1)

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
  select(1:3 | "Mort2022.5") %>%
  pivot_wider(names_from = c("Sex","Age"), values_from = "Mort2022.5")
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



# Step 5: apply Survival Rates and calculate Expected 2025 population

PEP2020 <- PEP2020 %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>% select(-x)

#multiply prior 2020 age group population by survival rate for current age group

expectedpop25 <- projectedBirths_0to4surviving %>%
  ungroup() %>%
  full_join(PEP2020, by=c("Region", "Sex", "Age")) %>%
  relocate(c(Age, Pop2020), .before= Pop2025)%>%
  full_join(Mort_MidPoint, by=c('Region', 'Age','Sex')) %>% select(Region, Sex, Age, Pop2020, Mort2022.5, Pop2025) %>%
  arrange(Region, desc(Sex)) %>%
  mutate(Pop2025 = case_when(Age != '0 to 4 years' ~ lag(Pop2020) * Mort2022.5,
                                    TRUE ~ Pop2025))

#drop Pop 2020 column so it doesn't cause confusion later on


PEP2020_test <- PEP2020 %>% mutate(Age2020 = factor(Age, levels = agefactors, ordered = TRUE), .before = Pop2020) %>%
  ungroup() %>%
  select(-Age)
expectedpop25 <- PEP2020_test %>%
  mutate(Age2025 = fct_shift(Age2020, -1))


#add in case when for 0 to 4 survival rate


# Step 6: Import Target Migrant values and calculate K factors
#fine in input folder


# Step 7: Apply K factors to NMRs in order to calculate Net Migration

# Step 8: Apply Net Migration to Expected Population in order to calculate Projected Population

# Step 9: Assemble Components of Change to check work (Optional)



