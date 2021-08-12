# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/PopData.Rdata")
load("Output/Mort_Proj.Rdata")
load("Output/ASFR.Rdata")
load("Output/Base_Migration.Rdata")
load("Output/BirthRatios.Rdata")

startyr = "2020"
midpointyr = "2022.5"
endyr = "2024"

under55 <- c('0 to 4 years', '5 to 9 years', '10 to 14 years', '15 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 to 49 years', '50 to 54 years')
over55 <- c('55 to 59 years', '60 to 64 years', '65 to 69 years', '70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over')

# Step 1: Age-Sex Specific Survival Rates, 2020-2050, Midpoints of 5-year Intervals

#get column names and use below to make cyclical
Mort_MidPoint <- Mort_Proj %>% mutate('Mort2022.5'=rowMeans(across('2020':'2025')),
                                      'Mort2027.5'=rowMeans(across('2025':'2030')),
                                      'Mort2032.5'=rowMeans(across('2030':'2035')),
                                      'Mort2037.5'=rowMeans(across('2035':'2040')),
                                      'Mort2042.5'=rowMeans(across('2040':'2045')),  #we don't need to go out to 2060 but we have the data to do so
                                      'Mort2047.5'=rowMeans(across('2045':'2050'))) %>%
                                       select(-c(4:13))

#Mort_MidPoint <- Mort_MidPoint %>%
# select(c(1:3) | ends_with(midpointyr))


Mort_Midpoint_Year <- names(Mort_MidPoint[,4:9]) %>% as_tibble() %>% rename('Midpoint' = 'value')



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

ASFR_Midpoint_Year <- ASFR_MidPoint %>% select(c(1:2) | ends_with(".5"))
ASFR_Midpoint_Year <- names(ASFR_Midpoint_Year[,3:8]) %>% as_tibble() %>% rename('Midpoint' = 'value')



#Step 3: Pull in 2020 PEP data

PEP2020 <- POP[["2020"]] %>% select(-County,-State) %>%
  group_by(Age, Region, Sex) %>% summarise(Pop2020 = sum(Population))

agefactors <- unique(POP[["2020"]]$Age) %>% factor(ordered=TRUE) %>% fct_relevel("5 to 9 years", after = 1)


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
  pivot_longer(cols=c("Female","Male"), names_to = "Sex", values_to = "Pop2025") %>% mutate(Age = "0 to 4 years") #%>%
#rename(Age2025 = Age)


# Step 5: apply Survival Rates and calculate Expected 2025 population

PEP2020 <- PEP2020 %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>% select(-x)


expectedpop25 <- projectedBirths_0to4surviving %>%
  ungroup() %>%
  full_join(PEP2020, by=c("Region", "Sex", "Age")) %>%
  relocate(c(Age, Pop2020), .before= Pop2025) %>%
  left_join(Mort_MidPoint, by=c('Region', 'Age','Sex')) %>% select(Region, Sex, Age, Pop2020, Mort2022.5, Pop2025) %>%
  arrange(Region, desc(Sex)) %>%
  mutate(Pop2025 = case_when(!Age %in% c('0 to 4 years', '85 years and over') ~ lag(Pop2020) * Mort2022.5, #multiply prior 2020 age group population by survival rate for current age group
                             Age == '85 years and over' ~ (Pop2020 + lag(Pop2020))* Mort2022.5,
                             TRUE ~ Pop2025)) %>%
  select(-Pop2020) #drop Pop 2020 column so it doesn't cause confusion later on


# Step 6: Import historical Net Migration values, calculate Target Net Migrants, calculate K factors

NetMig <- read_excel("Input/NetMigration_Berger.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)

target_NM <- NetMig %>% filter(Age == 'Total' & Sex == 'Both') %>% select(-Period) %>%
  group_by(Region) %>% summarise(NetMigration = round (mean(NetMigration),-3)) #round to nearest thousand

#Apportioning Target Net Migrants to Males and Females, Then to Broad Age Groups

#Target TM by sex
TM_Sex <- NetMig %>% filter(Period %in% c('2005-2010', '2014-2018'), Age == 'Total', Sex %in% c('Male', 'Female')) %>%
  group_by(Sex, Region) %>% mutate(NetTotal = sum(NetMigration)) %>% select(-NetMigration) %>%
  group_by(Period, Region) %>% mutate(SexProp = NetTotal / sum(NetTotal)) %>%
  full_join(target_NM, by='Region') %>% mutate(TargetTM = SexProp*NetMigration)


#Target TM <55 / 55+ by sex
TM_55 <- NetMig %>% filter(Period %in% c('2005-2010', '2014-2018'), Age == '55+', Sex %in% c('Male', 'Female')) %>%
  group_by(Sex, Region) %>% mutate(NetTotal2 = sum(NetMigration)) %>% select(-NetMigration) %>%
  group_by(Period, Region) %>% left_join(TM_Sex, by=c('Region', 'Period', 'Sex')) %>% select(-Age.y, -SexProp) %>%
  mutate(SexProp = NetTotal2/NetTotal) %>% select(-NetTotal2, -NetTotal) %>%
  mutate(TargetTM_55Plus = TargetTM*SexProp) %>%
  mutate(TargetTM_U55 = TargetTM - TargetTM_55Plus) %>% select(-NetMigration, -TargetTM, - SexProp, -Age.x) %>%
  ungroup() %>% select(-Period) %>% unique()


TM_Sex <- TM_Sex %>% ungroup() %>% select(-Period) %>% unique() %>% select(Region, Sex, Age, TargetTM)


#Net Migrants from prior5-year period

NM_55Plus <-  NetMig %>% filter(Period == '2014-2018', Age == '55+', Sex %in% c('Male', 'Female')) %>%
  group_by(Region, Sex) %>% rename(NM_O55 = NetMigration)

NM_Under55 <- NetMig %>% filter(Period == '2014-2018', Age == 'Total', Sex %in% c('Male', 'Female')) %>%
  group_by(Region, Sex) %>% full_join(NM_55Plus, by=c('Region', 'Period', 'Sex')) %>%
  mutate(NM_U55 = NetMigration - NM_O55) %>%
  select(-Age.x, -Age.y, -NetMigration, -NM_O55) %>%
  mutate(Age = 'Under 55')


#Change in net migrants from prior 5-year period
NM_Change_Prior_under55 <- full_join(TM_55, NM_Under55, by=c('Region', 'Sex')) %>%
  mutate(NM_Change_U55 = TargetTM_U55 - NM_U55) %>% select(Period, Region, Sex, Age, NM_Change_U55)

NM_Change_Prior_over55 <- full_join(TM_55, NM_55Plus, by=c('Region', 'Sex')) %>%
  mutate(NM_Change_U55 = TargetTM_55Plus - NM_O55) %>% select(Period, Region, Sex, Age, NM_Change_U55) %>%
  mutate(Age = 'Over 55')

#Expected Populations of Current Period
expectedpop_under55 <- expectedpop25 %>% select(-Mort2022.5) %>% filter(Age %in% under55) %>%
  group_by(Region, Sex) %>% replace(is.na(.), 0) %>% mutate(Pop2025 = sum(Pop2025)) %>% select( -Age) %>% distinct() %>%
  mutate(Age = 'Under 55')

expectedpop_over55 <- expectedpop25 %>% select(-Mort2022.5) %>% filter(Age %in% over55) %>%
  group_by(Region, Sex) %>% mutate(Pop2025 = sum(Pop2025)) %>% select( -Age) %>% distinct() %>%
  mutate(Age = 'Over 55')

#Change in Net Migration Rates (K) from Prior Period
K_Under55 <- full_join(NM_Change_Prior_under55, expectedpop_under55, by=c('Region', 'Sex', 'Age')) %>%
  mutate(k_under55 = NM_Change_U55/Pop2025) %>% select(Period, Region, Sex, Age, k_under55)

K_Over55 <- full_join(NM_Change_Prior_over55, expectedpop_over55, by=c('Region', 'Sex', 'Age')) %>%
  mutate(k_over55 = NM_Change_U55/Pop2025) %>% select(Period, Region, Sex, Age, k_over55)


# Step 7: Apply K factors to NMRs in order to calculate Net Migration

# Step 8: Apply Net Migration to Expected Population in order to calculate Projected Population

# Step 9: Assemble Components of Change to check work (Optional)




