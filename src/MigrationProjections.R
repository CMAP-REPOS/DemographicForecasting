# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/Mort_Proj.Rdata")
load("Output/ASFR.Rdata")
load("Output/BirthRatios.Rdata")

#load in variables from projection_control
baseyear = "2020"
startyr = "2020"                              #as.character(projstart)
midpointyr = "2022.5"                         #as.character(projmidpoint)
endyr = "2024"                                #as.character(projend - 1)
cycleyears = c(2020,2021,2022,2023,2024)      #projyears
lastyear = as.character(max(cycleyears))

if(startyr == baseyear){
  load("Output/PopData.Rdata")
  load("Output/Base_Migration.Rdata")
}else{
  #add in location/filename to load non-base year Pop and NMRs
}



under55 <- c('0 to 4 years', '5 to 9 years', '10 to 14 years', '15 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 to 49 years', '50 to 54 years')
over55 <- c('55 to 59 years', '60 to 64 years', '65 to 69 years', '70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over')

# Step 1: Age-Sex Specific Survival Rates, 2020-2050, Midpoints of 5-year Intervals

#get column names
Mort_MidPoint <- Mort_Proj %>% mutate('Mort2022.5'=rowMeans(across('2020':'2025')),
                                  'Mort2027.5'=rowMeans(across('2025':'2030')),
                                  'Mort2032.5'=rowMeans(across('2030':'2035')),
                                  'Mort2037.5'=rowMeans(across('2035':'2040')),
                                  'Mort2042.5'=rowMeans(across('2040':'2045')),  #we don't need to go out to 2060 but we have the data to do so
                                  'Mort2047.5'=rowMeans(across('2045':'2050'))) %>%
                                   select(-c(4:13))

Mort_MidPoint <- Mort_MidPoint %>%
select(c(1:3) | ends_with(midpointyr))


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
ASFR_MidPoint <- ASFR_MidPoint %>%
  select(c(1:2) | contains(midpointyr) | num_range("ASFR", cycleyears))


#Step 3: Pull in Base Year Population Data

PEP2020 <- POP[["2020"]] %>% select(-County,-State) %>%
  group_by(Age, Region, Sex) %>% summarise(Pop2020 = sum(Population))

#pull age groups, make ordered factors list with proper sorting
agefactors <- unique(POP[["2020"]]$Age) %>% factor(ordered=TRUE) %>% fct_relevel("5 to 9 years", after = 1)
#check levels
agefactors

# Step 4 part 1: Calculate projected Births by age cohort and Region in 1-year intervals

F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")

projectedBirths <- PEP2020 %>%
  filter(Sex == "Female", Age %in% F_Groups) %>%
  full_join(ASFR_MidPoint, by = c("Age", "Region")) %>%
  mutate(Births2020 = Pop2020 * ASFR2020,    #consider replacing with a matrix multiplication function
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
  left_join(bRatios, by="Region") %>%
  mutate(fBirths = totBirths*Female,
         mBirths = totBirths*Male, .keep = "unused")

#pull and rearrange 0-1 and 1-4 Survival Rates by sex and Region from Mort_MidPoint
Mort_0to4 <- Mort_MidPoint %>%
  filter(Age == "0 to 1 years" | Age == "1 to 4 years") %>%
  pivot_wider(names_from = c("Sex","Age"), values_from = starts_with("Mort"))
names(Mort_0to4) <- make.names(names(Mort_0to4))

# Step 4 part 3: calculate survivors by sex and year, then sum for total number of survivors by Region
endyearBirths <- projectedBirths_bySex$Year %>% str_subset(lastyear) %>% unique()

projectedBirths_0to4surviving <- projectedBirths_bySex %>%
  left_join(Mort_0to4, by="Region") %>%
  mutate(fSurvivors = case_when(Year == endyearBirths ~ fBirths * Female_0.to.1.years,
                                TRUE ~ fBirths * Female_0.to.1.years * Female_1.to.4.years),
         mSurvivors = case_when(Year == endyearBirths ~ mBirths * Male_0.to.1.years,
                                TRUE ~ mBirths * Male_0.to.1.years * Male_1.to.4.years)) %>%
  group_by(Region) %>%
  summarize(Female = round(sum(fSurvivors),0), Male = round(sum(mSurvivors),0)) %>%
  pivot_longer(cols=c("Female","Male"), names_to = "Sex", values_to = "Pop2025") %>% mutate(Age = "0 to 4 years")

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
                                    TRUE ~ Pop2025)) #%>%
  #select(-Pop2020) #drop Pop 2020 column so it doesn't cause confusion later on


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
  mutate(kfactor = NM_Change_U55/Pop2025) %>% select(Period, Region, Sex, Age, kfactor) %>% unique()

K_Over55 <- full_join(NM_Change_Prior_over55, expectedpop_over55, by=c('Region', 'Sex', 'Age')) %>%
  mutate(kfactor = NM_Change_U55/Pop2025) %>% select(Period, Region, Sex, Age, kfactor) %>% unique()

K_factors <- bind_rows(K_Under55, K_Over55) %>%
  select(-Period) %>%
  pivot_wider(names_from = Age, values_from = c('kfactor')) %>%
 # pivot_wider(names_from = Age, values_from = c('Female', 'Male')) %>%
  rename_with(make.names)


# Step 7: Apply K factors to NMRs in order to calculate Net Migration

Migration <- Base_Mig %>% select(Region, Age, Sex, NetRates) %>%
  left_join(K_factors, by=c('Region', 'Sex')) %>%
  pivot_wider(names_from = "Sex", values_from=c("NetRates", "Under.55", "Over.55")) %>%
  rename(K_U55_Female = Under.55_Female, K_O55_Female = Over.55_Female, K_U55_Male = Under.55_Male, K_O55_Male = Over.55_Male) %>%
  arrange(Region) %>%

  mutate(Male_NMR = case_when(Age %in% c('0 to 4 years', '5 to 9 years', '10 to 14 years') ~ (NetRates_Female + NetRates_Male + K_U55_Male + K_U55_Female)/2,
                              Age %in% c('15 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 to 49 years', '50 to 54 years') ~ NetRates_Male + K_U55_Male,
                              Age %in% c('55 to 59 years', '60 to 64 years', '65 to 69 years', '70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over') ~ NetRates_Male + K_O55_Male)) %>%

  mutate(Male_NMR = case_when(Age == "50 to 54 years" ~ (lead(Male_NMR) + lag(Male_NMR))/2, #why is this not calculating as negative??
                              TRUE ~ Male_NMR)) %>%

  mutate(Female_NMR = case_when(Age %in% c('0 to 4 years', '5 to 9 years', '10 to 14 years') ~ (NetRates_Female + NetRates_Male + K_U55_Male + K_U55_Female)/2,
                                Age %in% c('15 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 to 49 years', '50 to 54 years') ~ NetRates_Female + K_U55_Female,
                                Age %in% c('55 to 59 years', '60 to 64 years', '65 to 69 years', '70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over') ~ NetRates_Female + K_O55_Female)) %>%

  mutate(Male_NMR = case_when(Age == "50 to 54 years" ~ (lead(Female_NMR) + lag(Female_NMR))/2, #why is this not calculating as negative??
                              TRUE ~ Male_NMR))


# Step 8: Apply Net Migration to Expected Population in order to calculate Projected Population

Projections <- Migration %>% pivot_longer(cols = ends_with("NMR"),
                                names_to = "Sex",
                                values_to = "NMRs") %>% select(Region, Age, Sex, NMRs)

Projections$Sex <- substr(Projections$Sex,1,nchar(Projections$Sex)-4)

M <- Projections %>% left_join(expectedpop25, by=c("Region", "Age", "Sex")) %>% select(-Mort2022.5) %>% relocate(c(Pop2020, Pop2025), .before=NMRs)



#need to rewrite below code to work for both M/F

Projections_Male <- expectedpop25 %>% select(-Mort2022.5) %>%
 # pivot_wider(names_from = c("Sex"), values_from=c("Pop2020", "Pop2025")) %>%
 #                   select(-Pop2020_Female, -Pop2025_Female) %>%
                    #left_join(Migration %>% select(Region, Age, Male_NMR), by=c("Region","Age")) %>%
                    mutate(NMs_Living =  Pop2025_Male * Male_NMR) %>%
                    mutate(NMs_Living_Abs =  abs(Pop2025_Male * Male_NMR)) %>% group_by(Region) %>%
                   # rename(ExpPop2025_Male = Pop2025_Male)

sum_NM <- Projections_Male %>% filter(Age %in% under55) %>% group_by(Region) %>% summarise(sum_NM = sum(NMs_Living))
sum_NM_Abs <- Projections_Male %>% filter(Age %in% under55) %>% group_by(Region) %>% summarise(sum_NM_Abs = sum(NMs_Living_Abs))
Male_K <- TM_55 %>% filter(Sex == 'Male') %>% select(-TargetTM_55Plus, -Sex)


Projections_Male <- Projections_Male %>% full_join(sum_NM, by='Region') %>% full_join(sum_NM_Abs, by="Region") %>%
  full_join(Male_K, by= 'Region')


Projections_Male <- Projections_Male %>% mutate(migrants_living = (NMs_Living_Abs/sum_NM_Abs)*(sum_NM-TargetTM_U55)+NMs_Living) %>%
                                         mutate(pop2025 = round(ExpPop2025_Male + migrants_living,-1))

Births_2020 <- projectedBirths_bySex %>% select(-Year) %>% group_by(Region) %>% mutate(fBirths = sum(fBirths), mBirths = sum(mBirths)) %>% select(-fBirths) %>% distinct()

#good up to here
Projections_Male <- Projections_Male %>% full_join(Births_2020, by='Region')

Projections_Male <- Projections_Male %>%
                    mutate(Deaths = case_when(!Age %in% c('0 to 4 years', '85 years and over') ~ (Pop2020_Male - lead(pop2025)),
                                               Age == '85 years and over' ~ (Pop2020_Male + lag(Pop2020_Male))- pop2025,
                                               TRUE ~ 0))

# Step 9: Assemble Components of Change to check work (Optional)




