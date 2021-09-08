# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/Mort_Proj.Rdata")    #named Mort_Proj
load("Output/ASFR.Rdata")         #named ASFR_projections
load("Output/BirthRatios.Rdata")  #named bRatios

#load in variables from projection_control
baseyr = as.character(baseyear)    #"2020"
startyr = as.character(projstart)  #"2020"
midpointyr = as.character(projmidpoint)  #"2022.5"
endyr = as.character(projend)  #"2025"
cycleyears = projyears # c(2020,2021,2022,2023,2024)
lastyear = as.character(max(cycleyears))


if(startyr == baseyr){
  print(paste("GENERATING", baseyr, "PROJECTION"))
  print(paste("USING", tNMfile, "TARGET MIGRATION VALUES"))

#Load in and reformat population data
  load("Output/PopData.Rdata")  # named POP

#Import the baseyear population data (2020)
baseyearpoptable <- POP[[baseyr]] %>%
  group_by(Age, Region, Sex) %>% summarise(baseyrpop = sum(Population)) %>%
  ungroup()
print(baseyearpoptable[1:3,])

  #sort population by age group
  baseyearpoptable <- baseyearpoptable %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>% select(-x)

#Load in and reformat base year migration rate data
  load("Output/Base_Migration.Rdata") # named Base_Mig
  Base_Mig <- Base_Mig %>% select(Region, Age, Sex, NetRates)

  # Load in 1991-95 base net migration rates (Berger)
  #Base_Mig <- read.csv("C:/Users/amcadams/Documents/R/base_mig_91-95.csv") %>%
  #  select(-NetRates) %>%
  #  rename(NetRates = NetRates_91.95)


}else{
  print(paste("GENERATING", max(cycleyears)+1, "PROJECTION"))
  print(paste("USING", tNMfile, "TARGET MIGRATION VALUES"))

#Load in population data

  baseyearpoptable <- POPPROJ[[startyr]]
  names(baseyearpoptable) <- c("Age","Region","Sex","baseyrpop")
  #sort the pop table (may not be necessary, but it's here just in case)
  baseyearpoptable <- baseyearpoptable %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>% select(-x)

#load in Migration data
  Base_Mig <- NETMIGPROJ[[startyr]]
  names(Base_Mig) <- c("Region","Age","Sex","NetRates")


}


###### other definitions

#used for Births calculation
F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")

#used for k factor calculations:
under55 <- c('0 to 4 years', '5 to 9 years', '10 to 14 years', '15 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 to 49 years', '50 to 54 years')
over55 <- c('55 to 59 years', '60 to 64 years', '65 to 69 years', '70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over')

# Step 1: Age-Sex Specific Survival Rates, 2020-2050, Midpoints of 5-year Intervals

Mort_MidPoint <- Mort_Proj %>%
select(c(1:3) | ends_with(midpointyr))


# Step 2: Age Specific Fertility Rate Projections, Midpoints of 5-year Intervals, 2020-2050

ASFR_MidPoint <- ASFR_projections %>%
  select(c(1:2) | contains(midpointyr) | num_range("ASFR", cycleyears))

# Step 3: Special Handling for Calculating Predicted Births and Infant Survival (ages 0-4)

# Step 3 Part 1: Calculate projected Births by Age Cohort and Region in 1-year intervals

#grab possible Mother population (females 15-44) and join to ASFRs
projectedBirths <- baseyearpoptable %>%
  filter(Sex == "Female", Age %in% F_Groups) %>%
  full_join(ASFR_MidPoint, by = c("Age", "Region"))

#calculate expected births by one year increments by multiplying base population * ASFR for year of interest, then summarize total births by Region and Year
projectedBirths <- bind_cols(projectedBirths[1:2], projectedBirths$baseyrpop * projectedBirths[, 6:10]) %>%
  pivot_longer(cols=starts_with("ASFR"), names_to = "Year", values_to = "totBirths") %>%
  mutate(Year = paste("Births", str_sub(Year, start = -4), sep="")) %>%
  group_by(Region, Year) %>%
  summarise(totBirths = sum(totBirths)) %>%
  ungroup()

# Step 3 part 2: Calculate the number of Births (by Sex and Region) that survive the projection period.
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

# Step 3 part 3: calculate survivors by sex and year, then sum for total number of survivors by Region
endyearBirths <- projectedBirths_bySex$Year %>% str_subset(lastyear)

projectedBirths_0to4surviving <- projectedBirths_bySex %>%
  left_join(Mort_0to4, by="Region") %>%
  mutate(fSurvivors = case_when(Year == endyearBirths ~ fBirths * Female_0.to.1.years,
                                TRUE ~ fBirths * Female_0.to.1.years * Female_1.to.4.years),
         mSurvivors = case_when(Year == endyearBirths ~ mBirths * Male_0.to.1.years,
                                TRUE ~ mBirths * Male_0.to.1.years * Male_1.to.4.years)) %>%
  group_by(Region) %>%
  summarize(Female = round(sum(fSurvivors),0), Male = round(sum(mSurvivors),0)) %>%
  pivot_longer(cols=c("Female","Male"), names_to = "Sex", values_to = "ProjectedPop") %>% mutate(Age = "0 to 4 years")

earlyDeaths <- projectedBirths_bySex %>% pivot_longer(cols=c(3:4), names_to = "Sex", values_to = "Births") %>%
  group_by(Region, Sex) %>%
  summarize(totbirths = sum(Births)) %>%
  mutate(Sex = case_when(Sex == "fBirths" ~ "Female",
                         TRUE ~ "Male")) %>%
  left_join(projectedBirths_0to4surviving, by=c("Region","Sex")) %>%
  mutate(earlyDeaths = round(totbirths - ProjectedPop ,0)) %>%
  select(Region, Sex, Age, earlyDeaths)


# Step 4: apply Survival Rates and calculate Expected 2025 population

expectedpop <- baseyearpoptable %>%
  arrange(Region, desc(Sex)) %>%
  left_join(Mort_MidPoint, by=c('Region', 'Age','Sex')) %>% unique()

names(expectedpop) <- c("Age", "Region", "Sex", "baseyrpop", "Mort")

expectedpop <- expectedpop %>%
  mutate(ProjectedPop = case_when(!Age %in% c('0 to 4 years', '85 years and over') ~ lag(baseyrpop) * Mort, #multiply prior 2020 age group population by survival rate for current age group
                                Age == '85 years and over' ~ (baseyrpop + lag(baseyrpop))* Mort,
                                TRUE ~ NA_real_) ) %>%
  select(-Mort) %>%
  left_join(projectedBirths_0to4surviving, by = c("Region","Sex","Age")) %>%
  mutate(ProjectedPop = case_when(is.na(ProjectedPop.x) ~ ProjectedPop.y,
                                  TRUE ~ ProjectedPop.x), .keep = "unused")

olderDeaths <- expectedpop %>% mutate(deaths = lag(baseyrpop) - ProjectedPop) %>%
  mutate(deaths = case_when(Age == "0 to 4 years" ~ 0.0,
                            Age == "85 years and over" ~ (baseyrpop + lag(baseyrpop)) - ProjectedPop,
                            TRUE ~ deaths)) %>%
  select(Age,Region,Sex,deaths)

# Step 5: Calculate K factors from Target Net Migrants value and previous Net Migration totals
NetMig <- read_excel("Input/NetMigration_Berger_Full.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)
NMperiods <- NetMig %>% pull(Period) %>% unique() %>% sort()
NMperiods <- tail(NMperiods, 2)
print(paste("Net Migration Allocation Periods:", NMperiods[1],"and", NMperiods[2] ,sep=" "))

#Apportioning Target Net Migrants to Males and Females, Then to Broad Age Groups

#filter out the correct Target Net Migrant numbers from the list and apply to the projection
target_NMlocal <- target_NM %>% filter(Year == endyr) %>% select(-Year) %>% rename(RegionNMT = NetMigration)



#Target TM by sex - updated this code 8/27 to fix calculation errors
TM_Sex <- NetMig %>% filter(Period %in% NMperiods, Age == 'Total', Sex %in% c('Male', 'Female')) %>%
  group_by(Sex, Region, Period) %>% mutate(NetTotal = sum(NetMigration)) %>% select(-NetMigration)

TM_Sex <- TM_Sex %>%
  group_by(Region, Sex) %>% mutate(Sum_NM = sum(NetTotal)) %>% group_by(Region, Period) %>%
  mutate(SexProp = abs(Sum_NM / sum(abs(Sum_NM)))) %>%
  full_join(target_NMlocal, by='Region') %>% mutate(TargetTM = SexProp*RegionNMT) %>%
  select(-RegionNMT) %>% rename(Agegrp = Age)

#Target TM <55 / 55+ by sex
TM_55 <- NetMig %>% filter(Period %in% NMperiods, Age == '55+', Sex %in% c('Male', 'Female')) %>%
  group_by(Sex, Region) %>% mutate(NetTotal2 = sum(NetMigration)) %>%
  group_by(Period, Region) %>% left_join(TM_Sex, by=c('Region', 'Period', 'Sex')) %>%
  mutate(SexProp = NetTotal2/Sum_NM) %>%
  mutate(TargetTM_55Plus = TargetTM*SexProp) %>%
  mutate(TargetTM_U55 = TargetTM - TargetTM_55Plus) %>%
  ungroup() %>% unique()

TM_Sex <- TM_Sex %>% ungroup() %>%
  select(Region, Sex, Agegrp, TargetTM) %>% unique()


#Net Migrants from prior5-year period

NM_55Plus <-  NetMig %>% filter(Period == NMperiods[2], Age == '55+', Sex %in% c('Male', 'Female')) %>%
  group_by(Region, Sex) %>% rename(NM_O55 = NetMigration)

NM_Under55 <- NetMig %>% filter(Period == NMperiods[2], Age == 'Total', Sex %in% c('Male', 'Female')) %>%
  group_by(Region, Sex) %>% full_join(NM_55Plus, by=c('Region', 'Period', 'Sex')) %>%
  mutate(NM_U55 = NetMigration - NM_O55) %>%
  select(-Age.x, -Age.y, -NetMigration, -NM_O55) %>%
  mutate(Age = 'Under 55')

#Change in net migrants from prior 5-year period
NM_Change_Prior_under55 <- inner_join(TM_55, NM_Under55, by=c('Region', 'Sex', 'Period')) %>%
  mutate(NM_Change_U55 = TargetTM_U55 - NM_U55) %>% select(Period, Region, Sex, NM_Change_U55) %>% mutate(Age = "Under 55")

NM_Change_Prior_over55 <- inner_join(TM_55, NM_55Plus, by=c('Region', 'Sex','Period')) %>%
  mutate(NM_Change_U55 = TargetTM_55Plus - NM_O55) %>% select(Period, Region, Sex, NM_Change_U55) %>%
  mutate(Age = 'Over 55')



#Expected Populations of Current Period
expectedpop_under55 <- expectedpop %>% filter(Age %in% under55) %>%
                      group_by(Region, Sex) %>% mutate(ProjectedPop = sum(na.omit(ProjectedPop))) %>% distinct() %>%
                      mutate(Age = 'Under 55')

expectedpop_over55 <- expectedpop %>% filter(Age %in% over55) %>%
                      group_by(Region, Sex) %>% mutate(ProjectedPop = sum(na.omit(ProjectedPop))) %>% distinct() %>%
                      mutate(Age = 'Over 55')

#Change in Net Migration Rates (K) from Prior Period
K_Under55 <- full_join(NM_Change_Prior_under55, expectedpop_under55, by=c('Region', 'Sex', 'Age')) %>%
  mutate(kfactor = NM_Change_U55/ProjectedPop) %>% select(Period, Region, Sex, Age, kfactor) %>% unique()

K_Over55 <- full_join(NM_Change_Prior_over55, expectedpop_over55, by=c('Region', 'Sex', 'Age')) %>%
  mutate(kfactor = NM_Change_U55/ProjectedPop) %>% select(Period, Region, Sex, Age, kfactor) %>% unique()

K_factors <- bind_rows(K_Under55, K_Over55) %>%
  select(-Period) %>%
  pivot_wider(names_from = Age, values_from = c('kfactor')) %>%
  rename_with(make.names)

# Step 6: Apply K factors to NMRs in order to calculate Net Migration

Migration <- Base_Mig %>%
  left_join(K_factors, by=c('Region', 'Sex')) %>%
  pivot_wider(names_from = "Sex", values_from=c("NetRates", "Under.55", "Over.55")) %>%
  rename(K_U55_Female = Under.55_Female, K_O55_Female = Over.55_Female, K_U55_Male = Under.55_Male, K_O55_Male = Over.55_Male) %>%
  arrange(Region) %>%

  mutate(Male_NMR = case_when(Age %in% c('0 to 4 years', '5 to 9 years', '10 to 14 years') ~ (NetRates_Female + NetRates_Male + K_U55_Male + K_U55_Female)/2,
                              Age %in% c('15 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 to 49 years', '50 to 54 years') ~ NetRates_Male + K_U55_Male,
                              Age %in% c('55 to 59 years', '60 to 64 years', '65 to 69 years', '70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over') ~ NetRates_Male + K_O55_Male)) %>%

  mutate(Male_NMR = case_when(Age == "50 to 54 years" ~ (lead(Male_NMR) + lag(Male_NMR))/2,
                              TRUE ~ Male_NMR)) %>%

  mutate(Female_NMR = case_when(Age %in% c('0 to 4 years', '5 to 9 years', '10 to 14 years') ~ (NetRates_Female + NetRates_Male + K_U55_Male + K_U55_Female)/2,
                                Age %in% c('15 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 to 49 years', '50 to 54 years') ~ NetRates_Female + K_U55_Female,
                                Age %in% c('55 to 59 years', '60 to 64 years', '65 to 69 years', '70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over') ~ NetRates_Female + K_O55_Female)) %>%

  mutate(Male_NMR = case_when(Age == "50 to 54 years" ~ (lead(Female_NMR) + lag(Female_NMR))/2,
                              TRUE ~ Male_NMR)) %>%
  select(1:2 | ends_with("NMR")) %>%
  pivot_longer(cols = ends_with("NMR"), names_to = "Sex", values_to = "NMRs") %>%
  mutate(Sex = case_when(Sex == "Male_NMR" ~ "Male",
                         TRUE ~ "Female"))


# Step 7: Apply Net Migration to Expected Population in order to calculate Projected Population

Projections <- Migration %>% left_join(expectedpop, by=c("Region", "Age", "Sex")) %>% #relocate(c(Pop2020, Pop2025), .before=NMRs) %>%
                      mutate(NMs_Living =  ProjectedPop * NMRs)  %>%
                      mutate(NMs_Living_Abs =  abs(ProjectedPop * NMRs))

sum_NM <- Projections %>% filter(Age %in% under55) %>% group_by(Region, Sex) %>% summarise(sum_NM = sum(NMs_Living))
sum_NM_Abs <- Projections %>% filter(Age %in% under55) %>% group_by(Region, Sex) %>% summarise(sum_NM_Abs = sum(NMs_Living_Abs))
Target_TM_U55 <- TM_55 %>% select(Region, Sex, TargetTM_U55) %>% unique()

Projections <- Projections %>% left_join(sum_NM, by=c('Region', 'Sex')) %>% left_join(sum_NM_Abs, by=c("Region", "Sex")) %>%
  left_join(Target_TM_U55 , by=c('Region', 'Sex'))

Projections <- Projections %>% mutate(projNetMigrants = round((NMs_Living_Abs/sum_NM_Abs)*(TargetTM_U55-sum_NM)+NMs_Living),0) %>%
  mutate(ProjectedPop_final = round(ProjectedPop + projNetMigrants,0))

Migrants <- Projections %>% select(Region, Age, Sex, projNetMigrants)


#calculate Total Migration by Sex and +/-55, add to NetMig table

#tempNetMig1 <- Projections %>%
#  group_by(Region,Sex) %>%
#  summarise(NetMigration = round(sum(projNetMigrants),0)) %>%
#  mutate(agegrp = "Total")

#tempNetMig2 <- tempNetMig1 %>%
#  group_by(Region) %>%
#  summarise(NetMigration = sum(NetMigration)) %>%
#  mutate(Sex = "Both", agegrp = "Total")

#totNM <- Projections %>%
#  filter(Age %in% over55) %>%
#  mutate(agegrp = "55+") %>%
#  group_by(Region,Sex,agegrp) %>%
#  summarise(NetMigration = round(sum(projNetMigrants),0)) %>%
#  bind_rows(tempNetMig1) %>%
#  bind_rows(tempNetMig2) %>%
#  rename(Age = agegrp) %>%
#  mutate(Period = paste(cycleyears[1], cycleyears[5]+1, sep="-"))

#NetMig <- NetMig %>% bind_rows(totNM)



# Step 8: Save Population projection, Net Migration rates, and Total Migration

Projections <- Projections %>%
  select(Age, Region, Sex, ProjectedPop_final)

Migration <- Migration



# Step 9: Assemble Components of Change for each Region (Optional)

#Births (by Sex)
projectedBirths_reformat <- projectedBirths_bySex %>% pivot_longer(cols=c(3:4), names_to = "Sex", values_to = "Births") %>%
  group_by(Region, Sex) %>%
  summarize(componentValue = round(sum(Births),0)) %>%
  mutate(Sex = case_when(Sex == "fBirths" ~ "Female",
                         TRUE ~ "Male"),
         Age = NA,
         componentType = "Births")

#Total Migrants
Migrants <- Migrants %>%
  rename(NetMigrants = projNetMigrants)

migrantDeaths <- left_join(Migrants, Mort_MidPoint, by=c("Region", "Sex", "Age")) %>%
  rename(Mort = starts_with("Mort")) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(Region, Sex, x) %>% select(-x) %>%
  mutate(Mort = case_when(is.na(Mort) ~ lead(Mort),  #ballparking the 0-4 migrant survival rate - using 5 to 9 rate instead
                          TRUE ~ Mort)) %>%
  mutate(migdeaths = case_when(Age == "0 to 4 years" ~ round((NetMigrants/((Mort+1)/2) - NetMigrants),0),
                                TRUE ~ round((NetMigrants/((lag(Mort)+1)/2) - NetMigrants),0)    )) %>%
  select(Region, Age, Sex, migdeaths)

#Resident Deaths (by Sex, by age, and including deaths of Net Migrants)
projectedDeaths <- left_join(olderDeaths, earlyDeaths,by=c("Region", "Sex", "Age")) %>%
  mutate(Deaths = round(case_when(deaths == 0.0 ~ earlyDeaths,
                            TRUE ~ deaths),0), .keep = "unused") %>%
  left_join(migrantDeaths, by=c("Region", "Sex", "Age")) %>%
  mutate(Deaths = Deaths + migdeaths, .keep = "unused")

#put births, deaths and migration all together in long table in order to save it in COMPONENT list
Components <- left_join(Migrants, projectedDeaths, by = c("Region", "Sex", "Age")) %>%
  pivot_longer(cols = c(4:5), names_to = "componentType", values_to = "componentValue") %>%
  bind_rows(projectedBirths_reformat)

