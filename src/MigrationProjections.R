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

  baseyearpoptable <- POP[[baseyr]] %>%
    group_by(Age, Region, Sex) %>% summarise(baseyrpop = sum(Population)) %>%
    ungroup()
  #sort the population by age group
  baseyearpoptable <- baseyearpoptable %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>% select(-x)

#Load in and reformat base year migration rate data
  load("Output/Base_Migration.Rdata") # named Base_Mig

  Base_Mig <- Base_Mig %>% select(Region, Age, Sex, NetRates)

}else{
  print(paste("GENERATING", max(cycleyears)+1, "PROJECTION"))
  print(paste("USING", tNMfile, "TARGET MIGRATION VALUES"))

#Load in population data
  load("Output/PopProj.Rdata") #initial creation in projection_control for first cycle then updated each additional cycle

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

#calculate population * ASFR, pivot table to Long format, then summarize total births by Region and Year
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
endyearBirths <- projectedBirths_bySex$Year %>% str_subset(lastyear) %>% unique()

projectedBirths_0to4surviving <- projectedBirths_bySex %>%
  left_join(Mort_0to4, by="Region") %>%
  mutate(fSurvivors = case_when(Year == endyearBirths ~ fBirths * Female_0.to.1.years,
                                TRUE ~ fBirths * Female_0.to.1.years * Female_1.to.4.years),
         mSurvivors = case_when(Year == endyearBirths ~ mBirths * Male_0.to.1.years,
                                TRUE ~ mBirths * Male_0.to.1.years * Male_1.to.4.years)) %>%
  group_by(Region) %>%
  summarize(Female = round(sum(fSurvivors),0), Male = round(sum(mSurvivors),0)) %>%
  pivot_longer(cols=c("Female","Male"), names_to = "Sex", values_to = "ProjectedPop") %>% mutate(Age = "0 to 4 years")


# Step 4: apply Survival Rates and calculate Expected 2025 population

expectedpop <- baseyearpoptable %>%
  arrange(Region, desc(Sex)) %>%
  left_join(Mort_MidPoint, by=c('Region', 'Age','Sex'))

names(expectedpop) <- c("Age", "Region", "Sex", "baseyrpop", "Mort")

expectedpop <- expectedpop %>%
  mutate(ProjectedPop = case_when(!Age %in% c('0 to 4 years', '85 years and over') ~ lag(baseyrpop) * Mort, #multiply prior 2020 age group population by survival rate for current age group
                                Age == '85 years and over' ~ (baseyrpop + lag(baseyrpop))* Mort,
                                TRUE ~ NA_real_) ) %>%
  select(-Mort) %>%
  left_join(projectedBirths_0to4surviving, by = c("Region","Sex","Age")) %>%
  mutate(ProjectedPop = case_when(is.na(ProjectedPop.x) ~ ProjectedPop.y,
                                  TRUE ~ ProjectedPop.x), .keep = "unused")

# Step 5: Calculate K factors from Target Net Migrants value and previous Net Migration totals
NetMig <- read_excel("Input/NetMigration_Berger.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)
NMperiods <- NetMig %>% pull(Period) %>% unique() %>% sort()
NMperiods <- tail(NMperiods, 2)
print(paste("Net Migration Allocation Periods:", NMperiods[1],"and", NMperiods[2] ,sep=" "))

#Apportioning Target Net Migrants to Males and Females, Then to Broad Age Groups

#filter out the correct Target Net Migrant numbers from the list and apply to the projection
target_NMlocal <- target_NM %>% filter(Year == endyr) %>% select(-Year)

#Target TM by sex
TM_Sex <- NetMig %>% filter(Period %in% NMperiods, Age == 'Total', Sex %in% c('Male', 'Female')) %>%
  group_by(Sex, Region) %>% mutate(NetTotal = sum(NetMigration)) %>% select(-NetMigration) %>%
  group_by(Period, Region) %>% mutate(SexProp = NetTotal / sum(NetTotal)) %>%
  full_join(target_NMlocal, by='Region') %>% mutate(TargetTM = SexProp*NetMigration)

#Target TM <55 / 55+ by sex
TM_55 <- NetMig %>% filter(Period %in% NMperiods, Age == '55+', Sex %in% c('Male', 'Female')) %>%
  group_by(Sex, Region) %>% mutate(NetTotal2 = sum(NetMigration)) %>% select(-NetMigration) %>%
  group_by(Period, Region) %>% left_join(TM_Sex, by=c('Region', 'Period', 'Sex')) %>% select(-Age.y, -SexProp) %>%
  mutate(SexProp = NetTotal2/NetTotal) %>% select(-NetTotal2, -NetTotal) %>%
  mutate(TargetTM_55Plus = TargetTM*SexProp) %>%
  mutate(TargetTM_U55 = TargetTM - TargetTM_55Plus) %>% select(-NetMigration, -TargetTM, - SexProp, -Age.x) %>%
  ungroup() %>% select(-Period) %>% unique()


TM_Sex <- TM_Sex %>% ungroup() %>% select(-Period) %>% unique() %>% select(Region, Sex, Age, TargetTM)

#Net Migrants from prior5-year period

NM_55Plus <-  NetMig %>% filter(Period == NMperiods[2], Age == '55+', Sex %in% c('Male', 'Female')) %>%
  group_by(Region, Sex) %>% rename(NM_O55 = NetMigration)

NM_Under55 <- NetMig %>% filter(Period == NMperiods[2], Age == 'Total', Sex %in% c('Male', 'Female')) %>%
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
expectedpop_under55 <- expectedpop %>% filter(Age %in% under55) %>%
                      group_by(Region, Sex) %>% mutate(ProjectedPop = sum(ProjectedPop)) %>% select(-Age, -baseyrpop) %>% distinct() %>%
                      mutate(Age = 'Under 55')

expectedpop_over55 <- expectedpop %>% filter(Age %in% over55) %>%
                      group_by(Region, Sex) %>% mutate(ProjectedPop = sum(ProjectedPop)) %>% select(-Age, -baseyrpop) %>% distinct() %>%
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
Target_TM_U55 <- TM_55 %>% select(-TargetTM_55Plus)

Projections <- Projections %>% left_join(sum_NM, by=c('Region', 'Sex')) %>% left_join(sum_NM_Abs, by=c("Region", "Sex")) %>%
  left_join(Target_TM_U55 , by=c('Region', 'Sex'))

Projections <- Projections %>% mutate(projNetMigrants = (NMs_Living_Abs/sum_NM_Abs)*(TargetTM_U55-sum_NM)+NMs_Living) %>%
  mutate(ProjectedPop_final = round(ProjectedPop + projNetMigrants,0))


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



# Step 9: Assemble Components of Change to check work (Optional)



#Births_2020 <- projectedBirths_bySex %>% select(-Year) %>% group_by(Region) %>%
#mutate(fBirths = sum(fBirths), mBirths = sum(mBirths)) %>% distinct()


#Births2020 <- tibble(Region = c("CMAP Region", "CMAP Region", "External IL", "External IL", "External WI", "External WI", "External WI", "External WI"),
#                      Sex = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
#                      Births = c(263558, 252240, 24441, 22974, 23565, 19616, 14202, 13301))

#Projections <- Projections %>% left_join(Births2020, by=c('Region','Sex'))%>%  arrange(Region, Sex) %>%
#                    mutate(Deaths = case_when(!Age %in% c('0 to 4 years', '85 years and over') ~ round(lag(Pop2020) - Pop2025,0),
#                                               Age == '85 years and over' ~ round((Pop2020 + lag(Pop2020))- Pop2025,0),
#                                               Age == '0 to 4 years' ~ round(Births - Pop2025,0)))

# OK to use the survival rate for ages 1-4 in first part of equation, rather than recalculating 0-4.
#Mort_MidPoint <- Mort_MidPoint %>% mutate(Age = case_when(Age == '1 to 4 years' ~ '0 to 4 years',
#                                                          TRUE ~ Age)) %>% rename(Mort_Calc = Mort2022.5)

#Projections <- Projections %>% left_join(Mort_MidPoint, by=c("Sex", "Region", "Age"))

#Projections <- Projections %>% mutate(Migrant_Deaths = round((net_migrants25/((Mort_Calc+1)/2)-net_migrants25),0)) %>%
#                               mutate(Resident_Deaths = Deaths + Migrant_Deaths) %>%
#                               select(-sum_NM, -sum_NM_Abs, -TargetTM_U55, -Births, -Mort_Calc)





