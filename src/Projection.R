# CMAP | Alexis McAdams, Mary Weber | 8/2/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/Mort_Proj.Rdata")    #named Mort_Proj
load("Output/ASFR.Rdata")         #named ASFR_projections
load("Output/BirthRatios.Rdata")  #named bRatios

#ASFR Data source OVERRIDE
if(ASFRoverride == 1){
  ASFR_projections <- bergerpop <- read_excel("Input/berger_ASFRs.xlsx") %>%
    filter(Age != "10 - 14") %>%
    filter(Age != "45 - 49")
  print("ASFR Override in effect: Using Berger's Log Projection method")
}

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
  #
  load("Output/POP_PEP.Rdata")  # named POP

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

  start_Base_Mig <- Base_Mig


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
group1 <- c('0 to 4 years', '5 to 9 years', '10 to 14 years', '15 to 19 years', '20 to 24 years')
group2 <- c('25 to 29 years', '30 to 34 years', '35 to 39 years')
group3 <- c('40 to 44 years', '45 to 49 years', '50 to 54 years', '55 to 59 years', '60 to 64 years', '65 to 69 years')
group4 <- c('70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over')

Age_Groups <- list(group1, group2, group3, group4)

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
  summarize(totbirths = sum(Births), .groups = 'drop') %>%
  mutate(Sex = case_when(Sex == "fBirths" ~ "Female",
                         TRUE ~ "Male")) %>%
  left_join(projectedBirths_0to4surviving, by=c("Region","Sex")) %>%
  mutate(earlyDeaths = round(totbirths - ProjectedPop ,0)) %>%
  select(Region, Sex, Age, earlyDeaths)

# Step 4: apply Survival Rates and calculate Expected 2025 population

expectedpop <- baseyearpoptable %>%
  arrange(Region, desc(Sex)) %>%
  left_join(Mort_MidPoint, by=c('Region', 'Age','Sex')) %>%
  ungroup()

names(expectedpop) <- c("Age", "Region", "Sex", "baseyrpop", "Mort")

expectedpop <- expectedpop %>%
  mutate(ProjectedPop = case_when(!Age %in% c("0 to 4 years", "85 years and over") ~ (lag(baseyrpop) * Mort), #multiply prior 2020 age group population by survival rate for current age group
                                  Age == '85 years and over' ~ (baseyrpop + lag(baseyrpop))* Mort,
                                  TRUE ~ NA_real_) ) %>%
  select(-Mort) %>%
  left_join(projectedBirths_0to4surviving, by = c("Region","Sex","Age")) %>%
  mutate(ProjectedPop = case_when(is.na(ProjectedPop.x) ~ ProjectedPop.y,
                                  TRUE ~ ProjectedPop.x), .keep = "unused")


olderDeaths <- expectedpop %>%
  mutate(deaths = case_when(Age == "0 to 4 years" ~ 0.0,
                            Age == "85 years and over" ~ (baseyrpop + lag(baseyrpop)) - ProjectedPop,
                            TRUE ~ lag(baseyrpop) - ProjectedPop)) %>%
  select(Age,Region,Sex,deaths)

# Step 5: Calculate K factors from Target Net Migrants value and previous Net Migration totals

NMperiods <- NetMig %>% pull(Period) %>% unique() %>% sort() #FYI: "NetMig" comes from line 60 of projection_control
NMperiods <- tail(NMperiods, 3)
print(paste("Net Migration Allocation Periods:", NMperiods[1],NMperiods[2],NMperiods[3],sep=" "))

#Apportioning Target Net Migrants to Males and Females, Then to Broad Age Groups

#filter out the correct Target Net Migrant numbers from the list and apply to the projection; update in projection_control.R
target_NMlocal <- target_NM %>% filter(Year == endyr) %>% select(-Year)

#NM by Sex (Excel rows 14-21)
NM_By_Sex <- NetMig %>% filter(Period %in% NMperiods, Sex %in% c('Male', 'Female'))

# NM calculations using base year periods (Excel rows 8-12)
BaseYears_NM <- NM_By_Sex %>% select(-Age) %>% group_by(Region, Sex, Period) %>%
  mutate(NetMigration = sum(NetMigration)) %>% distinct() %>% group_by(Sex, Region) %>%
  mutate(Sum_BaseYears = sum(NetMigration)) %>%
  select(-NetMigration, -Period) %>% distinct() %>%
  group_by(Region) %>%
  mutate(Prop_NM = Sum_BaseYears / sum(Sum_BaseYears)) %>%
  left_join(target_NMlocal, by = ('Region')) %>%
  mutate(target_next_cycle = Prop_NM * NetMigration)

temp5 <- BaseYears_NM
base_year_NM_check <- bind_rows(base_year_NM_check, temp5)

#Sum NM by Sex (Excel rows 23-27)
Sum_NM_Sex <- NM_By_Sex %>%
  group_by(Region, Sex, Age) %>% mutate(Sum_NM = sum(NetMigration)) %>%
  select(-NetMigration) %>% mutate(Period = 'Base Years') %>% distinct()

#NM Proportions (Excel rows 28-31)
NM_Proportions <- Sum_NM_Sex %>% group_by(Region, Sex) %>%
  mutate(SexProp = Sum_NM / sum(Sum_NM))

#Target Net Migrants by sex (Excel rows 33-36)
target_NM_Sex <- full_join(NM_Proportions, BaseYears_NM, by=c('Region', 'Sex')) %>%
  mutate(TargetNM = case_when(SexProp < 0 & NetMigration > 0 ~ abs(SexProp*target_next_cycle),
                              TRUE ~ SexProp*target_next_cycle))

temp4 <- target_NM_Sex
target_NM_Sex_check <- bind_rows(target_NM_Sex_check, temp4)

# Net Migrants from prior 5 year period (Excel rows 39-42). For 1st projection period, data comes from data crunched
# from 2014-2018. Subsequent years come from previous projection period. Totals are summed
# later in this script (lines XXX - XXXish)
if(startyr == baseyr){
  NM_Prior_Period <- NM_By_Sex %>% filter(Period %in% NMperiods[3]) #pulled from pastMigration_ageGroupSums.R

}else{
  NM_Prior_Period <- NM_Prior_Period2 %>%
    rename(Age = Age_Group)

}


#Change in net migrants from prior 5-year period (Excel 45-48)
NM_Change_Prior <- full_join(NM_Prior_Period, target_NM_Sex, by=c("Region", "Sex", "Age")) %>%
  select(Region, Sex, Age, TargetNM, NetMigration.x) %>%
  rename(NetMigration = NetMigration.x) %>%
  rowwise() %>%
  mutate(NM_Change = TargetNM - NetMigration) %>%
 select(Region, Sex, Age, NM_Change)


#Expected Populations of Current Period (Excel 51-54)
#Note: These numbers have changed since initial Excel model
sum_expectedPop <- expectedpop %>%
  mutate(Age_Group = case_when(Age %in% group1 ~ '0 to 24 years',
                               Age %in% group2 ~ '25 to 39 years',
                               Age %in% group3 ~ '40 to 69 years',
                               Age %in% group4 ~ '70 years and older')) %>%
  select(-Age, -baseyrpop) %>%
  rename(Age = Age_Group) %>%
  group_by(Region, Sex, Age) %>% mutate(ProjectedPop = sum(na.omit(ProjectedPop))) %>% distinct()


#Change in Net Migration Rates (K) from Prior Period (Excel 57-60)
K_factors <- full_join(sum_expectedPop, NM_Change_Prior, by=c('Region', 'Sex', 'Age')) %>%
  mutate(kfactor = NM_Change/ProjectedPop) %>% select(-ProjectedPop, -NM_Change) %>%
  pivot_wider(names_from = "Age", values_from=c("kfactor"))

# Step 6: Apply K factors to NMRs in order to calculate Net Migration

Migration <- Base_Mig %>%
  full_join(K_factors, by=c('Region', 'Sex')) %>%
  group_by(Region) %>%
  pivot_wider(names_from = "Sex", values_from=c("NetRates", "0 to 24 years", "25 to 39 years", "40 to 69 years", "70 years and older")) %>%
  rename(Male_0to24 = '0 to 24 years_Male',
         Male_25to39 = '25 to 39 years_Male',
         Male_40to69 = '40 to 69 years_Male',
         Male_70Plus = '70 years and older_Male',
         Female_0to24 = '0 to 24 years_Female',
         Female_25to39 = '25 to 39 years_Female',
         Female_40to69 = '40 to 69 years_Female',
         Female_70Plus = '70 years and older_Female')

Migration <- Migration %>% group_by(Region) %>%
  mutate(Male_NMR = case_when(Age %in% c('0 to 4 years', '5 to 9 years', '10 to 14 years') ~ (NetRates_Female + NetRates_Male + Male_0to24 + Female_0to24)/2,
                              Age %in% c('15 to 19 years', '20 to 24 years') ~ NetRates_Male + Male_0to24,
                              Age %in% c('25 to 29 years', '30 to 34 years', '35 to 39 years') ~ NetRates_Male + Male_25to39,
                              Age %in% c('40 to 44 years', '45 to 49 years', '50 to 54 years', '55 to 59 years', '60 to 64 years', '65 to 69 years') ~ NetRates_Male + Male_40to69,
                              TRUE ~ NetRates_Male + Male_70Plus)) %>%
   mutate(Female_NMR = case_when(Age %in% c('0 to 4 years', '5 to 9 years', '10 to 14 years') ~ (NetRates_Female + NetRates_Male + Male_0to24 + Female_0to24)/2,
                               Age %in% c('15 to 19 years', '20 to 24 years') ~ NetRates_Female + Female_0to24,
                               Age %in% c('25 to 29 years', '30 to 34 years', '35 to 39 years') ~ NetRates_Female + Female_25to39,
                               Age %in% c('40 to 44 years', '45 to 49 years', '50 to 54 years', '55 to 59 years', '60 to 64 years', '65 to 69 years') ~ NetRates_Female + Female_40to69,
                               TRUE ~ NetRates_Female + Female_70Plus)) %>%
                  select(1:2 | ends_with("NMR")) %>%
                  pivot_longer(cols = ends_with("NMR"), names_to = "Sex", values_to = "NMRs") %>%
                  mutate(Sex = case_when(Sex == "Male_NMR" ~ "Male",
                                                   TRUE ~ "Female"))


# Step 7: Apply Net Migration to Expected Population in order to calculate Projected Population

Projections <- Migration %>% left_join(expectedpop, by=c("Region", "Age", "Sex")) %>%
  mutate(NMs_Living =  ProjectedPop * NMRs)  %>%
  mutate(NMs_Living_Abs =  abs(ProjectedPop * NMRs))


# sum net migrants by broad k-factor age groups
n <- 1
NM_by_Age <- tibble()
temp <- tibble()

while(n <= length(Age_Groups)) {

  temp <- Projections %>% filter(Age %in% Age_Groups[[n]])

  temp <- temp %>% mutate(Age_Group = case_when(Age %in% Age_Groups[[1]] ~ '0 to 24 years',
                                                          Age %in% Age_Groups[[2]] ~ '25 to 39 years',
                                                          Age %in% Age_Groups[[3]]~ '40 to 69 years',
                                                          Age %in% Age_Groups[[4]] ~ '70 years and older'))

  temp <- temp %>% select(-Age) %>% group_by(Region, Sex, Age_Group) %>% summarise(sum_NM = sum(NMs_Living))

  NM_by_Age <- rbind(temp, NM_by_Age)


  n = n + 1
}

# sum absolute values of net migrants by broad k-factor age groups
n <- 1
Abs_NM_by_Age <- tibble()
temp <- tibble()

while(n <= length(Age_Groups)) {

  temp <- Projections %>% filter(Age %in% Age_Groups[[n]])

  temp <- temp %>% mutate(Age_Group = case_when(Age %in% Age_Groups[[1]] ~ '0 to 24 years',
                                                Age %in% Age_Groups[[2]] ~ '25 to 39 years',
                                                Age %in% Age_Groups[[3]]~ '40 to 69 years',
                                                Age %in% Age_Groups[[4]] ~ '70 years and older'))

  temp <- temp %>% select(-Age) %>% group_by(Region, Sex, Age_Group) %>% summarise(sum_NM_Abs = sum(NMs_Living_Abs))

  Abs_NM_by_Age <- rbind(temp, Abs_NM_by_Age)

  n = n + 1
}

remove(temp)
remove(n)

NM <- full_join(NM_by_Age, Abs_NM_by_Age, by =c('Region', "Age_Group", "Sex"))


# pull in target NM by sex, age group
target_NM_Sex <- target_NM_Sex %>% select(Region, Sex, Age, TargetNM) %>% rename(Age_Group = Age) %>%
  full_join(NM, by=c('Region', 'Age_Group', 'Sex'))




Projections <- Projections %>% mutate(Age_Group = case_when(Age %in% Age_Groups[[1]] ~ '0 to 24 years',
                                              Age %in% Age_Groups[[2]] ~ '25 to 39 years',
                                              Age %in% Age_Groups[[3]]~ '40 to 69 years',
                                              Age %in% Age_Groups[[4]] ~ '70 years and older'))


Projections <- Projections %>% left_join(target_NM_Sex, by=c('Region', 'Sex', 'Age_Group'))

Projections <- Projections %>%
  rowwise() %>%
  mutate(projNetMigrants = round( ((NMs_Living_Abs/sum_NM_Abs)*(TargetNM-sum_NM)+NMs_Living), digits = 0))

detailedMigs <- Projections %>%
  rename(originalMigrantTotals = projNetMigrants)

if(override > 0){

#----------------MANUAL MIGRATION OVERRIDES
# 1 This override checks if the number of 70+ in the CMAP region is positive. If it is, this override makes it negative and increases it by one half
Projections <- Projections %>%
  mutate(projNetMigrants = case_when(Region == "CMAP Region" && Age_Group == '70 years and older' && projNetMigrants > 0 ~ (projNetMigrants * -1.5),
                                     TRUE ~ projNetMigrants))
# 2 This override checks if the number of <24 is positive. If it is, it increases it by x1.5
Projections <- Projections %>%
  mutate(projNetMigrants = case_when(Region == "CMAP Region" && Age_Group == '0 to 24 years' && projNetMigrants > 0 ~ (projNetMigrants * 1.5),
                                     TRUE ~ projNetMigrants))
# 3 This override checks decreases the number of 30-39 by one half and adds 5k.
Projections <- Projections %>%
  mutate(projNetMigrants = case_when(Region == "CMAP Region" && Age_Group == '25 to 39 years' && Age != "25 to 29 years" ~ (projNetMigrants * 0.5) + 5000,
                                     TRUE ~ projNetMigrants))

print("Overrides Activated!")

}else{
  print("Migration Override Not Activated.")
}

detailedMigs <- detailedMigs %>% left_join(Projections, by=c("Age","Region","Sex")) %>%
  rename(modifiedMigrantTotals = projNetMigrants) %>%
  select(Age, Region, Sex, originalMigrantTotals, modifiedMigrantTotals)

# OPTION TO TURN OFF ALL MIGRATION! (for curiosity's sake only)
if(zeromigrationoverride > 0){
  Projections <- Projections %>%
    ungroup() %>%
    mutate(projNetMigrants = 0)
}

# apply the number of net migrants to the total population (PROJECTION COMPLETE!)
Projections <- Projections %>%
  mutate(ProjectedPop_final = round((ProjectedPop + projNetMigrants), digits = 0))

# Save Net Migration Sums by Age Grouping (for the next Projection Period's K factor, see lines 201-207 above)
NM_Prior_Period2 <- Projections %>%
  group_by(Region, Sex, Age_Group) %>%
  summarize(NetMigration = sum(projNetMigrants))

# Save total number of Migrants for Components of Change (see lines 375-376 below)
Migrants <- Projections %>% select(Region, Age, Sex, projNetMigrants)


# Step 8: Save Population projection, Net Migration rates, and Total Migration

Projections <- Projections %>%
  select(Age, Region, Sex, ProjectedPop_final)


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
  ungroup() %>%
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



