# CMAP | Alexis McAdams, Mary Weber | 8/18/2021

# This code calculates Households and GQ population estimates for all
# specified population years, past and forecast.

# This code runs off of the projection outputs that are saved to GitHub, so if you're
# working from a new Projection, make sure the projected population data is saved to GitHub!

#--------------------

library(tidyverse)
library(tidycensus)
library(readxl)

load("Output/GQData2.Rdata") # GQratios, GQ_Military
load("Output/Head_of_HH.Rdata") # Headship
#load("Output/Migration_Projections.Rdata") #Mig_Proj
load("Output/PopData.Rdata") #POP

#override Headship #s with 2010 Adjusted Headship Ratios (pulled from Berger)
Headship <- read.csv("C:/Users/amcadams/Documents/R/Headships2010.csv") %>% select(Age, Region, Ratio_Adj)

startyear = 2010
projectionstart = 2020
endyear = 2050

series <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)

cycles <- ((endyear - startyear) / 5) + 1 #number of 5-year projection cycles to complete

#-------------------- Start of loop

HH_PROJ <- list()
for(years in series){
  HH_PROJ[[as.character(years)]] <- tibble()
}

GQ_PROJ <- list()
for(years in series){
  GQ_PROJ[[as.character(years)]] <- tibble()
}


i <- 1
while(i <= cycles){
  projstart <- series[i]

  source("src/Household_Totals.R")

  HH_PROJ[[as.character(projstart)]] <- HouseholdPop

  GQ_PROJ[[as.character(projstart)]] <- GQ_Pop

  #save(HH_PROJ, file="Output/HH_Proj.Rdata")

  i <- i+1
}

#---------------------- End of loop


###     De-list and reformat the outputs, generate summary tables

### HOUSEHOLDS

Households <- tibble()
i=1
for(item in HH_PROJ){
  temp2 <- item
  temp2$Year <- names(HH_PROJ)[i]
  Households <- bind_rows(Households, temp2)
  i <- i + 1
}

Households <- Households %>%
  rowwise() %>%
  mutate(TotalPop = sum(Population_Female, Population_Male), .after = Region) %>%
  mutate(TotalHHPop = sum(HH_Pop_Female, HH_Pop_Male), .after = Population_Male)

HouseholdSummary <- Households %>%
  group_by(Year, Region) %>%
  summarize(FemaleHHPop = sum(HH_Pop_Female),
            MaleHHPop = sum(HH_Pop_Male),
            HH_total = sum(Head_HH)) %>%
  rowwise() %>% mutate(TotHHPop = FemaleHHPop + MaleHHPop, .after = Region)

#calculate the household Size (HHpop / HH) for each region and year

HouseholdSize <- Households %>% group_by(Region, Year) %>%
  summarise(totHHpop = sum(TotalHHPop), totHH = sum(Head_HH)) %>% #sum up HHpop and Heads(aka # Households) by Region&Year
  rowwise() %>% mutate(householdSize = totHHpop / totHH) #calculate householdSize

#calculate the HHpop, total Household Heads, and household size by Travel Model age group (<35, 35-65, 65+)
#NOTE: household size really only useful for 65+
travelModelHHs <- Households %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  mutate(agegroup = case_when(x < 35 ~ "a_lessthan35",
                              x >= 65 ~ "c_over65",           #define age groups
                              TRUE ~ "b_between35_65")) %>%   # a,b,c is for table sorting only
  group_by(Region, agegroup, Year) %>%
  summarize(totHHpop = sum(TotalHHPop), totHH = sum(Head_HH)) %>%
  rowwise() %>% mutate(householdSize = totHHpop / totHH) #calculate householdSize

#calculate the HHpop, total Household Heads, and household size by <65 and 65+

HHs_65split <- Households %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  mutate(agegroup = case_when(x >= 65 ~ "b_over65",
                              TRUE ~ "a_lessthan65")) %>%
  group_by(Region, agegroup, Year) %>%
  summarize(totHHpop = sum(TotalHHPop), totHH = sum(Head_HH)) %>%
  rowwise() %>% mutate(householdSize = totHHpop / totHH) #calculate householdSize

### GROUP QUARTERS

GQ_full <- tibble()
i=1
for(item in GQ_PROJ){
  temp2 <- item
  temp2$Year <- names(GQ_PROJ)[i]
  GQ_full <- bind_rows(GQ_full, temp2)
  i <- i + 1
}
GQ_full  <- GQ_full  %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>% select(-x) %>% arrange(Region, Year, desc(Sex)) %>%
  filter(Year != "2010" & Year != "2015") %>%
  relocate(c(Year, totalGQ, Inst_GQ, nonInst_GQ), .after = Population) %>%
  relocate(starts_with("GQ_Inst_"), .after = Inst_GQ) %>%
  relocate(GQ_NonInst_Military, .before = GQ_NonInst_Other)

GQ_basic_summary <- GQ_full %>%
  group_by(Region, Year) %>%
  summarize(totalGQ = sum(totalGQ),
            totalGQ_Inst = sum(Inst_GQ),
            totalGQ_NonInst = sum(nonInst_GQ))

GQ_summary <- GQ_full %>% group_by(Region, Sex, Age, Year) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>%
  mutate(Agegroup = case_when(x < 15 ~ "14 and Under",
                              x >= 65 ~ "65 and Over",
                              TRUE ~ "15 - 64" )) %>%
  arrange(x) %>% select(-x) %>% arrange(Region, Year, Sex) %>%
  group_by(Region, Year, Agegroup) %>%
  summarize(GQ_NonInst_Military = sum(GQ_NonInst_Military),
            GQ_NonInst_College = sum(GQ_NonInst_College),
            GQ_NonInst_Other = sum(GQ_NonInst_Other))

GQ_Other  <- GQ_full %>% #break down the GQ_NonInst_Other into the age group totals required by the travel model (16-64, 65+)
  select(Sex, Age, Region, Year, GQ_NonInst_Other) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>%
  filter(x > 10) %>% # remove Age groups 10-14 and below
  mutate(Agegroup = case_when(x == 15 ~ "GQ_NonInst_Other_15_to_19",
                              x >= 65 ~ "GQ_NonInst_Other_65_plus",
                              TRUE ~ "GQ_NonInst_Other_20_to_64" ) ) %>%
  group_by(Region, Sex, Year, Agegroup) %>%
  summarize(Othertot = sum(GQ_NonInst_Other)) %>%
  pivot_wider(names_from = Agegroup, values_from = Othertot) %>%
  mutate(GQ_NonInst_Other_16_to_64 = round(GQ_NonInst_Other_20_to_64 + 0.8 * GQ_NonInst_Other_15_to_19,0),
         GQ_NonInst_Other_65_plus = round(GQ_NonInst_Other_65_plus,0)) %>%
  select(Sex, Region, Year, GQ_NonInst_Other_16_to_64,GQ_NonInst_Other_65_plus)

GQ_summary_collegemil <- GQ_full %>% group_by(Region, Year, Sex) %>%
  summarize(GQ_Military = sum(GQ_NonInst_Military),
            GQ_College = round(sum(GQ_NonInst_College)))

GQ_summary_travelmodel <- left_join(GQ_summary_collegemil, GQ_Other, by = c("Region", "Year", "Sex")) %>%
  group_by(Region,Year) %>%
  summarize(GQ_Military = sum(GQ_Military),
            GQ_College = sum(GQ_College),
            GQ_NonInst_Other_16_to_64 = sum(GQ_NonInst_Other_16_to_64),
            GQ_NonInst_Other_65_plus = sum(GQ_NonInst_Other_65_plus))

#View(HouseholdSummary)
#View(HouseholdSize)
View(GQ_basic_summary)


'
write.csv(Households, file = "C:/Users/amcadams/Documents/R/export_Households.csv")
write.csv(HouseholdSummary, file = "C:/Users/amcadams/Documents/R/export_HouseholdsSummary.csv")
write.csv(HouseholdSize, file = "C:/Users/amcadams/Documents/R/export_HouseholdSize.csv")
write.csv(travelModelHHs, file = "C:/Users/amcadams/Documents/R/export_travelmodelHHs.csv")
write.csv(HHs_65split, file = "C:/Users/amcadams/Documents/R/export_Households_Age65split.csv")

write.csv(GQ_full, file = "C:/Users/amcadams/Documents/R/export_GQ_full.csv")
write.csv(GQ_basic_summary, file = "C:/Users/amcadams/Documents/R/export_GQ_summary.csv")
write.csv(GQ_summary, file = "C:/Users/amcadams/Documents/R/export_GQ_summary.csv")
write.csv(GQ_summary_travelmodel, file = "C:/Users/amcadams/Documents/R/export_GQ_summary_travelmodel.csv")
write.csv(HouseholdSize, file = "C:/Users/amcadams/Documents/R/export_HouseholdSize.csv")
'



#save(HH_PROJ, file="Output/HH_Proj.Rdata")
#save(GQ_PROJ, file="Output/GQ_Proj.Rdata")

