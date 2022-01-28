# CMAP | Alexis McAdams, Mary Weber | 8/18/2021

# This code calculates Households, GQ population,  estimates for all
# specified forecast years (probably 2025-2050)
# AND this code pulls households and GQ population for add'l years (2010, 2015, 2020)


# set-up --------------------

library(tidyverse)
library(tidycensus)
library(readxl)

# Headship source choice

if(HeadshipSource == 1){
    Headship <- read.csv("Input/Headships2010.csv") %>% select(Age, Region, Ratio_Adj) # Headship
} else if(HeadshipSource == 0){
    source("src/PUMS_Headship_Rates.R") # HEADSHIP_RATES
    source("src/Adjusted_Headship_Rates.R") # Headship
} else {
    print("ERROR! Invalid Headship calculation method chosen, edit HeadshipSource variable and run script again." )
}

load("Output/GQData2.Rdata") # GQratios, GQ_Military
load("Output/POP_PEP.Rdata") #POP

# External IL Adjustment option
if(EXTIL == 1){
  POP[["2010"]] <- read.csv("Input/adjustedCensus2010_ExtIL.csv")
  POP[["2015"]] <- read.csv("Input/adjustedPEP2015_ExtIL.csv")
  POP[["2020"]] <- read_excel("Input/censusadjustedPEP2020_ExtILadj.xlsx") # partial LOL counties

} else if(EXTIL == 0){
  print("ExtIL Area Adjustment override NOT implemented.")
} else {
  print("ERROR! Improper EXTIL value supplied. Modify and run again.")
}

# Load in and unlist projection results (used in loop below)
load(file="Output/PopProj.Rdata") # POPPROJ
results <- tibble()
i=1
for(item in POPPROJ){
  #print(item)
  temp <- item
  temp$year <- names(POPPROJ)[i]
  results <- bind_rows(results, temp)
  i <- i + 1
}


# set up Household and GQ Population Calculation loop -------------------

startyear = 2010
projectionstart = 2020
endyear = 2050

series <- c(2010,
            2015,
            2020,
  2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)

cycles <- ((endyear - startyear) / 5) + 1 #number of 5-year projection cycles to complete

#-------------------- Start of loop:

HHPOP_PROJ <- list() # for household population
for(years in series){
  HH_POPPROJ[[as.character(years)]] <- tibble()
}

HH_PROJ <- list() # for number of households
for(years in series){
  HH_PROJ[[as.character(years)]] <- tibble()
}

GQ_PROJ <- list() # for group quarters populations
for(years in series){
  GQ_PROJ[[as.character(years)]] <- tibble()
}


i <- 1
while(i <= cycles){
  projstart <- series[i] %>% as.character()

  if(projstart <= projectionstart) {
    basepop <- POP[[series[i] %>% as.character()]] %>%
      group_by(Sex, Age, Region) %>%
      summarize(Population = sum(Population), .groups = "drop")
    print(paste("Pulling past data:", startyear, sep=" "))

  }else{
    basepop <- results %>%
      filter(year == projstart) %>%
      rename(Population = ProjectedPop_final) %>%
      select(Sex, Age, Region, Population) %>%
      ungroup()
    print(paste("Pulling projected data:", startyear, sep=" "))
  }

  # Join GQ ratios to Population, calculates GQ population
  GQ_Pop <- full_join(basepop, GQratios, by=c("Sex", "Age", "Region")) %>%
    mutate(round(across(starts_with("GQ"), ~.*Population ),0)) %>% # multiply every GQ column by Population
    left_join(GQ_Military, by=c("Region", "Sex", "Age")) %>%
    rename(GQ_NonInst_Military = Value) #join the (held-constant over time) Military values

  # Subtract GQ from Population (Household Population), pivot wide, join Headship ratio & calculate Heads of Household (aka Households)
  HouseholdPop <- GQ_Pop %>%
    rowwise() %>% mutate(totalGQ = sum(across(starts_with("GQ")))) %>%
    select(Age, Sex, Region, Population, totalGQ) %>%
    mutate(HH_Pop = Population - totalGQ) %>%
    select(-totalGQ) %>%
    pivot_wider(names_from = "Sex", values_from=c("Population", "HH_Pop")) %>%
    left_join(Headship, by=c("Age", "Region")) %>%
    mutate(Head_HH = round((HH_Pop_Male*Ratio_Adj)+(HH_Pop_Female*Ratio_Adj),0))

  print(paste("Year", series[i], "households and GQ calculations complete!", sep=" "))

  # save results in lists
  HHPOP_PROJ [[projstart]] <- HouseholdPop %>% select(Age, Region, HH_Pop_Female, HH_Pop_Male)
  HH_PROJ[[projstart]] <- HouseholdPop %>% select(Age, Region, Head_HH) %>% rename(Households = Head_HH)
  GQ_PROJ[[projstart]] <- GQ_Pop

  i <- i+1
}

#save(HHPOP_PROJ, file="Output/HHPOP_PROJ.Rdata")
#save(HH_PROJ, file="Output/HH_Proj.Rdata")
#save(GQ_PROJ, file="Output/GQ_Proj.Rdata")


#---------------------- End of loop

# run script to collect % of GQ by Race/Ethnicity (2010 Census)
source("src/GQ_by_RaceEth.R")
load("Output/GQRE_rates.Rdata") # GQRE_perc


############# ARRANGING AND FORMATTING -----------------

### HOUSEHOLD POPULATION

HouseholdPops <- tibble()
i=1
for(item in HHPOP_PROJ){
  temp2 <- item
  temp2$Year <- names(HHPOP_PROJ)[i]
  HouseholdPops <- bind_rows(HouseholdPops, temp2)
  i <- i + 1
}

HouseholdPop_byAge <- HouseholdPops %>%
  rowwise() %>%
  mutate(TotalHHPop = sum(HH_Pop_Female, HH_Pop_Male), .keep = "unused")

HouseholdPop_bySex <- HouseholdPops %>%
  pivot_longer(cols = c("HH_Pop_Female", "HH_Pop_Male"), names_to = "Sex", values_to = "TotalHHPop") %>%
  mutate(Sex = case_when(Sex == "HH_Pop_Female" ~ "Female",
                         TRUE ~ "Male")) %>%
  group_by(Region, Year, Sex) %>%
  summarize(TotalHHPop = sum(TotalHHPop), .groups = "drop")

HouseholdPop_Summary <- HouseholdPop_bySex %>%
  group_by(Region, Year) %>%
  summarize(TotalHHPop = sum(TotalHHPop), .groups = "drop")

### HOUSEHOLDS

HeadofHH_by_Age <- tibble()
i=1
for(item in HH_PROJ){
  temp2 <- item
  temp2$Year <- names(HHPOP_PROJ)[i]
  HeadofHH_by_Age <- bind_rows(HeadofHH_by_Age, temp2)
  i <- i + 1
}

Households_summary <- HeadofHH_by_Age %>%
  group_by(Region, Year) %>%
  summarize(Households = sum(Households), .groups = "drop")

# Average Household Size (HHpop / HH) for each region and year
HouseholdSize <- full_join(Households_summary, HouseholdPop_Summary, by = c("Region", "Year")) %>%
  rowwise() %>%
  mutate(HHSize = TotalHHPop / Households, .keep = "unused")

# calculate the HHpop by Travel Model age group (<35, 35-64, 65+)
travelModelHHpop <- HouseholdPop_byAge %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  mutate(agegroup = case_when(x < 35 ~ "a_lessthan35",
                              x >= 65 ~ "c_over65",           # define age groups
                              TRUE ~ "b_between35_65")) %>%   # "a,b,c" is only for helping to sort the table
  group_by(Region, agegroup, Year) %>%
  summarize(TotalHHPop = sum(TotalHHPop), .groups = "drop") %>% arrange(Year)

#calculate # of Heads of Household by Travel Model age group (<35, 35-64, 65+)
travelModelHeadsofHHs <- HeadofHH_by_Age %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  mutate(agegroup = case_when(x < 35 ~ "a_lessthan35",
                              x >= 65 ~ "c_over65",           #define age groups
                              TRUE ~ "b_between35_65")) %>%   # a,b,c is for table sorting only
  group_by(Region, agegroup, Year) %>%
  summarize(totalHeadsofHHs = sum(Households), .groups = "drop") %>% arrange(Year)

### GROUP QUARTERS

GQresult <- tibble()
i=1
for(item in GQ_PROJ){
  temp2 <- item
  temp2$Year <- names(GQ_PROJ)[i]
  GQresult <- bind_rows(GQresult, temp2)
  i <- i + 1
}

GQ_detailed <- GQresult %>%
  select(-Population) %>%
  relocate(Year, .after = Region) %>%
  rowwise() %>%
  mutate(Inst_GQ = round(sum(across(starts_with("GQ_Inst"))), 0),
         nonInst_GQ = round(sum(across(starts_with("GQ_NonInst"))),0) ) %>%
  mutate(totalGQ = sum(across(ends_with("GQ")))) %>%
  relocate(c(totalGQ, Inst_GQ, nonInst_GQ), .after = Year) %>%
  relocate(starts_with("GQ_Inst_"), .after = Inst_GQ) %>%
  relocate(GQ_NonInst_Military, .before = GQ_NonInst_Other)

GQ_summarybyAge <- GQ_detailed %>%
  pivot_longer(cols = contains("GQ"), names_to = "type", values_to = "value") %>%
  group_by(Region, Year, Age, type) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = "type", values_from = "value") %>%
  relocate(c(totalGQ, Inst_GQ, nonInst_GQ), .after = Age) %>%
  relocate(starts_with("GQ_Inst_"), .after = Inst_GQ) %>%
  relocate(GQ_NonInst_Military, .before = GQ_NonInst_Other)

GQ_summary <- GQ_summarybyAge %>%
  select(!contains("GQ") | ends_with("GQ")) %>%
  group_by(Region, Year) %>%
  summarize(totalGQ = sum(totalGQ),
            Inst_GQ = sum(Inst_GQ),
            nonInst_GQ = sum(nonInst_GQ), .groups = "drop")


travelModel_GQNonInst_byAge <- GQ_summarybyAge %>%
  select(!contains("GQ") | "GQ_NonInst_Military" | "GQ_NonInst_College" | "GQ_NonInst_Other") %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  mutate(Agegroup = case_when(x < 15 ~ "14 and Under",
                              x >= 65 ~ "65 and Over",
                              TRUE ~ "15 - 64" )) %>%
  select(-x) %>%
  group_by(Region, Year, Agegroup) %>%
  summarize(GQ_NonInst_Military = sum(GQ_NonInst_Military),
            GQ_NonInst_College = sum(GQ_NonInst_College),
            GQ_NonInst_Other = sum(GQ_NonInst_Other), .groups = "drop")

travelModel_GQOther_byAge <- GQ_summarybyAge %>%
  select(!contains("GQ") | "GQ_NonInst_Other") %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>%
  mutate(Agegroup = case_when(x < 15 ~ "GQ_NonInst_Other_lessthan15",
                              x == 15 ~ "GQ_NonInst_Other_15_to_19",
                              x >= 65 ~ "GQ_NonInst_Other_65_plus",
                              TRUE ~ "GQ_NonInst_Other_20_to_64") ) %>%
  group_by(Region, Year, Agegroup) %>%
  summarize(Othertot = sum(GQ_NonInst_Other), .groups = "drop") %>%
  pivot_wider(names_from = Agegroup, values_from = Othertot) %>%
  mutate(GQ_NonInst_lessthan16 = round(GQ_NonInst_Other_lessthan15 + GQ_NonInst_Other_15_to_19 * 0.2, 0),
         GQ_NonInst_Other_16_to_64 = round(GQ_NonInst_Other_20_to_64 + 0.8 * GQ_NonInst_Other_15_to_19, 0),
         GQ_NonInst_Other_65_plus = round(GQ_NonInst_Other_65_plus, 0)) %>%
  select(-GQ_NonInst_Other_lessthan15, -GQ_NonInst_Other_20_to_64, -GQ_NonInst_Other_15_to_19) %>%
  relocate(GQ_NonInst_Other_65_plus, .after = last_col())

travelModel_collegemil <- GQ_summarybyAge %>%
  group_by(Region, Year) %>%
  summarize(GQ_Military = sum(GQ_NonInst_Military),
            GQ_College = round(sum(GQ_NonInst_College)), .groups = "drop")

travelModel_GQ <- left_join(travelModel_collegemil,
                            travelModel_GQOther_byAge %>% select(-GQ_NonInst_lessthan16),
                            by = c("Region", "Year"))

placement_ratios <- GQ_summarybyAge %>%
  select(!contains("GQ") | starts_with("GQ")) %>%
  pivot_longer(cols = starts_with("GQ_"), names_to = "type", values_to = "value") %>%
  group_by(Region, Year, type) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(Year = paste("y_",Year,sep = "")) %>%
  pivot_wider(names_from = "Year", values_from = "value")
write.csv(placement_ratios, "C:/Users/amcadams/Documents/R/gq/placement_ratios.csv")

write.csv()


placement_InstGQfactors <- placement_InstGQ %>%
  mutate(across(starts_with("y_"), ~./y_2010))

  mutate(round(across(starts_with("GQ"), ~.*Population ),0))

temp <- GQ_summary %>%
  select(Region, Year, Inst_GQ) %>%
  rename(TotalInstGQ = Inst_GQ)




###   GROUP QUARTERS and HOUSEHOLDS BY RACE AND ETHNICITY
# total GQ pop by RE
GQRE_pop <- GQ_summary %>%
  select(!contains("GQ") | "totalGQ") %>%
  full_join(GQRE_perc, by = "Region") %>%
  rowwise() %>% mutate(GQRE_pop_proj = round(totalGQ * GQ_perc,0), .keep = "unused")

# total HH pop by RE
load("Output/totalPop_RE_projection.Rdata") #raceeth_proj
#reformat R/E groupings
raceeth_proj <- raceeth_proj %>%
  mutate(variable = case_when(HISP == "Hispanic" ~ "Hispanic",
                              RACE == "Asian alone" ~ "NH_Asian",
                              RACE == "Black alone" ~ "NH_Black",
                              RACE == "White alone" ~ "NH_White",
                              RACE == "NH_Other" ~ "NH_Other"))  %>%
  select(Region, Year, variable, calcPop) %>%
  rename(totalRE_pop = calcPop)

#join the GQ pops to total R/E populations, subtract to get HH pop
HHRE_pop <- GQRE_pop %>%
  full_join(raceeth_proj, by=c("Region", "Year", "variable")) %>%
  rowwise() %>%
  mutate(HHRE_pop_proj = totalRE_pop - GQRE_pop_proj) %>%
  select(Region, Year, variable, HHRE_pop_proj)


################ TACK ON / OVERWRITE WITH KNOWN DATA (2010-2020)









################# EXPORT ---------------------



write.csv(Households, file = "C:/Users/amcadams/Documents/R/extILadj/export_Households.csv")
write.csv(HouseholdSummary, file = "C:/Users/amcadams/Documents/R/extILadj/export_HouseholdsSummary.csv")
write.csv(HouseholdSize, file = "C:/Users/amcadams/Documents/R/extILadj/export_HouseholdSize.csv")
write.csv(travelModelHHs, file = "C:/Users/amcadams/Documents/R/extILadj/export_travelmodelHHs.csv")
#write.csv(HHs_65split, file = "C:/Users/amcadams/Documents/R/extILadj/export_Households_Age65split.csv")

write.csv(GQ_full, file = "C:/Users/amcadams/Documents/R/extILadj/export_GQ_full.csv")
write.csv(GQ_basic_summary, file = "C:/Users/amcadams/Documents/R/extILadj/export_GQ_basic_summary.csv")
#write.csv(GQ_summary, file = "C:/Users/amcadams/Documents/R/extILadj/export_GQ_summary.csv")
write.csv(GQ_summary_travelmodel, file = "C:/Users/amcadams/Documents/R/extILadj/export_GQ_summary_travelmodel.csv")








### calculate projected GQ and Household populations by Race/Ethnicity
  # make sure race_ethnicity_projection.R is checked out FIRST!!!

#import GQ by R/E rates
load("Output/GQRE_rates.Rdata") #GQRE_perc, from GQ_by_RaceEth.R

#apply rates to total GQ population (GQ_basic_summary) to get GQ pop by Race/Ethnicity
GQRE_pop <- GQ_basic_summary %>%
  select(Region, Year, totalGQ) %>%
  full_join(GQRE_perc, by="Region") %>%
  rowwise() %>%
  mutate(GQRE_pop_proj = round(totalGQ * GQ_perc,0)) %>%
  select(Region, Year, variable, GQRE_pop_proj) #%>%
  #filter(Year >=2025)

#import total R/E populations


#export
#write.csv(GQRE_pop, file = "C:/Users/amcadams/Documents/R/extILadj/export_GQpop_RE.csv")
#write.csv(HHRE_pop, file = "C:/Users/amcadams/Documents/R/extILadj/export_HHpop_RE.csv")



