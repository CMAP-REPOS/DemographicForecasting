# CMAP | Mary Weber | 8/18/2021

library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

#lines 15-22 are duplicate from PEP code.... should consolidate
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

startyear = as.character(projstart)  #"2020"

Head_of_HH <- Head_of_HH %>% select(-Headship_Rate, -Head_HH, -Households, -Head_HH_Adjust)

# if/else statement decides if population is pulled from POP or MIG_PROJ and formats data accordingly
if(startyear <= baseyear2) {

  basepop <- POP[[startyear]] %>%
    group_by(Sex, Age, Region) %>%
    summarize(Population = sum(Population)) %>%
    ungroup()

}else{

  basepop <- Mig_Proj %>%
    filter(year == startyear) %>%
    rename(Population = ProjectedPop_final) %>%
    select(Sex, Age, Region, Population) %>%
    ungroup()
}


# Joins GQ ratios to Population, calculates GQ totals
GQ_Pop <- full_join(basepop, GQratios, by=c("Sex", "Age", "Region")) %>%
  mutate(across(starts_with("GQ"), ~.*Population)) %>% #multiply every GQ column by Population
  left_join(GQ_Military, by=c("Region", "Sex", "Age")) %>% rename(GQ_NonInst_Military = Value) %>% #join the Military values
  rowwise() %>%
  mutate(Inst_GQ = round(sum(across(starts_with("GQ_Inst"))), 0),
         nonInst_GQ = round(sum(across(starts_with("GQ_NonInst"))),0) ) %>%
  mutate(totalGQ = sum(across(ends_with("GQ"))))

# Subtract GQ from Population, pivot table wider, join Headship ratio, calculate Heads of Household (aka Households)
HouseholdPop <- GQ_Pop %>% select(Age, Sex, Region, Population, totalGQ) %>%
  mutate(HH_Pop = Population - totalGQ) %>%
  select(-totalGQ) %>%
  pivot_wider(names_from = "Sex", values_from=c("Population", "HH_Pop")) %>%
  left_join(Headship, by=c("Age", "Region")) %>%
  mutate(Head_HH = round((HH_Pop_Male*Ratio_Adj)+(HH_Pop_Female*Ratio_Adj),0))
