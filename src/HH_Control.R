# CMAP | Alexis McAdams, Mary Weber | 8/18/2021

# This code calculates Households and GQ population estimates for all
# specified population years, past and forecast.

library(tidyverse)
library(tidycensus)
library(readxl)

load("Output/GQData2.Rdata") # GQratios, GQ_Military
load("Output/Head_of_HH.Rdata") # Headship
load("Output/Migration_Projections.Rdata") #Mig_Proj
load("Output/PopData.Rdata") #POP


startyear = 2010
projectionstart = 2020
endyear = 2050

series <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)

cycles <- ((endyear - startyear) / 5) + 1 #number of 5-year projection cycles to complete

#--------------------

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


Households <- tibble()
i=1
for(item in HH_PROJ){
  temp2 <- item
  temp2$Year <- names(HH_PROJ)[i]
  Households <- bind_rows(Households, temp2)
  i <- i + 1
}
Households  <- Households  %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>% select(-x) %>% arrange(Region, Year)

HouseholdSummary <- Households %>%
  group_by(Year, Region) %>%
  summarize(FemaleHHPop = sum(HH_Pop_Female),
            MaleHHPop = sum(HH_Pop_Male),
            HH_total = sum(Head_HH))

GQ_full <- tibble()
i=1
for(item in GQ_PROJ){
  temp2 <- item
  temp2$Year <- names(GQ_PROJ)[i]
  GQ_full <- bind_rows(GQ_full, temp2)
  i <- i + 1
}
GQ_full  <- GQ_full  %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>% select(-x) %>% arrange(Region, Year, desc(Sex))

GQ_summary <- GQ_full %>% group_by(Region, Sex, Age, Year) %>%
  summarize(totalGQpop = sum(totalGQ))




#save(HH_PROJ, file="Output/HH_Proj.Rdata")
#save(GQ_PROJ, file="Output/GQ_Proj.Rdata")

