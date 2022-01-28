# Workforce.R
#
# This script estimates the number of workers in households from several inputs: total
# population by age and sex (Census and CMAP Projection), estimates of Group
# Quarter populations by age and sex (Census and CMAP Projection), national age- and sex-
# specific Labor Force Participation Rates (Source: CBO's "2021 Long-Term Budget
# Outlook", interpolated and held constant post-2030), national annual
# non-Military 16+ Unemployment rates (source: CBO's "An Update to the Budget
# and Economic Outlook: 2021 to 2031", held constant post-2030 (~4.5%))
#
# Alexis McAdams, CMAP, Oct 2021

# load libraries
library(dplyr)
library(tidyverse)
library(readxl)

##### Set-up
# GQ ratios from GroupQuarters.R
load("Output/GQData2.Rdata") # named GQratios

# define "too young to work" age groups (assumption)
kiddos <- c("0 to 4 years", "5 to 9 years", "10 to 14 years")

# import Labor Force Participation Rate projections (15 and up)
LFPRs <- read.csv("Input/LFPRs.csv")

# import unemployment rates (historical and projections)
unemp <- read.csv("Input/unemploymentrates.csv") %>%
  mutate(Year = as.character(Year))

#######re-load in and format known population data (2010-2020)
# POP was originally retrieved in Projection.R
load("Output/POP_PEP.Rdata")  # named POP

# apply EXTIL adjustment (if necessary)
if(EXTIL == 1){
  POP[["2020"]] <- read_excel("Input/censusadjustedPEP2020_ExtILadj.xlsx") %>% # partial LOL counties
    mutate(GEOID = as.character(GEOID))
} else if (EXTIL == 0){
  # no modification to POP file.
  print("ExtIL Area Adjustment override NOT implemented.")
}else {
  print("ERROR! Improper EXTIL value supplied. Modify and run again.")
}

#create POPrecent list
POPrecent <- list()
POPrecent[["2010"]] <- POP[["2010"]]
POPrecent[["2015"]] <- POP[["2015"]] # TO-DO: build in option to look at 15,18,19 (for comparison to EMP employment forecast)
#POPrecent[["2018"]] <- POP[["2018"]]
#POPrecent[["2019"]] <- POP[["2019"]]
POPrecent[["2020"]] <- POP[["2020"]]
for(i in names(POPrecent)){
  POPrecent[[i]] <- POPrecent[[i]] %>%
    group_by(Region, Sex, Age) %>%
    summarize(Population = sum(Population), .groups = "drop")
}

#load in and format projected population data (2025-2050)
POPPROJcopy <- POPPROJ
POPPROJcopy[['2020']] <- NULL  #remove the blank 2020 from POPPROJ (investigate that later)
for(i in names(POPPROJcopy)){
  POPPROJcopy[[i]] <- POPPROJcopy[[i]] %>% ungroup() %>%
    rename(Population = ProjectedPop_final) #reformat Projected Population column
}

#combine the two lists created above into one list
laborforce <- c(POPrecent, POPPROJcopy)

#slight reformat to GQratios (sum partial GQ ratios to get Total GQ ratio)
GQratios2 <- GQratios %>%
  mutate(GQratio = GQ_NonInst_College + GQ_Inst_Corr + GQ_Inst_Juv + GQ_Inst_Nurs + GQ_Inst_Other + GQ_NonInst_Other)

#join the GQ ratios to the population for every year
for(i in names(laborforce)){
  laborforce[[i]] <- left_join(laborforce[[i]], GQratios2, by=c("Region","Age","Sex")) %>%
    mutate(GQpop = Population * GQratio,
           NonGQpop = round(Population - GQpop,0)) %>%
    filter(!Age %in% kiddos)
}

#join the LFPRs to each of the tables in laborforce
laborforce[["2010"]] <- laborforce[["2010"]] %>% left_join(LFPRs[c(1:2,3)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2010)
laborforce[["2015"]] <- laborforce[["2015"]] %>% left_join(LFPRs[c(1:2,4)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2015)
#laborforce[["2018"]] <- laborforce[["2018"]] %>% left_join(LFPRs[c(1:2,5)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2018) #see line 49 above
#laborforce[["2019"]] <- laborforce[["2019"]] %>% left_join(LFPRs[c(1:2,6)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2019)
laborforce[["2020"]] <- laborforce[["2020"]] %>% left_join(LFPRs[c(1:2,7)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2020)
laborforce[["2025"]] <- laborforce[["2025"]] %>% left_join(LFPRs[c(1:2,8)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2025)

#use the 2030 LPFR for 2030 and all projection years after that
for(i in c("2030","2035","2040","2045","2050")){
  laborforce[[i]] <- laborforce[[i]] %>% left_join(LFPRs[c(1:2,9)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2030)
}

#multiply NonGQpop * LFPR for each year
for(i in names(laborforce)){
  laborforce[[i]] <-  laborforce[[i]] %>%
    ungroup() %>%
    mutate(laborforce = NonGQpop * (LFPR/100))
}

#condense laborforce list into single tibble
workers <- tibble()
i=1
for(item in laborforce){
  #print(item)
  temp <- item
  temp$year <- names(laborforce)[i]
  workers <- bind_rows(workers, temp)
  i <- i + 1
}

#total up labor force, join with unemployment rate, calculate number of workers
workers <- workers %>%
  #select(-starts_with("LFPR")) %>% #filter out to select an employment forecast
  group_by(Region, year) %>%
  summarize(totlaborforce = sum(laborforce), .groups = "drop") %>%
  left_join(unemp, by=c("year" = "Year")) %>%
  mutate(workers = totlaborforce * (1 - (Unemployment.Rate / 100))) %>%
  select(-totlaborforce, -Unemployment.Rate) %>%
  mutate(workers = round(workers * 1.00113,0)) %>% # constant to account for jobs held by people commuting from out of region (Berger's #)
  mutate(Year = as.numeric(year)) %>% select(-year) %>%
  mutate(type = case_when(Year < 2021 ~ "PEPestimate",
                          TRUE ~ "Projection"))

