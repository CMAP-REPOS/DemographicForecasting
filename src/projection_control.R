# CMAP | Alexis McAdams, Mary Weber | 8/11/2021

###########  Projection Control file info  ###############

# Hey you! Make sure to run setup_control.R BEFORE running this script :)
#
# This script runs a loop to cycle through the Projection.R script over
# and over to get population projections in 5 year increments up to 2050.
# For each projection period, the Population projection and Net Migration
# are saved in lists (POPPROJ and NETMIGPROJ, respectively) and are used as
# input for the subsequent projection period.
# When the loop is complete, the POPPROJ list is reformatted into a table
# called "Mig_Proj".

######## load libraries ----------------

library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)

######### set options ----------------
#

#### set External Illinois Adjustment area Base Population override
# this option changes the Base Year (2020) population
# this option does NOT recalculate fertility/mortality/migration rates (county or region-wide rates will be applied.)
#   if 1: model will use travel model boundary-adjusted population for LOL (Lee, Ogle, LaSalle) counties
#   if 0: model will use full county populations for the LOL counties, no adjustment will be applied.

EXTIL = 0

#### COVID-19 Base Population Adjustment (***NOT YET IMPLEMENTED)
# this option will change the Base Year (2020) population to account for the Covid-19 pandemic.
# this option does NOT alter the mortality rates (reflecting assumption that post-COVID death rates will
# return to pre-COVID forecasted rates.)
#   if 0: base population WILL NOT be modified.
#   if 1: base population WILL be modified to account for deaths specifically attributed to COVID-19 (County records)
#   if 2: base population WILL be modified to account for deaths likely due to impacts of the COVID-19 pandemic (Excess Deaths, CDC)

c19deaths = 0

#### set Target Net Migration (TNM) values
# (see target_netmigration folder for options)
# NOTE: due to modifications to how Net Migration is calculated, these TNM values are
# a ballpark figure and may not be reached exactly for each projection period.

TNMfilename <- "target_netmigration/TNM_workerjobbalance.xlsx"

#### set coarse Migration overrides
# these options crudely modify the CMAP Region's projected Net Migration values in order to more closely
# match the recent distribution of Net Migrants (aka shape of # net migrants v age plot).
#   if 0: no override applied
#   if 1: migration overrides will be applied. (see Projection.R lines 363-375)

override = 1

#### Zero Net Migration
# This option will force net migration to 0.
# Used to examine Natural Increase (or Decrease) impacts on population.
#   if 0: no override applied
#   if 1: net migration for every age and sex group will be forced to 0 (see Projection.R lines 383-388)

zeromigrationoverride = 0




######## set-up total projection start/end variables ---------------
baseyear <- 2020
startyear <- 2020
endyear <- 2050

projnums <- (endyear - startyear) / 5 #number of 5-year projection cycles to complete

series <- seq(from=startyear,
              to=endyear,
              by= 5)

target_NM_Sex_check <- tibble()
base_year_NM_check <- tibble()

###### set up import of TNM values
target_NM <- read_excel(TNMfilename) %>%
  mutate(Year = as.character(Year))
#name which net migration values you're using (important for documentation!)
TNMnote <- str_split(TNMfilename, "TNM_")[[1]][2] # formerly tNMfile

######## set up the population projection and migration projection lists
#create new file
POPPROJ <- list()
for(years in series){
  POPPROJ[[as.character(years)]] <- tibble()
}
NETMIGPROJ <- list()
for(years in series){
  NETMIGPROJ[[as.character(years)]] <- tibble()
}

COMPONENTS <- list()
for(years in series){
  COMPONENTS[[as.character(years)]] <- tibble()
}

MIG_DETAIL <- list()
for(years in series){
  MIG_DETAIL[[as.character(years)]] <- tibble()
}


#import in Base Net Migration data
#NetMig <- read_excel("Input/NetMigration_Berger.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)
#NetMig <- read_excel("Input/NetMigration_ExpandedAgeGroups.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)
load("Output/pastMigration_ageGroupSums.Rdata") #netMigSums, derived from pastMigration_ageGroupSums.R
NetMig <- netMigSums %>% select(-Source)


######## run the loop

i <- 1
while(i <= projnums){

  # Apply Base Population Adjustments

  # ExtIL Adjustment
  if(EXTIL == 1){
    POP[["2020"]] <- read_excel("Input/censusadjustedPEP2020_ExtILadj.xlsx") %>% # partial LOL counties
      mutate(GEOID = as.character(GEOID))
  } else if (EXTIL == 0){
    # no modification to POP file.
  }else {
    print("ERROR! Improper EXTIL value supplied. Modify and run again.")
  }

  #c19 Adjustment
  if(c19deaths == 0){
    # no modificaiton to POP file.
  } else if(c19deaths == 1){
    # placeholder!
  } else if(c19deaths == 2){
    # placeholder!
  } else {
    print("ERROR! Improper c19deaths value supplied. Modify and run again.")
  }



# TOGGLE *MIGRATION* OVERRIDE (see Projection.R lines 363-375), 1 is ON, 0 is OFF
override = 1

# TOGGLE *FERTILITY* OVERRIDE (see Projection.R lines 15-20), 1 is ON, 0 is OFF
# ASFRoverride = 1

# TOGGLE *ZERO MIGRATION* OVERRIDE (see Projection.R lines 383-388), 1 is ON, 0 is OFF
#zeromigrationoverride = 0

  ##### OVERRIDES #####}

#set up variables that MigrationProjections needs
projstart <- series[i]
projend <- series[i+1]
projmidpoint  <- (projstart + projend) / 2
projyears <- seq(from=projstart,
                 to=projend - 1)

print(paste("Creating forecast for the period",projstart, "to", projend, sep=" "))

#run the projection code
source("src/Projection.R")

#save the MigrationProjections.R outputs in list format

POPPROJ[[as.character(projend)]] <- Projections

#save the Net Migration rates
NETMIGPROJ[[as.character(projend)]] <- Migration

#save the Components of Change
COMPONENTS[[as.character(projend)]] <- Components

#save the detailed Migration data
MIG_DETAIL[[as.character(projend)]] <- detailedMigs

#-------
  i <- i+1
}


######## Final Steps: unlist the population projections, last bits of formatting
#upload the finished projection lists to GitHub
save(POPPROJ, file="Output/PopProj.Rdata")
save(NETMIGPROJ, file="Output/NMProj.Rdata")
save(COMPONENTS, file="Output/ComponentsOfChange.Rdata")


#export projections
export <- tibble()
i=1
for(item in POPPROJ){
  print(item)
  temp <- item
  temp$year <- names(POPPROJ)[i]
  export <- bind_rows(export, temp)
  i <- i + 1
}

#load("Output/Base_Migration.Rdata") # named Base_Mig
start_Base_Mig <- start_Base_Mig %>% select(Region, Age, Sex, NetRates) %>% rename(NMRs = NetRates)
NETMIGPROJ[[1]] <- start_Base_Mig

projectedNetMigrationrates <- tibble()
i=1
for(item in NETMIGPROJ){
  print(item)
  temp <- item
  temp$year <- names(NETMIGPROJ)[i]
  projectedNetMigrationrates <- bind_rows(projectedNetMigrationrates, temp)
  i <- i + 1
}


components_all <- tibble()
i=1
for(item in COMPONENTS){
  temp <- item
  temp$year <- names(COMPONENTS)[i]
  components_all <- bind_rows(components_all, temp)
  i <- i + 1
}

Mig_Proj <- export %>% unique() %>% # we should think about renaming this variable - it's not really a migration projection, it's a population projection with migration included
  mutate(TNMtype = TNMnote) #add column that documents WHICH SET of target net migrant values were used for this projection

save(Mig_Proj, file="Output/Migration_Projections.Rdata")
#write.csv(Mig_Proj, "/Users/mweber/Desktop/Mig_Proj.csv")
#write.csv(target_NM_Sex_check, "/Users/mweber/Desktop/target_NM_Sex_check.csv")
#write.csv(base_year_NM_check, "/Users/mweber/Desktop/base_year_NM_check.csv")

