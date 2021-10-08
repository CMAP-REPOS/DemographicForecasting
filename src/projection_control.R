# CMAP | Alexis McAdams, Mary Weber | 8/11/2021

         ###########  Projections control file info  ###############
#The concept: run a loop to cycle through the MigrationProjections script over
#and over to get population projections in 5 year increments up to 2050.
#For each projection period, the Population projection and Net Migration
#are saved in lists (POPPROJ and NETMIGPROJ, respectively) and are used as
#input for the subsequent projection period.
#When the loop is complete, the POPPROJ list is reformatted into a table
#called "Mig_Proj".

########load libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)


########set total projection control variables
baseyear <- 2020
startyear <- 2020
endyear <- 2050

projnums <- (endyear - startyear) / 5 #number of 5-year projection cycles to complete
projnums

series <- seq(from=startyear,
              to=endyear,
              by= 5)
series

target_NM_Sex_check <- tibble()
base_year_NM_check <- tibble()

#set which target net migration values you'd like to use for the projection (see target_netmigration folder for options)

target_NM <- read_excel("target_netmigration/TNM_workerjobbalance.xlsx") %>%
  mutate(Year = as.character(Year))
#name which net migration values you're using (important for documentation!)
tNMfile <-  "workerjobbalance TNMs, no ASFR override"


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

#import in Base Net Migration data
#NetMig <- read_excel("Input/NetMigration_Berger.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)
#NetMig <- read_excel("Input/NetMigration_ExpandedAgeGroups.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)
load("Output/pastMigration_ageGroupSums.Rdata") #netMigSums, derived from pastMigration_ageGroupSums.R
NetMig <- netMigSums %>% select(-Source)


######## run the loop

i <- 1
while(i <= projnums){

# TOGGLE *MIGRATION* OVERRIDE (see Mary_working.R lines 360-370), 1 is ON, 0 is OFF

override = 1

# TOGGLE *FERTILITY* OVERRIDE (see Mary_working.R lines 15-19), 1 is ON, 0 is OFF

ASFRoverride = 1

#set up variables that MigrationProjections needs
projstart <- series[i]
projend <- series[i+1]
projmidpoint  <- (projstart + projend) / 2
projyears <- seq(from=projstart,
                 to=projend - 1)

print(paste("Creating forecast for the period",projstart, "to", projend, sep=" "))

#run the projection code
source("src/Mary_working.R")

#save the MigrationProjections.R outputs in list format

POPPROJ[[as.character(projend)]] <- Projections

#save the Net Migration rates
NETMIGPROJ[[as.character(projend)]] <- Migration

#save the Components of Change
COMPONENTS[[as.character(projend)]] <- Components

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
  mutate(TNMtype = tNMfile) #add column that documents WHICH SET of target net migrant values were used for this projection

#save(Mig_Proj, file="Output/Migration_Projections.Rdata")
#write.csv(Mig_Proj, "/Users/mweber/Desktop/Mig_Proj.csv")
#write.csv(target_NM_Sex_check, "/Users/mweber/Desktop/target_NM_Sex_check.csv")
#write.csv(base_year_NM_check, "/Users/mweber/Desktop/base_year_NM_check.csv")

