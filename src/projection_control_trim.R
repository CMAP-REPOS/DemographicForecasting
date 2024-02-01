# CMAP | Alexis McAdams, Mary Weber | 8/11/2021

###########  Projection Control file info  ###############

# Hey you! Make sure to run setup_control.R BEFORE running this script :)
#
# This script runs a loop to cycle through the Projection.R script over
# and over to get population projections in 5 year increments up to 2050.
# For each projection period, the Population projection and Net Migration
# are saved in lists (POPPROJ and NETMIGPROJ, respectively) and are used as
# input for the subsequent projection period.
#
# Formatting, export, and secondary projection products are handled in
# the script called "final_control.R"

######## load libraries ----------------

library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)

######## set-up projection start/end variables ---------------

baseyear <- 2020
startyear <- 2020
endyear <- 2050

most_recent_PEP <- 2022

projnums <- (endyear - startyear) / 5 #number of 5-year projection cycles to complete

series <- seq(from=startyear, to=endyear, by= 5)
# series[1] <- most_recent_PEP+1

######## set up the population projection and migration projection lists

POPPROJ <- list() # total population
for(years in series){
  POPPROJ[[as.character(years)]] <- tibble()
}

NETMIGPROJ <- list() # Net migrants
for(years in series){
  NETMIGPROJ[[as.character(years)]] <- tibble()
}

COMPONENTS <- list() # components of change (births, deaths, migrants)
for(years in series){
  COMPONENTS[[as.character(years)]] <- tibble()
}

MIG_DETAIL <- list() # net migrants, detailed data
for(years in series){
  MIG_DETAIL[[as.character(years)]] <- tibble()
}


######## run the loop ---------------

i <- 1
while(i <= projnums){

#set up variables for Projection.R
projstart <- series[i]
projend <- series[i+1]
projmidpoint  <- (projstart + projend) / 2
projyears <- seq(from = projstart,
                 to = projend - 1)

print(paste("Creating forecast for the period",projstart, "to", projend, sep=" "))

#run the projection code
source("src/Projection_test.R")

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

#belatedly put the starting migration rates in the final list (see Projection.R 1st proj period loop, ~ line 47)
start_Base_Mig <- start_Base_Mig %>% select(Region, Age, Sex, NetRates) %>% rename(NMRs = NetRates)
NETMIGPROJ[[1]] <- start_Base_Mig


######## Final Steps

#upload the finished projection lists to GitHub

save(POPPROJ, file="Output/PopProj.Rdata")
save(NETMIGPROJ, file="Output/NMProj.Rdata")
save(COMPONENTS, file="Output/ComponentsOfChange.Rdata")
save(MIG_DETAIL, file = "Output/MigTesting.Rdata")

#Recordkeeping list ("SETTINGS")
projection_options <- c('External IL Area Adjustment to Base Pop' = EXTIL,
                        'COVID19 Deaths Adjustment to Base Pop' = c19deaths,
                        'Total Net Migration Target Values' = TNMfilename,
                        'Coarse Migration Characteristics Override' = override,
                        'Zero Migration Scenario Override' = zeromigrationoverride
)
load("Output/recordkeeping.Rdata") # SETTINGS
SETTINGS[[2]] <- projection_options
save(SETTINGS, file = "Output/recordkeeping.Rdata")










#write.csv(Mig_Proj, "/Users/mweber/Desktop/Mig_Proj.csv")
#write.csv(target_NM_Sex_check, "/Users/mweber/Desktop/target_NM_Sex_check.csv")
#write.csv(base_year_NM_check, "/Users/mweber/Desktop/base_year_NM_check.csv")

