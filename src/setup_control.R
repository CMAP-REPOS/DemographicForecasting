##################

##### Set-Up Control #########

# This script handles calling the scripts that set up the
# major inputs for the population projection process: base population,
# birth rates, mortality rates, and migration.


######## load libraries
#library(dplyr)
#library(tidyverse)
#library(readxl)
#library(ggplot2)

######## create and save shared resources -----------------

#resources for importing data
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

save(COUNTIES, CMAP_GEOIDS, file="Output/importhelpers.Rdata")




######### set options -----------------

#### fertility method choice
    # this option changes the method of the fertility calculation in Projection.R.
# if 1: model will use Approach A: log projection of 1990-2010 ASFRs, summed by Region
# if 0: model will use Approach B: calculate recent (2014-2018) ASFRs, summed by Region,
#       and project using trends outlined by Census Bureau's National Projections (2014)

fMethod = 1



########## run scripts to prepare set-up for projection -----------------

source("src/POP_PEP.R")
source("src/Age_0_4_PUMS_Breakdown.R")
source("src/Mortality.R")
source("src/GroupQuarters.R")

if(fMethod == 1){
  ASFR_projections <- read_excel("Input/berger_ASFRs.xlsx") %>%
    filter(Age != "10 - 14") %>%
    filter(Age != "45 - 49")
  save(ASFR_projections, file="Output/ASFR.Rdata")
}else if(fMethod == 0){
  source("src/Fertility.R")
}else{
  print("ERROR! Invalid ASFR method chosen, edit fMethod variable and run script again." )
}


source("src/recentMigration.R") # formerly "Migration.R"
source("src/pastMigration_ageGroupSums.R")


print("Set-up Script Complete!")

