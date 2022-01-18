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

######## create and save shared resources

#resources for importing data
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

save(COUNTIES, CMAP_GEOIDS, file="Output/importhelpers.Rdata")




######### set options

#### set External Illinois Adjustment area override
# if 1: model will use travel model boundary-adjusted population for LOL (Lee, Ogle, LaSalle) counties
# if 0: model will use full county populations for the LOL counties, no adjustment will be applied.

EXTIL = 1




########## run scripts to prepare set-up for projection

source("src/POP_PEP.R")


