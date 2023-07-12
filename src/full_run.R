# CMAP | Alex Bahls | 7/11/2023

##### full_run.R

#what the diff fertility things are/how to program them in -- set up control

#set params (maybe this should be its own file; think of what else could go in)


# fMethod = 1 # see set up controls
# HeadshipSource = 1 #see final control file
# recentData = 0  #see final control file
#
# EXTIL = 0 #see projection control
#
#
# setup_options <- c('fMethod' = fMethod) # add additional named items when other options are implemented/exercised.
#
# SETTINGS <- list()
# SETTINGS <- list("Setup Options" = setup_options, "Projection Options" = NULL, "Export Options" = NULL)
# save(SETTINGS, file = "Output/recordkeeping.Rdata")


COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

source("src/POP_PEP.R") #1

#these two need to be run sequentially 2a
source("src/Age_0_4_PUMS_Breakdown.R")
source("src/Mortality.R")


#these two need to be run sequentially; can be 2b
source("src/GroupQuarters.R")
source("src/Fertility.R")

#these need to be run in order
source("src/recentMigration.R")
source("src/pastMigration_ageGroupSums.R")
source("src/projection_control.R")

source("src/workforce.R")


source("src/race_ethnicity_projection.R")

source("src/HH_Control.R")

source("src/final_control.R")
