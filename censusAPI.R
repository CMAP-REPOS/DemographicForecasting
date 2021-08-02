# CMAP | Noel Peterson, Mary Weber | 3/16/2021

#This file contains 1995, 2005 and 2011 - 2019 Population Estimates Program (PEP) data

#install.packages(c("tidyverse", "tidycensus", "readxl"))
library(tidyverse)
library(tidycensus)
library(readxl)
load("Output/PopData.Rdata") #must load, following code dependent on POP[[]]

# Set parameters ----------------------------------------------------------

install.packages('censusapi')
library(censusapi)
Sys.setenv(CENSUS_KEY="d94fbe16b1b053593223397765874bf147d1ae72")
readRenviron("~/.Renviron")



m <- listCensusMetadata(name = "acs/acs5", vintage =2009) #gives 5 year estimates for 2005 through 2009
View(m)


acs_group <- getCensus(name = "acs/acs5",
                       vintage = 2009,
                       vars = c("NAME", "B01003_001E"),
                       region = "county:*", regionin = "state:17") %>% rename(TotalPOP = B01003_001E)
head(acs_group)
