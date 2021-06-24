# CMAP | Mary Weber | 6/24/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/LifeTables.Rdata")
#read in SSA talbes for males and females


#multiple SSA tables by the life tables to create the final projections for each region
