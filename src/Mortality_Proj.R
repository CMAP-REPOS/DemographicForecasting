# CMAP | Mary Weber | 6/24/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/LifeTables.Rdata")
regions <- unique(a$Region)

SSA <- read_excel("/Users/mweber/Desktop/SSA.xlsx", col_names = T)


#multiple SSA tables by the life tables to create the final projections for each region
Male_Proj <- a %>%
        select(Region, Sex, Age, Sx) %>%
        left_join(SSA_Male, by= c("Sex", "Age")) %>%
        mutate(across(c(3:11), .fns = ~.*Sx)) %>%
        select(-Sx)

View(Male_Proj)
