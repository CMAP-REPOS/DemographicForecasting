# CMAP | Mary Weber | 8/18/2021

library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/Migration_Projections.Rdata") #Mig_Proj
load("Output/Head_of_HH.Rdata") #Head_of_HH
load("Output/Migration_Projections.Rdata") #temp

#add in 2020 data but also back project to 2010 and 2015

#lines 15-22 are duplicate from PEP code....should consolidate
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

baseyear2 = as.character(baseyr2)    #"2019"
startyear = as.character(projstart)  #"2019"
endyr = as.character(projend - 1)  #"2024"
cycleyears = projyears # c(2020,2021,2022,2023,2024)
lastyear = as.character(max(cycleyears))

Head_of_HH <- Head_of_HH %>% select(-Headship_Rate, -Head_HH, -Households, -Head_HH_Adjust)

if(startyear == baseyear2) {

  print(paste("GENERATING", baseyr2, "PROJECTION"))

  Head_of_HH <- Head_of_HH %>% mutate(Head_HH = round((HH_Pop_Male*Ratio_Adj)+(HH_Pop_Female*Ratio_Adj),0)) %>% select(-Year)


}else{

  Mig_Proj_Pop <- Mig_Proj %>% pivot_wider(names_from = Sex, values_from = ProjectedPop_final) %>% filter(Age != c('0 to 4 years', '5 to 9 years', '10 to 14 years')) %>%
  filter(year == projstart)

  Mig_Proj_Pop$year <- as.double(Mig_Proj_Pop$year)

  # update population with projections from migration code
  #Head_of_HH$Year = Mig_Proj_Pop$year
  Head_of_HH$Population_Female <- Mig_Proj_Pop$Female
  Head_of_HH$Population_Male <- Mig_Proj_Pop$Male

  # update GQ estimates, calculate base year HH population, calculate head of households
  Head_of_HH <- Head_of_HH %>% mutate(GQ_Estimates_Male = Population_Male * GQratio_Male)%>%
    mutate(GQ_Estimates_Female = Population_Female * GQratio_Female) %>%
    mutate(HH_Pop_Male = Population_Male - GQ_Estimates_Male)%>%
    mutate(HH_Pop_Female = Population_Female - GQ_Estimates_Female) %>%
    mutate(Head_HH = round((HH_Pop_Male*Ratio_Adj)+(HH_Pop_Female*Ratio_Adj),0))

  }




