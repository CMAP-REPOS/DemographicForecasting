# CMAP | Alexis McAdams, Mary Weber | 13 Aug 2021

#This script imports historical Net Migration values and calculates Target Net Migrants for use in the Demographic Projection.

#The current method: The calculated net migration in/out of the four regions from the following 5-year time periods are averaged. 
  #1990 - 1995    source: Berger
  #1995 - 2000    source: Berger
  #2000 - 2005    source: Berger
  #2005 - 2010    source: Berger
  #2014 - 2018    source: CMAP calculation, sourced from Census PEP and County Vital Stats (Births and Deaths)

NetMig <- read_excel("Input/NetMigration_Berger.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)

target_NM <- NetMig %>% filter(Age == 'Total' & Sex == 'Both') %>% select(-Period) %>%
  group_by(Region) %>% summarise(NetMigration = round (mean(NetMigration),-3)) #round to nearest thousand
  
 save(target_NM, file"Output/targetNM.Rdata")
