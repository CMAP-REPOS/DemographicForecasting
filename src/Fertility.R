# CMAP | Mary Weber | 3/17/2021

library(tidyverse)
library(tidycensus)
library(readxl)

load("Output/PopData.Rdata")
load("Output/GQData.Rdata")

# For ASFR calculations, years of interest are 2010-2019

# Set parameters ----------------------------------------------------------

  GQE_YEARS <- c(2011:2019) #GQ estimate year range
  F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")
  
# Remove estimates of females in GQ from each year ----------------------------

  # Step 1: 2010 GQ population by county
  GQ_totals_2010 <- GQ %>% 
    filter(Category == 'County Total')  %>% 
    filter(Concept != 'GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE') %>% 
    group_by(GEOID, County, State, Year, Region, Age, Sex) %>%
    summarise(County_GQtotal = sum(Value))%>%
    ungroup() %>%
    select(GEOID, County, County_GQtotal)
  
  # Step 2: 2010 female GQ population by county, age
    FGQ_2010 <- GQ %>% 
      filter(Sex == 'Female')  %>% 
      filter(Concept != 'GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE') %>% 
      filter(Age %in% F_Groups) %>%
      group_by(GEOID, County, State, Year, Region, Age, Sex) %>%
      summarise(Female_GQ = sum(Value)) 
    
  # Step 3: For each GQ female age group of interest, calculate what proportion of each county's total 2010 GQ population they represent
    GQ2010 <- left_join(FGQ_2010, GQ_totals_2010) %>% mutate(Prop_Female = Female_GQ/County_GQtotal)
  
  # Step 4: Read in GQ Census estimates by State, County for 2011-2019
    GQE <- read_excel("Input/GQE.xlsx") 
    GQE$GEOID <- as.character(GQE$GEOID)
  
  # Step 5: Multiply 2010 proportions by 2011-2019 GQ estimates to get expected number of females
    GQE_projec <- left_join(GQ2010, select(GQE, c(GEOID, Year, Population)), by='GEOID') %>%
      mutate(GQE_pred = round(Prop_Female*Population),0) %>%
      rename(Year = Year.y) %>%
      select(GEOID, County, State, Sex, Age, GQE_pred, Year,Region)
  
  # Step 6: Female HH population estimates by County, Age and Sex for 2011-2019 
    TEMP_DATA <- tibble()
    for (YEAR in GQE_YEARS) {
      Pop_Est <- POP[[as.character(YEAR)]] %>%
      filter(Sex == 'Female')  %>% 
      filter(Age %in% F_Groups) %>%
      group_by(GEOID, County, State, Year, Region, Age) %>%
      summarise(County_Total = sum(Population))
      TEMP_DATA <- bind_rows(TEMP_DATA, Pop_Est)
    }
  
  # Step 7: For 2011-2019 subtract expected GQ population from county Total to get County HH populations
    HH_Pop <- left_join(GQE_projec, select(TEMP_DATA, c(GEOID, Year, Age, County_Total)), by=c('GEOID', 'Age', 'Year')) %>%
      mutate(Population = County_Total - GQE_pred) %>%
      select(GEOID, County, State, Sex, Year, Age, Population, Year,Region)

  # Step 8: Filter 2010 Decencial Census HH Data for females 15-44
    Female_HH_2010 <- POP[["2010"]]  %>%
        filter(Sex == 'Female') %>%
        filter(Age %in% F_Groups)
 
  # Step 9: Merge 2010 Census HH Population data and 2011-2019 HH Population Estimates to form a complete table of Household Population data
    F_Data <- bind_rows(Female_HH_2010, HH_Pop)
 

# Birth data - add pre-age-15 births to 15-19 group, add 45+ age births to 40-44 group ------------------
  
  Births <- read_excel("Input/Vital Stats IN.xlsx") %>% 
            mutate(Age = case_when(Age %in% c("10 to 14 years") ~ "15 to 19 years",
                                   Age %in% c("45 to 49 years") ~ "40 to 44 years",
                                   TRUE ~ Age))

# ASFR Calculation - # of live births per 1,000 women ------------------

  Births <- Births %>% group_by(State, Age, Year) %>% summarise(Births = sum(Births), .groups="drop")
  
  ASFR <- F_DATA %>% inner_join(Births, by = c("State", "Age", "Year")) %>% mutate(ASFR = round((Births/Population)*1000, 0))

# sample plots ---------

  ASFR %>% 
    filter(Year %in% c(2010:2019)) %>%
    ggplot(aes(x=Year, y=ASFR, group=Age, color=Age)) +  
    scale_x_continuous(breaks = c(2010:2019)) +
    ggtitle("2010-2019 ASFRs") + 
    geom_line() + 
    geom_point()
  


