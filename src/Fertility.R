# CMAP | Mary Weber | 2/10/2021

library(tidyverse)
library(tidycensus)
library(readxl)

load("Output/PopData.Rdata")

# Set parameters ----------------------------------------------------------

F_YEARS <- c(2010, 2015:2019) #will eventually include all years 2010 through 2020
F_Groups <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years")

# Filter data to only include female population within child-bearing years (15-44) ----------------------

F_DATA <- tibble()

for (YEAR in F_YEARS){
  
  TEMP_DATA <- POP[[as.character(YEAR)]] %>%
    filter(Sex == 'Female' & State == 'Indiana') %>%
    filter(Age %in% F_Groups) #may see instances where age groups appear twice due to data grouping in Population.R code
    F_DATA <- bind_rows(F_DATA, TEMP_DATA)
}

# Birth data - add pre-age-15 births to 15-19 group, add 45+ age births to 40-44 group ------------------

Births <- tibble() 
Births <- read_excel("Input/Vital Stats IN.xlsx") %>% 
          mutate(Age = case_when(Age %in% c("10 to 14 years") ~ "15 to 19 years",
                                 Age %in% c("45 to 49 years") ~ "40 to 44 years",
                                 TRUE ~ Age))

# ASFR Calculation - # of live births per 1,000 women ------------------

#STILL NEED TO REMOVE GQ 

F_DATA <- F_DATA %>% group_by(State, Age, Year)  %>% summarise(Population = sum(Population))
View(F_DATA)

Births <- Births %>% group_by(State, Age, Year)  %>% summarise(Births = sum(Births))
View(Births)

ASFR <- F_DATA %>% inner_join(Births, by = c("State", "Age", "Year")) %>% mutate(ASFR = round((Births/Population)*1000, 0))
View(ASFR)

# sample plots ---------

ASFR %>% 
  filter(Year %in% c(2010, 2015:2019)) %>%
  ggplot(aes(x=Year, y=ASFR, group=Age, color=Age)) +  
  scale_x_continuous(breaks = c(2010, 2015:2019)) +
  geom_line() + 
  geom_point()


#demography package

install.packages("demography")
library(demography)

ASFR <- F_DATA %>% inner_join(Births, by = c("State", "Age", "Year")) %>% mutate(ASFR = (Births/Population))


a <- as.matrix(ASFR$ASFR)
b <- as.matrix(ASFR$Population)
c <- as_vector(ASFR$Age)
d <- c(2015)

demogdata(a, b, c, d, type = "fertility", label="IN", name="total")

m <- demogdata(a, b, c, d, type = "fertility", label="IN", name="total")

lca(m, names(ASFR)[6], max.age = 45, adjust="dt", restype = 'logrates')



T <- ASFR %>%
    mutate(Year10 = '2010') %>% 
    mutate(Year15 = '2015') %>% 
    mutate(Year16 = '2016') %>% 
    mutate(Year17 = '2017') %>% 
    mutate(Year18 = '2018') %>% 
    mutate(Year19 = '2019') %>% 
    select(ASFR, Age, State, Population, Year10, Year15, Year16, Year17, Year18, Year19)



