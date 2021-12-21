library(tidyverse)
library(tidycensus)
library(ipumsr)
##### Preparation
# Geography
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

# Variables
HH_ID <- c('H007003', #NH_White
                  'H007004', #NH_Black
                  'H007005', #NH_AmIn
                  'H007006', #NH_Asian
                  'H007007', #NH_PI               These are householders (2000 and 2010 have the same IDs)
                  'H007008', #NH_Other
                  'H007009', #NH_More
                  'H007010') #Hisp

############################ Retrieving decennial data using tidycensus #############################
# Decenial 2000
Decennial_2000_POP <- tibble()
Decennial_2000_HH <- tibble()

POP_ID_2000 <- c('P008003', #NH_White
            'P008004', #NH_Black
            'P008005', #NH_AmIn
            'P008006', #NH_Asian
            'P008007', #NH_PI              These are population (2000 and 2010 have different IDs)
            'P008008', #NH_Other
            'P008009', #NH_More
            'P008010') #Hisp

for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = POP_ID_2000,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = 2000, survey = "sf1", cache_table = TRUE) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(variable = case_when(variable == 'P008003' ~ 'NH_White',
                                variable == 'P008004' ~ 'NH_Black',
                                variable == 'P008005' ~ 'NH_AmIn',
                                variable == 'P008006' ~ 'NH_Asian',
                                variable == 'P008007' ~ 'NH_PI',
                                variable == 'P008008' ~ 'NH_Other',
                                variable == 'P008009' ~ 'NH_More',
                                variable == 'P008010' ~ 'Hispanic')) %>%
    rename(Population = value, Race = variable)

  Decennial_2000_POP <- bind_rows(Decennial_2000_POP, TEMP)
}

for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = HH_ID,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = 2000, survey = "sf1", cache_table = TRUE) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(variable = case_when(variable == 'H007003' ~ 'NH_White',
                                variable == 'H007004' ~ 'NH_Black',
                                variable == 'H007005' ~ 'NH_AmIn',
                                variable == 'H007006' ~ 'NH_Asian',
                                variable == 'H007007' ~ 'NH_PI',
                                variable == 'H007008' ~ 'NH_Other',
                                variable == 'H007009' ~ 'NH_More',
                                variable == 'H007010' ~ 'Hispanic')) %>%
    rename(Householder = value, Race = variable)

  Decennial_2000_HH <- bind_rows(Decennial_2000_HH, TEMP)
}

Decennial_2000 <- cbind(Decennial_2000_POP,Decennial_2000_HH$Householder) %>%
  rename(Householder = `Decennial_2000_HH$Householder`)

Decennial_2000_other <- Decennial_2000 %>%
  filter(Race %in% c('NH_AmIn','NH_Other','NH_PI','NH_More')) %>%
  group_by(GEOID)


Decennial_sum <- Decennial_2000_other %>%
  summarise_at(vars(Population, Householder), list(name = sum))

colnames(Decennial_sum) <- c("GEOID", "Population", "Householder")

names(Decennial_sum, c("GEOID", "Population", "Householder"))

Decennial_sum <- right_join(unique(Decennial_2000_other[,1:3]), Decennial_sum)

Decennial_sum$Race <- "NH_Other"

Decennial_sum <- Decennial_sum %>% select("GEOID","County","State","Race","Population","Householder")

Decennial_2000_FINAL <- Decennial_2000 %>%
  filter(!Race %in% c('NH_AmIn','NH_Other','NH_PI','NH_More'))

Decennial_2000_FINAL <- rbind(Decennial_2000_FINAL,Decennial_sum)


Decennial_2000_FINAL$head.rate <- Decennial_2000_FINAL$Householder/Decennial_2000_FINAL$Population

Decennial_2000_FINAL$Year <- 2000

##### Decenial 2010
POP_ID_2010 <- c('P005003', #NH_White
            'P005004', #NH_Black
            'P005005', #NH_AmIn
            'P005006', #NH_Asian
            'P005007', #NH_PI              These are population
            'P005008', #NH_Other
            'P005009', #NH_More
            'P005010') #Hisp

Decennial_2010_POP <- tibble()
Decennial_2010_HH <- tibble()

for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = POP_ID_2010,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = 2010, survey = "sf1", cache_table = TRUE) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(variable = case_when(variable == 'P005003' ~ 'NH_White',
                                variable == 'P005004' ~ 'NH_Black',
                                variable == 'P005005' ~ 'NH_AmIn',
                                variable == 'P005006' ~ 'NH_Asian',
                                variable == 'P005007' ~ 'NH_PI',
                                variable == 'P005008' ~ 'NH_Other',
                                variable == 'P005009' ~ 'NH_More',
                                variable == 'P005010' ~ 'Hispanic')) %>%
    rename(Population = value, Race = variable)

  Decennial_2010_POP <- bind_rows(Decennial_2010_POP, TEMP)
}

for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = HH_ID,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = 2010, survey = "sf1", cache_table = TRUE) %>%
    separate(NAME, c("County", "State"), sep = "\\, ") %>%
    mutate(variable = case_when(variable == 'H007003' ~ 'NH_White',
                                variable == 'H007004' ~ 'NH_Black',
                                variable == 'H007005' ~ 'NH_AmIn',
                                variable == 'H007006' ~ 'NH_Asian',
                                variable == 'H007007' ~ 'NH_PI',
                                variable == 'H007008' ~ 'NH_Other',
                                variable == 'H007009' ~ 'NH_More',
                                variable == 'H007010' ~ 'Hispanic')) %>%
    rename(Householder = value, Race = variable)

  Decennial_2010_HH <- bind_rows(Decennial_2010_HH, TEMP)
}

Decennial_2010 <- cbind(Decennial_2010_POP,Decennial_2010_HH$Householder) %>%
  rename(Householder = `Decennial_2010_HH$Householder`)

Decennial_2010_other <- Decennial_2010 %>%
  filter(Race %in% c('NH_AmIn','NH_Other','NH_PI','NH_More')) %>%
  group_by(GEOID)


Decennial_sum <- Decennial_2010_other %>%
  summarise_at(vars(Population, Householder), list(name = sum))

colnames(Decennial_sum) <- c("GEOID", "Population", "Householder")

names(Decennial_sum, c("GEOID", "Population", "Householder"))

Decennial_sum <- right_join(unique(Decennial_2010_other[,1:3]), Decennial_sum)

Decennial_sum$Race <- "NH_Other"

Decennial_sum <- Decennial_sum %>% select("GEOID","County","State","Race","Population","Householder")

Decennial_2010_FINAL <- Decennial_2010 %>%
  filter(!Race %in% c('NH_AmIn','NH_Other','NH_PI','NH_More'))

Decennial_2010_FINAL <- rbind(Decennial_2010_FINAL,Decennial_sum)


Decennial_2010_FINAL$head.rate <- Decennial_2010_FINAL$Householder/Decennial_2010_FINAL$Population

Decennial_2010_FINAL$Year <- 2010

############################### Retrieve 1990 data from IPUMS ##############################



########################## Combine 1990, 2000, 2010 data together ##########################

# just combine the data from tidycensus at first
Decennial_2000_2010 <- rbind(Decennial_2000_FINAL, Decennial_2010_FINAL)
Decennial_2000_2010 <- Decennial_2000_2010 %>%
  select("GEOID", "County", "State", "Race", "Year", "Population", "Householder", "head.rate")
Decennial_2000_2010 <- Decennial_2000_2010[order(Decennial_2000_2010$GEOID, Decennial_2000_2010$Race, Decennial_2000_2010$Year),]

##### Export Data
write.csv(Decennial_2000_2010, "C:/Users/hshi/Desktop/Demographic_Prediction/HH_2000_2010.csv",
          quote = F,
          row.names = F)
