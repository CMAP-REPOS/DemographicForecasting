# CMAP | Noel Peterson, Mary Weber | 6/3/2021

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

# Parameters ---------------------------------------------------------

load("Output/POP_PEP.Rdata")
load("Output/Age_0_4_Freq.Rdata")
AGE_0_4_FREQ$Population <- as.numeric(AGE_0_4_FREQ$Population)

MORT_YEARS <- c(2014:2018)
DEATHS_XLSX <- "Input/CMAPMortality1990-2019.xlsx"

# Create mortality age groups ---------------------------------------------------------

MORT_POP <- tibble()

for (YEAR in MORT_YEARS) {
   MORT_POP <- bind_rows(MORT_POP, POP[[as.character(YEAR)]])
}

# Load deaths data
Deaths <- read_excel(DEATHS_XLSX) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  filter(Year >= min(MORT_YEARS) & Year <= max(MORT_YEARS)) %>%
  mutate(Age = case_when(Age %in% c("85 to 89 years", "90 to 94 years", "95 years and over") ~ "85 years and over",
                         TRUE ~ Age)) %>%
  group_by(GEOID, Sex, Age, Year, Region) %>%
  summarize(Mortality = sum(Mortality), .groups = "drop")

# Join pop to deaths
MORT_DATA <- MORT_POP %>%
  full_join(Deaths, by=c('GEOID', 'Age', 'Sex', 'Year', 'Region')) %>%
  group_by(Age, Sex, Region) %>%
  summarise(Population = sum(Population),
            Mortality = sum(Mortality),
            .groups = "drop") %>%
  arrange(Region, desc(Sex))

# Use PUMA proportion estimates for 0-1 and 1-4 age group -------------------------------------

AGE_0_4_PROP <- AGE_0_4_FREQ %>% mutate(Age = case_when(AgeGroup == 'Less than 1 year' ~ '0 to 1 years',
                                        TRUE ~ AgeGroup)) %>% select(-AgeGroup, -Population)


MORT_DATA <- MORT_DATA %>% left_join(AGE_0_4_PROP, by=c('Age','Region', 'Sex'))



MORT_DATA <- MORT_DATA %>% mutate(Population = case_when(Age == '0 to 1 years' ~ lead(Population)*Age_0_4_Share,
                                                         Age == '1 to 4 years' ~ lag(Population)*Age_0_4_Share,
                                                          TRUE ~ Population)) %>%
                           select(-Age_0_4_Share) %>% subset(Age != '0 to 4 years')

# Life Tables Calculations ---------------------------------------------------------

LT <- tibble(Age = unique(Deaths$Age)) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>%
  add_column(Ax = c(0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))

LifeTable <- MORT_DATA %>%
  left_join(LT, by="Age") %>%
  select(Region, Sex, Age, Mortality, Population, x, Ax) %>%
  arrange(Region, desc(Sex), x) %>%
  group_by(Region, Sex) %>%
  mutate(Mx = (Mortality/as.numeric(Population)),
    n = case_when(Age == '0 to 1 years' ~ 1,
                  Age == '1 to 4 years' ~ 4,
                  Age == '85 years and over' ~ 2/Mx,
                  TRUE ~ 5),
    Qx = ifelse(Age == '85 years and over', 1,  # 85+ should always be 1
                (n*Mx/(1+n*(1-Ax)*Mx))),
    Px = (1-Qx),
    Ix = head(accumulate(Px, `*`, .init=100000), -1), # 0-1 should always be 100000
    Dx = (ifelse(Age == '85 years and over', Ix, Ix -lead(Ix))),
    Lx = (ifelse(Age == '85 years and over', Ix/Mx, n*(lead(Ix)+(Ax*Dx)))),
    temp = ifelse(Age == '85 years and over', Lx, 0),
    Tx = (ifelse(Age == '85 years and Over', Lx, accumulate(Lx, `+`, .dir = "backward"))),
    Ex = (Tx/Ix),
    Sx = case_when(Age == '0 to 1 years' ~ Lx/Ix,
                   Age == '1 to 4 years' ~ Lx/(lag(Lx)*4),
                   Age == '5 to 9 years' ~ Lx/(lag(Lx) + lag(Lx, n = 2)),
                   Age == '85 years and over' ~ Lx/(Lx +lag(Lx)),
                   TRUE ~ Lx/lag(Lx))
  ) %>%
  select(-temp) %>%
  relocate(c(x, n, Ax), .before= Mortality)%>%
  ungroup()

# Read in SSA tables -----------------------------------------------------------

SSA <- read_excel("Input/SSA.xlsx")

# Create final projections for each region  ------------------------------------

Mort_Proj <- LifeTable %>%
  select(Region, Sex, Age, Sx) %>%
  left_join(SSA, by= c("Sex", "Age")) %>%
  mutate(across(c(5:13), .fns = ~.*Sx)) %>%
  #select(-Sx)
  rename("2018" = Sx) #keep the calculated Sx

# Clean-up to values >= 1  ------------------------------------ This could use some adjustments to make it more dynamic

temp <- Mort_Proj %>% filter_at(vars(4:13), any_vars(. >= 1))
temp[7:13] <- temp$'2035'
#view(temp)

Mort_Proj <- rbind(Mort_Proj, temp)
Mort_Proj <- Mort_Proj %>% filter_at(vars(4:13), all_vars(. < 1))
#Mort_Proj_full <- Mort_Proj

# Calculate midpoints
Mort_Proj <- Mort_Proj %>% mutate('Mort2022.5'=rowMeans(across('2020':'2025')),
                                      'Mort2027.5'=rowMeans(across('2025':'2030')),
                                      'Mort2032.5'=rowMeans(across('2030':'2035')),
                                      'Mort2037.5'=rowMeans(across('2035':'2040')),
                                      'Mort2042.5'=rowMeans(across('2040':'2045')),
                                      'Mort2047.5'=rowMeans(across('2045':'2050')),
                                      'Mort2052.5'=rowMeans(across('2050':'2055')),
                                      'Mort2057.5'=rowMeans(across('2055':'2060'))) %>%
                                      select(c(1:3) | starts_with("Mort"))



save(Deaths, Mort_Proj, MORT_POP, file="Output/Mort_Proj.Rdata")
#save(LifeTable, file="Output/LifeTables.Rdata")

