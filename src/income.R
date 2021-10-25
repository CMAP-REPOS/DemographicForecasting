# This script pulls 5- year ACS (2015-2019) income data by household, calculates quartiles, and applies the rates forward
# for all projection years (2025-2050).


library(tidyverse)
library(tidycensus)
library(readxl)

# Set parameters ----------------------------------------------------------

load("Output/PumaRegions.Rdata") #"puma_region" - key for identifying CMAP region PUMAs. Created in Age_0_4_PUMS_Breakdown.R script



# get data from ACS
# HINCP - household income
# ADJINC - adjustment factor for income and earnings dollar amounts; Use ADJINC to adjust HINCP to constant dollars.


pums_il <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "17", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "18", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "55", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)

pums_all <- bind_rows(pums_il, pums_in, pums_wi)





##### Working scraps below vvv
pums_il_2 <- pums_il %>%
  select(1:8) %>% left_join(puma_region, by=c("PUMA" = "PUMACE10", "ST" = "STATEFP10")) %>%
  filter(SPORDER == "1") %>%
  filter(!is.na(Region)) %>%
  mutate(HHincome = HINCP * as.numeric(ADJINC)) %>% #HH income x adjustment factor
  group_by(Region) %>% summarize(totHH = sum(WGTP))


cookco <- pums_il %>% filter(str_starts(PUMA, "031")) %>%
  filter(HINCP >= 0) %>%
  select(1:8)


pums_in <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "18", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "AGEP", "SEX"), state = "55", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 0:4), show_call = TRUE)
