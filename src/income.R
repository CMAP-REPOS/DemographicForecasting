# income.R
# CMAP | Alexis McAdams | Nov 2021

# This script pulls 5- year ACS (2015-2019) income data by household, calculates
# the number of households by quantile, (same definition of quantile bins across all regions),
# and applies the HH/quantile rate forward across subsequent projection years (2025-2050).
# Income is in ~2012 dollars~ (See lines 40-41)


library(tidyverse)
library(tidycensus)
library(readxl)

# Set parameters ----------------------------------------------------------

load("Output/PumaRegions.Rdata") #"puma_region" - key for identifying CMAP region PUMAs. Created in Age_0_4_PUMS_Breakdown.R script

# STEP 1: get data from ACS
# HINCP - household income
# ADJINC - adjustment factor for income and earnings dollar amounts; Use ADJINC to adjust HINCP to 2019 dollars (see line 41)

pums_il <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "17", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "18", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "55", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)

# put all 3 state ACS data together
pums_all <- bind_rows(pums_il, pums_in, pums_wi)

# STEP 2: By County, divide households into 16 income groups (defined by Berger - consider modifying in future?)
# reformat ACS income data
###~~~ Equal income groupings for all Regions

#REcategorize and re-summarize data (not the same as above!)
pums_data <- pums_all %>%
  select(1:8) %>%
  left_join(puma_region, by=c("PUMA" = "PUMACE10", "ST" = "STATEFP10")) %>% #join to "puma_region" key (which counties are in which region)
  filter(!is.na(Region))  %>% #filter out all data from areas outside of 4 regions
  rowwise() %>% mutate(HINC_19 = HINCP * as.numeric(ADJINC)) %>%  # multiply income by adjustment factor (result: all $$$ in 2019 dollars)
  mutate(HINC_12 = HINC_19 * 0.652 * 1.377, .drop = "unused") %>%  # multiply income to convert to 1999, then 2012 dollars: https://usa.ipums.org/usa/cpi99.shtml
  mutate(inc_group = case_when(HINC_12 < 10000 ~ "0_to_10k",    # categorize each of the incomes into income groupings
                               HINC_12 <= 14999 ~ "10_to_15k",
                               HINC_12 <= 19999 ~ "15_to_20k",
                               HINC_12 <= 24999 ~ "20_to_25k",
                               HINC_12 <= 29999 ~ "25_to_30k",
                               HINC_12 <= 34999 ~ "30_to_35k",
                               HINC_12 <= 39999 ~ "35_to_40k",
                               HINC_12 <= 44999 ~ "40_to_45k",
                               HINC_12 <= 49999 ~ "45_to_50k",
                               HINC_12 <= 59999 ~ "50_to_60k",
                               HINC_12 <= 74999 ~ "60_to_75k",
                               HINC_12 <= 99999 ~ "75_to_100k",
                               HINC_12 <= 124999 ~ "100_to_125k",
                               HINC_12 <= 149999 ~ "125_to_150k",
                               HINC_12 <= 199999 ~ "150_to_200k",
                               TRUE ~ "200_and_over" ))
pums_data <- pums_data %>% group_by(inc_group) %>%
  summarize(tot_incgroup = sum(WGTP), .groups = "drop")

# STEP 3: For 21 county region:
#                     calculate what % of total households each grouping represents (household income group / total household count)
#                     calculate the cumulative percentage (highest income grouping should equal 100%)
#                     use the cumulative percentages to separate the income groupings into quantiles
# calculate % by income grouping, assign quantile, and calculate percentage of households in each quantile

# total number of households (all 21 counties)
pums_total <- pums_data %>% summarize(total = sum(tot_incgroup))
#
pums_perc <- pums_data %>%
  mutate(total = pums_total[1,1] %>% as.numeric()) %>% # join total # of HH to income groupings
  rowwise() %>% mutate(incperc = round((tot_incgroup / total) * 100,5)) %>% # calculate percentage of HH in each income grouping
  mutate(x_min = as.numeric(str_split_fixed(inc_group, "_", 2)[,1])) %>% # grab the min value from the income grouping
  mutate(x_max = str_remove(str_split_fixed(inc_group, "_", 3)[,3], "k")) %>% mutate(x_max = case_when(x_max == "over" ~ "-1", TRUE ~ x_max)) %>% mutate(x_max = as.numeric(x_max)) %>%
  arrange(x_min) %>% # create column for proper sorting of income groupings (low to high)
  ungroup() %>% #remove rowwise grouping (required for cumulative sum function)
  mutate(perc_cumulative = cumsum(incperc)) %>% #calculate cumulative percentage
  mutate(quantile = case_when(perc_cumulative < 25 ~ "1st_quantile", # assign quantile name to each income grouping
                              perc_cumulative < 50 ~ "2nd_quantile",
                              perc_cumulative < 75 ~ "3rd_quantile",
                              TRUE ~ "4th_quantile"))

# create bin key (min and max income for each quantile bin, used for final formatting later)
temp <- pums_perc %>% select(quantile, x_min, x_max) %>%
  group_by(quantile) %>%
  mutate(mininc = min(x_min) * 1000,  # fix max/min bin numbers
         maxinc = max(x_max) * 1000, .keep = "unused") %>%
  ungroup() %>% unique() %>%
  mutate(maxinc = case_when(maxinc >= 200000 ~ -1,
                            TRUE ~ maxinc ))

# total up percentage of HH in each quantile
quantiles2 <- pums_perc %>% select(quantile, incperc) %>% group_by(quantile) %>%
  summarize(hh_quant_perc = sum(incperc), .groups = "drop") %>%
  left_join(temp, by=c("quantile")) %>%
  mutate(JoinCol = "All") #used for joining to Households data later

#import and reformat Households data
load("Output/HH_PROJ.Rdata") # HH_PROJ

Households <- tibble()
i=1
for(item in HH_PROJ){
  temp2 <- item
  temp2$Year <- names(HH_PROJ)[i]
  Households <- bind_rows(Households, temp2)
  i <- i + 1
}

Households <- Households %>%
  group_by(Year, Region) %>% summarize(totalHH = sum(Head_HH)) %>% ungroup() %>%
  mutate(JoinCol = "All")

# join tables and apply the quantile percentage to the total number of Households
HH_incomes <- Households %>%
  full_join(quantiles, by = "JoinCol") %>%
  rowwise() %>% mutate(households = round(totalHH * (hh_quant_perc / 100),0)) %>%
  select(Region, Year, households, quantile, mininc, maxinc) %>%
  ungroup()

save(HH_incomes, file="Output/HH_income_byQuantile.Rdata")

