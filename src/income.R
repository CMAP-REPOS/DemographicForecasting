# This script pulls 5- year ACS (2015-2019) income data by household, calculates
# quantiles, and applies the rates forward across the number of households for
# all subsequent projection years (2025-2050).


library(tidyverse)
library(tidycensus)
library(readxl)

# Set parameters ----------------------------------------------------------

load("Output/PumaRegions.Rdata") #"puma_region" - key for identifying CMAP region PUMAs. Created in Age_0_4_PUMS_Breakdown.R script

# STEP 1: get data from ACS
# HINCP - household income
# ADJINC - adjustment factor for income and earnings dollar amounts; Use ADJINC to adjust HINCP to 2019 dollars.

pums_il <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "17", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "18", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "HINCP", "ADJINC"), state = "55", year = 2019, survey = "acs5",
                    variables_filter = list(SPORDER = 1), rep_weights = "housing", show_call = TRUE)

# put all 3 state ACS data together
pums_all <- bind_rows(pums_il, pums_in, pums_wi)

# reformat ACS income data
pums_test <- pums_all %>%
  select(1:8) %>%
  left_join(puma_region, by=c("PUMA" = "PUMACE10", "ST" = "STATEFP10")) %>% #join to "puma_region" key (which counties are in which region)
  filter(!is.na(Region))  %>% #filter out all data from areas outside of 4 regions
  rowwise() %>% mutate(HINC_19 = HINCP * as.numeric(ADJINC)) %>%  #multiply income by adjustment factor (result: all $$$ in 2019 dollars)
  mutate(inc_group = case_when(HINC_19 < 10000 ~ "0_to_10k",    # categorize each of the incomes into income groupings
                               HINC_19 <= 14999 ~ "10_to_15k",
                               HINC_19 <= 19999 ~ "15_to_20k",
                               HINC_19 <= 24999 ~ "20_to_25k",
                               HINC_19 <= 29999 ~ "25_to_30k",
                               HINC_19 <= 34999 ~ "30_to_35k",
                               HINC_19 <= 39999 ~ "35_to_40k",
                               HINC_19 <= 44999 ~ "40_to_45k",
                               HINC_19 <= 49999 ~ "45_to_50k",
                               HINC_19 <= 59999 ~ "50_to_60k",
                               HINC_19 <= 74999 ~ "60_to_75k",
                               HINC_19 <= 99999 ~ "75_to_100k",
                               HINC_19 <= 124999 ~ "100_to_125k",
                               HINC_19 <= 149999 ~ "125_to_150k",
                               HINC_19 <= 199999 ~ "150_to_200k",
                               TRUE ~ "200_and_over" )) %>%
  group_by(Region, inc_group) %>% summarize(tot_incgroup = sum(WGTP)) #sum up the number of HH in each of the income groupings

# calculate percentages by income grouping, assign quantile, and calculate percentage of households in each quantile
pums_perc <- pums_test %>% group_by(Region) %>% summarize(total = sum(tot_incgroup)) %>% #calculate total # of households by region
  right_join(pums_test, by="Region") %>% #join total # of HH to income groupings
  rowwise() %>% mutate(incperc = round((tot_incgroup / total) * 100,2)) %>% #calculate percentage of HH in each income grouping
  mutate(x_min = as.numeric(str_split_fixed(inc_group, "_", 2)[,1])) %>% #grab the min value from the income grouping
  mutate(x_max = str_remove(str_split_fixed(inc_group, "_", 3)[,3], "k")) %>% mutate(x_max = case_when(x_max == "over" ~ "-1", TRUE ~ x_max)) %>% mutate(xmax = as.numeric(x_max)) %>%
  arrange(Region, x_min) %>% #create column for proper sorting of income groupings (low to high)
  group_by(Region) %>% mutate(perc_cumulative = cumsum(incperc)) %>% #calculate cumulative percentage
  rowwise() %>%
  mutate(quantile = case_when(perc_cumulative < 25 ~ "1st_quantile", # assign quartile to each income grouping
                              perc_cumulative < 50 ~ "2nd_quantile",
                              perc_cumulative < 75 ~ "3rd_quantile",
                              TRUE ~ "4th_quantile"))

temp <- pums_perc %>% group_by(Region, quantile) %>%
  mutate(mininc = min(x_min), maxinc = as.numeric(max(x_max))) %>% ungroup() %>% #grab the min and max income for each quantile
  mutate(maxinc = case_when(maxinc >= 200 ~ -1, TRUE ~ maxinc )) %>% # fix for the top quantile (no upper bound)
  select(Region, quantile, mininc, maxinc)

pums_perc2 <- pums_perc %>%
  group_by(Region, quantile) %>%
  summarize(total = total, totperc = sum(incperc), tot_quantile = sum(tot_incgroup)) %>% unique() %>% # total up number of households in each quantile
  rowwise() %>% mutate(hh_quant_perc = (tot_quantile / total) * 100 ) # calculate percentage of households in each quantile

quantiles <- pums_perc2 %>% select(Region, quantile, hh_quant_perc) %>% # isolate hh quantile percentage by region (ready to apply to projected # of households)
  left_join(temp, by=c("Region", "quantile")) %>% unique() #tack on min and max income values for each quantile



####

temp <- pums_perc %>% group_by(Region, quantile) %>% mutate(Region = Region, mininc = min(x_min), maxinc = max(x_max))

inc_quant <- pums_all %>%
  select(1:8) %>%
  left_join(puma_region, by=c("PUMA" = "PUMACE10", "ST" = "STATEFP10")) %>% #join to "puma_region" key
  filter(!is.na(Region)) %>% #filter out all data from areas outside of 4 regions
  rowwise() %>% mutate(HINC_19 = HINCP * as.numeric(ADJINC))

temp <- inc_quant %>% filter(Region == "CMAP Region") %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x)


# STEP 2: By County, divide households into 16 income groups as defined by Berger
# STEP 3: By County, calculate what % of total households each grouping represents (household income group / total household count)
# STEP 4: By County, calculate the cumulative percentage (highest income grouping should equal 100%)
# STEP 5: By County, use the cumulative percentages to separate the income groupings into quantiles
          # simply looking for the income group closest to, but less than, the quantile in question
          # so if there's an income group with a cumulative % of 24 and the next grouping is 27%, the 24% grouping is the cut off for the 25% group
# STEP 6: Once quantile groupings are identified, for each quantile sum up the % of total households the quantile represents (values from Step 2)



group_by(Region) #summarize(n = sum(WGTP)) #household weighting factor

regions <- c(distinct(puma_region, Region)) %>% unlist()

for(item in regions){
  temp <- inc_quant %>% filter(Region == item) %>%
    rowwise() %>%
    rep()

}




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
