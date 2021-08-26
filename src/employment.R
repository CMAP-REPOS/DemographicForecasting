#Employment data

#this script reads in the baseline employment data from the consultant report and formats it for easy ggplot graphing

########### Preparation

# load libraries
library(dplyr)
library(tidyverse)
library(readxl)

#define necessary variables
localindustries <- c("44-45", "71", "72", "81") #local industries that do not drive in-migration (Berger assumption)
multijob_adjustment <- 1.049 #Berger's assumption (from https://fred.stlouisfed.org/series/LNS12026620 data) to address workers that hold multiple jobs

#read in detailed baseline employment forecast data and create definition of "Local" and "Non-Local" industries according to NAICS codes in line 11
baseline <- read_excel("C:/Users/amcadams/OneDrive - Chicago Metropolitan Agency for Planning/Documents/Demographic Model Project/employment_report/Scenario_Detailed_Forecasts_2021-08-05/baseline_detailed.xlsx",
                       na = "0.0") %>%
  mutate(locsplit = case_when(industry_code %in% localindustries ~ "local",
                              TRUE ~ "non-local"))

#borrow the fips-to-county-and-region PEP2020 input data to serve as a key, join it to baseline
pepdata <- read_excel("Input/PEP2020.xlsx") %>%
  select(1:3, 8) %>%
  unique() %>%
  mutate(GEOID = as.character(GEOID))

########### Creating the summary tables for each forecast by Region and by Local/Non-Local industries

#Baseline summed employment data:
baseline <- baseline %>%
  left_join(pepdata, by = c("area_fips" = "GEOID"))

baseline_all <- baseline %>% #total number of jobs by region and year
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * multijob_adjustment) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "all_job_industries", forecast = "baseline")

baseline_noloc <- baseline %>% #total number of jobs by region and year NOT INCLUDING the local employment NAICS codes
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  filter(locsplit == "non-local") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * multijob_adjustment) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "jobs_minus_local_industries", forecast = "baseline")


#Upside summed employment data:
upside <- read_excel("C:/Users/amcadams/OneDrive - Chicago Metropolitan Agency for Planning/Documents/Demographic Model Project/employment_report/Scenario_Detailed_Forecasts_2021-08-05/upside_detailed.xlsx",
                       na = "0.0") %>%
  mutate(locsplit = case_when(industry_code %in% localindustries ~ "local",
                              TRUE ~ "non-local"))
upside <- upside %>%
  left_join(pepdata, by = c("area_fips" = "GEOID"))

upside_all <- upside %>% #total number of jobs by region and year
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * multijob_adjustment) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "all_job_industries", forecast = "upside")

upside_noloc <- upside %>% #total number of jobs by region and year NOT INCLUDING the local employment NAICS codes
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  filter(locsplit == "non-local") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * multijob_adjustment) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "jobs_minus_local_industries", forecast = "upside")

#Slowgrowth summed employment data:
slowgrowth <- read_excel("C:/Users/amcadams/OneDrive - Chicago Metropolitan Agency for Planning/Documents/Demographic Model Project/employment_report/Scenario_Detailed_Forecasts_2021-08-05/slowgrowth_detailed.xlsx",
                     na = "0.0") %>%
  mutate(locsplit = case_when(industry_code %in% localindustries ~ "local",
                              TRUE ~ "non-local"))
slowgrowth <- slowgrowth %>%
  left_join(pepdata, by = c("area_fips" = "GEOID"))

slowgrowth_all <- slowgrowth %>% #total number of jobs by region and year
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * multijob_adjustment) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "all_job_industries", forecast = "slow_growth")

slowgrowth_noloc <- slowgrowth %>% #total number of jobs by region and year NOT INCLUDING the local employment NAICS codes
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  filter(locsplit == "non-local") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * multijob_adjustment) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "jobs_minus_local_industries", forecast = "slow_growth")
