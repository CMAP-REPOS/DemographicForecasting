#Employment data

#this script reads in the baseline employment data from the consultant report and formats it for graphing

library(dplyr)
library(tidyverse)
library(readxl)

localindustries <- c("44-45", "71", "72", "81") #local industries that do not drive in-migration (Berger assumption)

#read in detailed employment forecast data
baseline <- read_excel("C:/Users/amcadams/OneDrive - Chicago Metropolitan Agency for Planning/Documents/Demographic Model Project/employment_report/Scenario_Detailed_Forecasts_2021-08-05/baseline_detailed.xlsx",
                       na = "0.0") %>%
  mutate(locsplit = case_when(industry_code %in% localindustries ~ "local",
                              TRUE ~ "non-local"))

#borrow the fips-to-county-and-region key from the PEP2020 input data and join to baseline
pepdata <- read_excel("Input/PEP2020.xlsx") %>%
  select(1:3, 8) %>%
  unique() %>%
  mutate(GEOID = as.character(GEOID))

baseline <- baseline %>%
  left_join(pepdata, by = c("area_fips" = "GEOID"))

#copied out OLD baseline here
#oldbaseline <- baseline (OPE)

baseline_all <- baseline %>% #total number of jobs by region and year
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * 1.049) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "all_job_industries", forecast = "baseline") %>%

baseline_noloc <- baseline %>% #total number of jobs by region and year NOT INCLUDING the local employment NAICS codes
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  filter(locsplit == "non-local") %>%
  group_by(Region, Year) %>%
  summarize(total_jobs = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(total_jobs = total_jobs * 1.049) %>% #constant to address multiple-job-holders. The resulting # is "Primary Jobs"
  mutate(type = "jobs_minus_local_industries", forecast = "baseline")

#let's combine the two and plot to see what it looks like

p <- bind_rows(baseline_all, baseline_noloc) %>% ggplot(aes(x=Year, y = total_jobs, color = type, group = type)) + geom_point() + geom_line() + facet_wrap(~Region, scales = "free") + ggtitle("Number of Jobs, 2010-2050 \n baseline forecast")
p


#let's try to join it with the workers data, and then graph it
workersandjobs <- left_join(workers, baseline2, by=c("Region", "year" = "Year")) %>%
  select(-workers, -totemp) %>%
  pivot_longer(cols = c(3:4), names_to = "type", values_to = "num")


q <- workersandjobs %>% ggplot(aes(x=year, y=num, color = type, group = type)) + geom_point() + geom_line() +  facet_wrap(~Region, scales = "free") + ggtitle("Jobs and Workers, 2010-2050 \n Local industries INcluded")
q



export3 <- export %>%
  pivot_wider(names_from = year, values_from = ProjectedPop_final)
write.csv(export3, file = "C:/Users/amcadams/Documents/R/projections_wide.csv")

write.csv(target_NM, file = "C:/Users/amcadams/Documents/R/targetNMs.csv")
