


# load in required packages
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)

#run required scripts
source("src/employment.R") # for Job Forecast graphs


######### GRAPHS OF JOB FORECASTS

#Graph of the baseline jobs (all and non-local) from employment.R

p <- bind_rows(baseline_all, baseline_noloc) %>%
  ggplot(aes(x=Year, y = total_jobs, color = type, group = type)) +
  geom_point() + geom_line() + facet_wrap(~Region, scales = "free") +
  ggtitle("Number of Jobs, 2010-2050 \n Baseline Forecast") +
  theme(legend.position = "bottom")
p

#graph of all 3 of the job forecasts (all and non-local) from employment.R
q <- bind_rows(upside_all, upside_noloc, baseline_all, baseline_noloc, slowgrowth_all, slowgrowth_noloc) %>% ungroup() %>%
  mutate(category = paste(forecast,type,sep="_")) %>%
  ggplot(aes(x=Year, y=total_jobs, group = category, color=forecast, shape = type)) +
  geom_point() + geom_line() + facet_wrap(~Region, scales = "free") +
  ggtitle("Number of Jobs, 2010-2050 \n   All Forecasts (upside, baseline, and slow growth) \n    and All Jobs and Non-Local Industry Jobs") +
  theme(legend.position = "bottom")
q


######## GRAPHS OF POPULATION FORECASTS

#


######## GRAPHS OF WORKERS AND JOBS

#join employment baseline forecast with workers data, and graph # IN PROGRESS
workersandjobs <- left_join(workers, baseline2, by=c("Region", "year" = "Year")) %>%
  select(-workers, -totemp) %>%
  pivot_longer(cols = c(3:4), names_to = "type", values_to = "num")
q <- workersandjobs %>% ggplot(aes(x=year, y=num, color = type, group = type)) + geom_point() + geom_line() +  facet_wrap(~Region, scales = "free") + ggtitle("Jobs and Workers, 2010-2050 \n Local industries INcluded")
q



r <-
