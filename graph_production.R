


# load in required packages
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)

#run required scripts
source("src/employment.R") # for Job Forecast graphs

source("src/workforce.R") # for Worker calculation graphs, is built off of Projection results


######### GRAPHS OF JOB FORECASTS

#Graph of the baseline jobs (all and non-local) from employment.R

p <- bind_rows(baseline_all, baseline_noloc) %>%
  ggplot(aes(x=Year, y = total_jobs, color = type, group = type)) +
  geom_point() + geom_line() + facet_wrap(~Region, scales = "free") +
  ggtitle("Number of Jobs, 2010-2050 \n Baseline Forecast") +
  theme(legend.position = "bottom")
p

#graph of all 3 of the job forecasts (all and non-local) from employment.R
alljobforecasts <- bind_rows(upside_all, upside_noloc, baseline_all, baseline_noloc, slowgrowth_all, slowgrowth_noloc)
q <- alljobforecasts %>% ungroup() %>%
  mutate(category = paste(forecast,type,sep="_")) %>%
  ggplot(aes(x=Year, y=total_jobs, group = category, color=forecast, shape = type)) +
  geom_point() + geom_line() + facet_wrap(~Region, scales = "free") +
  ggtitle("Number of Jobs, 2010-2050 \n   All Forecasts (upside, baseline, and slow growth) \n    and All Jobs and Non-Local Industry Jobs") +
  theme(legend.position = "bottom")
q


######## GRAPHS OF POPULATION FORECASTS

#~~~build graphs of total male and female population projections for each region and year
pop_proj <- export
pop_proj_totals <- pop_proj %>% group_by(Region, Sex, year) %>% summarize(total_population = sum(ProjectedPop_final))
p <- export_totals %>% ggplot(aes(x=year, y=total_population, color=Sex, group = Sex)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population, 2025-2050", subtitle = paste("Target Net Migration values: ", tNMfile))
p

#pull in data from 2010,2015,2020 and add to total population graphs for each region and year
pop2020 <- POP[['2020']] %>%
  mutate(GEOID = as.character(GEOID)) #extra step to fix the GEOID type and make sure the bind_rows below works
recentdata <- bind_rows(POP[['2010']], POP[['2015']], pop2020) %>%
  select(Year, Age, Region, Sex, Population) %>%
  mutate(type = "PEPestimate")
pop_proj <- export %>% rename(Population = ProjectedPop_final) %>% mutate(Year = as.numeric(year), type = "Projection") %>% select(-year)

pop_recandproj <- bind_rows(recentdata, pop_proj) ###population, recent and projected

pop_totals <- pop_recandproj %>% group_by(Region, Sex, Year, type) %>% summarize(totpop = sum(Population))
q <- pop_totals %>% ggplot(aes(x=Year, y=totpop, color = Sex, group = Sex, shape = type)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population, estimated and projected, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom")
q



export_allpop <- bind_rows(rename(export, population = ProjectedPop_final), rename(exporttemp2, population = totpop))
export_allpop$year <- as.integer(export_allpop$year)


write.csv(export_allpop, file = "C:/Users/amcadams/Documents/R/projections_exportallpops_23AUG.csv")




######## GRAPHS OF WORKERS AND JOBS

#join employment baseline forecast with workers data, and graph # IN PROGRESS
workersandjobs <- left_join(workers, baseline2, by=c("Region", "year" = "Year")) %>%
  select(-workers, -totemp) %>%
  pivot_longer(cols = c(3:4), names_to = "type", values_to = "num")
q <- workersandjobs %>% ggplot(aes(x=year, y=num, color = type, group = type)) + geom_point() + geom_line() +  facet_wrap(~Region, scales = "free") + ggtitle("Jobs and Workers, 2010-2050 \n Local industries INcluded")
q

#plot the number of workers
p <- workers %>% ggplot(aes(x=Year, y=workers, group = Region, shape = type)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Number of Workers, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile))
p

#plot the number of workers WITH the jobs forecasts
tempworkers <- workers %>%
  rename(value = workers) %>%
  mutate(valuename = "workers")
workersandjobs <- alljobforecasts %>%
  #filter(forecast == "") %>%
  rename(value = total_jobs) %>%
  mutate(valuename = "jobs")


