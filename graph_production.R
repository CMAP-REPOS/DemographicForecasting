


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

n <- bind_rows(baseline_all, baseline_noloc) %>%
  ggplot(aes(x=Year, y = total_jobs, color = type, group = type)) +
  geom_point() + geom_line() + facet_wrap(~Region, scales = "free") +
  ggtitle("Number of Jobs, 2010-2050 \n Baseline Forecast") +
  theme(legend.position = "bottom")
n

#graph of all 3 of the job forecasts (all and non-local) from employment.R
alljobforecasts <- bind_rows(upside_all, upside_noloc, baseline_all, baseline_noloc, slowgrowth_all, slowgrowth_noloc) %>%
  ungroup() %>% mutate(Year = as.numeric(Year))

o <- alljobforecasts %>% ungroup() %>%
  mutate(category = paste(forecast,type,sep="_")) %>%
  ggplot(aes(x=Year, y=total_jobs, group = category, color=forecast, shape = type)) +
  geom_point() + geom_line() + facet_wrap(~Region, scales = "free") +
  ggtitle("Number of Jobs, 2010-2050 \n   All Forecasts (upside, baseline, and slow growth) \n    and All Jobs and Non-Local Industry Jobs") +
  theme(legend.position = "bottom")
o


######## GRAPHS OF POPULATION FORECASTS

#~~~build graphs of total male and female population projections for each region and year
pop_proj <- export
pop_proj_totals <- pop_proj %>% group_by(Region, Sex, year) %>% summarize(total_population = sum(ProjectedPop_final))
p <- pop_proj_totals %>% ggplot(aes(x=year, y=total_population, color=Sex, group = Sex)) + geom_point() + geom_line() +
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

#build population pyramid  ####**** STILL NEED TO FIX AGE GROUP SORT (factors!)
pp <- export %>% filter(Region == "CMAP Region") %>%
  #filter(year == 2025) %>%
  ggplot(aes(x = Age, fill = Sex,
             y = ifelse(test = Sex == "Male",
                        yes = -ProjectedPop_final, no = ProjectedPop_final))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(export$ProjectedPop_final) * c(-1,1)) +
  coord_flip() +
  facet_wrap(year ~ Region)
pp



######## GRAPHS OF WORKERS AND JOBS

#plot the number of workers
#p <- workers %>% ggplot(aes(x=Year, y=workers, group = Region, shape = type)) + geom_point() + geom_line() +
#  facet_wrap(~Region, scales="free") + ggtitle("Number of Workers, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile))
#p

#plot the number of workers WITH the jobs forecasts
tempworkers <- workers %>%
  rename(value = workers) %>%
  mutate(valuename = "workers")
workersandjobs <- alljobforecasts %>% #need "alljobforecasts" from above!
  filter(forecast == "baseline") %>% #choose which forecast you want to graph with the workers
  rename(value = total_jobs) %>%
  mutate(valuename = "jobs") %>%
  bind_rows(tempworkers)

workersandjobs_graph <- workersandjobs %>%
  mutate(forecast = case_when(is.na(forecast) ~ "workers",
                              TRUE ~ forecast)) %>%
  mutate(forecast = case_when(valuename == "jobs" ~ paste(forecast,type,sep="_"),
                              TRUE ~ forecast))
r <- workersandjobs_graph %>% ggplot(aes(x=Year, y= value, group = forecast, color = forecast)) +
  geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") +
  ggtitle("Projected Workers and Jobs, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom")
r

#calculate the # difference and % difference between the # of workers and jobs, graph percent difference as bar chart
workerjobdiff <- alljobforecasts %>%
  filter(forecast == "baseline" | type == "all_job_industries") %>% #choose which forecast  and industries you want to
  left_join(workers, by=c("Region", "Year")) %>%
  mutate(diff = total_jobs - workers) %>%
  mutate(percentdiff = (abs(diff)* 100 / ((total_jobs + workers) /2)))

s <- workerjobdiff %>% ggplot(aes(x=Year, y=percentdiff, fill=type.y)) + geom_col() + facet_wrap(~Region, scales="free")
s


############### GRAPHS OF COMPONENTS OF CHANGE
#add up all of the components of change and graph them together (1 graph per region)
summary_components <- components_all %>%
  group_by(Region, year, componentType) %>% summarize(summaryvalue = sum(componentValue))
t <- summary_components %>% ggplot(aes(x=year, y=summaryvalue, group = componentType, color = componentType)) +
  geom_point() + geom_line() +
  facet_wrap( ~ Region, scales = "free") +
  ggtitle("Components of Population Change 2025-2050", subtitle = paste("Target Net Migration values: ", tNMfile))
t

#same as above, but break out each component by Sex
summary_components_bysex <- components_all %>%
  group_by(Region, year, Sex, componentType) %>% summarize(summaryvalue = sum(componentValue)) %>%
  mutate(category = paste(componentType,Sex,sep="_"))
u <- summary_components_bysex %>% ggplot(aes(x=year, y=summaryvalue,  color = componentType, shape = Sex, group = category)) +
  geom_point() + geom_line() +
  facet_wrap( ~ Region, scales = "free") +
  ggtitle("Components of Population Change 2025-2050", subtitle = paste("Target Net Migration values: ", tNMfile))
u

#Net Migration age distribution
netmig_graphs <- components_all %>% filter(componentType == "NetMigrants") %>%
  mutate(category = paste(year, Sex, sep="_"))
v <- netmig_graphs %>%
  filter(Region == "External WI") %>% #filter to focus on one region
  ggplot(aes(x=Age, y=componentValue, shape = Sex, color = year, group = category)) +
  geom_point() + geom_line() + facet_wrap(~ Region, scales = "free") +
  ggtitle("Age Distribution of Net Migration", subtitle = paste("Target Net Migration values: ", tNMfile))
v

#deaths age distribution
deaths_graphs <- components_all %>% filter(componentType == "Deaths") %>%
  mutate(category = paste(year, Sex, sep="_"))
w <- deaths_graphs %>%
  #filter(Region == "External WI") %>% #filter to focus on one region
  ggplot(aes(x=Age, y=componentValue, shape = Sex, color = year, group = category)) +
  geom_point() + geom_line() + facet_wrap(~ Region, scales = "free") +
  ggtitle("Age Distribution of Projected Deaths", subtitle = paste("Target Net Migration values: ", tNMfile))
w

#combine population with components
comp_pop <- export %>% rename(componentValue = ProjectedPop_final) %>%
  mutate(componentType = "PopulationProjection") %>%
  bind_rows(components_all)



