


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


#graph of all 3 of the job forecasts (all and non-local) from employment.R
alljobforecasts <- bind_rows(upside_all, upside_noloc, baseline_all, baseline_noloc, slowgrowth_all, slowgrowth_noloc) %>%
  ungroup() %>% mutate(Year = as.numeric(Year))

o <- alljobforecasts %>% ungroup() %>%
  mutate(category = paste(forecast,type,sep="_")) %>%
  ggplot(aes(x=Year, y=total_jobs, group = category, color=forecast, shape = type)) +
  geom_point() + geom_line() + facet_wrap(~Region, scales = "free") +
  ggtitle("Number of Jobs, 2010-2050 \n   All Forecasts (upside, baseline, and slow growth) \n    and All Jobs and Non-Local Industry Jobs") +
  theme(legend.position = "bottom")


######## GRAPHS OF POPULATION FORECASTS

#~~~graph of total population

#~~~build graphs of total male and female population projections for each region and year
pop_proj <- export
pop_proj_totals <- pop_proj %>% group_by(Region, Sex, year) %>% summarize(total_population = sum(ProjectedPop_final))
p <- pop_proj_totals %>% ggplot(aes(x=year, y=total_population, color=Sex, group = Sex)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population, 2025-2050", subtitle = paste("Target Net Migration values: ", tNMfile))


#pull in data from 2010,2015,2020 and add to total population graphs for each region and year
pop2020 <- POP[['2020']] %>%
  mutate(GEOID = as.character(GEOID)) #extra step to fix the GEOID type and make sure the bind_rows below works
recentdata <- bind_rows(POP[['2010']], POP[['2015']], pop2020) %>%
  select(Year, Age, Region, Sex, Population) %>%
  mutate(type = "PEPestimate")
pop_proj <- export %>% rename(Population = ProjectedPop_final) %>% mutate(Year = as.numeric(year), type = "Projection") %>% select(-year)

pop_recandproj <- bind_rows(recentdata, pop_proj) ###population, recent and projected
pop_totals_check <- pop_recandproj %>% group_by(Region, Year, type) %>% summarize(totpop = sum(Population))
pop_totals_check

#Graphing total population by region (including Census)
pop_totals <- pop_recandproj %>% group_by(Region, Year, type) %>% summarize(totpop = sum(Population))
pp <- pop_totals %>% ggplot(aes(x=Year, y=totpop, group = Region, shape = type)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population, estimated and projected, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom")
pp

#graphing total population by sex and region
pop_totals <- pop_recandproj %>% group_by(Region, Sex, Year, type) %>% summarize(totpop = sum(Population))
q <- pop_totals %>% ggplot(aes(x=Year, y=totpop, color = Sex, group = Sex, shape = type)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population, estimated and projected, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom")
q

#build population pyramid  ####
pp1 <- export %>% filter(Region == "CMAP Region") %>%
  filter(year == 2025 | year == 2050) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>%
  ggplot(aes(x = x, fill = Sex,
             y = ifelse(test = Sex == "Male",
                        yes = -ProjectedPop_final, no = ProjectedPop_final))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(export$ProjectedPop_final) * c(-1,1)) +
  coord_flip() +
  facet_wrap(Region ~ year, ncol = 4, scales = "free")
pp1
pp2 <- export %>% filter(Region == "External IL") %>%
  filter(year == 2025 | year == 2050) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>%
  ggplot(aes(x = x, fill = Sex,
             y = ifelse(test = Sex == "Male",
                        yes = -ProjectedPop_final, no = ProjectedPop_final))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(export %>% filter( Region == "External IL") %>% select(ProjectedPop_final)) * c(-1,1)) +
  coord_flip() +
  facet_wrap(Region ~ year, ncol = 4, scales = "free")
pp2
pp3 <- export %>% filter(Region == "External IN") %>%
  filter(year == 2025 | year == 2050) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>%
  ggplot(aes(x = x, fill = Sex,
             y = ifelse(test = Sex == "Male",
                        yes = -ProjectedPop_final, no = ProjectedPop_final))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(export %>% filter( Region == "External IN") %>% select(ProjectedPop_final)) * c(-1,1)) +
  coord_flip() +
  facet_wrap(Region ~ year, ncol = 4, scales = "free")
pp3
pp4 <- export %>% filter(Region == "External WI") %>%
  filter(year == 2025 | year == 2050) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x) %>%
  ggplot(aes(x = x, fill = Sex,
             y = ifelse(test = Sex == "Male",
                        yes = -ProjectedPop_final, no = ProjectedPop_final))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(export %>% filter( Region == "External WI") %>% select(ProjectedPop_final)) * c(-1,1)) +
  coord_flip() +
  facet_wrap(Region ~ year, ncol = 4, scales = "free")
pp4

######## GRAPHS OF WORKERS AND JOBS

#plot the number of working-age people in the region
workingage <- export %>% mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  filter(x >= 20 & x < 65) %>% select(-x) %>% #filter for population aged 20-64
  group_by(Region, year, Sex) %>%
  summarize(workingage = sum(ProjectedPop_final)) %>%
  ungroup()
wk <- workingage %>% ggplot(aes(x=year, y=workingage)) + geom_col(aes(fill = Sex), width = 0.7) +
  facet_wrap(~Region, scales = "free")


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
r <- workersandjobs_graph %>%   #filter(Region == "CMAP Region") %>%
  ggplot(aes(x=Year, y= value, group = forecast, color = forecast)) +
  geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") +
  ggtitle("Projected Workers and Jobs, 2010-2050 (Baseline Economic Forecast)", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom", legend.direction = "vertical") + ylab("Number of People") + labs(tag = "1", color = NULL)
r

#calculate the # difference and % difference between the # of workers and jobs, graph percent difference as bar chart
workerjobdiff <- alljobforecasts %>%
  filter(forecast == "baseline" | type == "all_job_industries") %>% #choose which forecast  and industries you want to
  left_join(workers, by=c("Region", "Year")) %>%
  mutate(diff = total_jobs - workers) %>%
  mutate(percentdiff = (abs(diff)* 100 / ((total_jobs + workers) /2)))

s <- workerjobdiff %>% ggplot(aes(x=Year, y=percentdiff, fill=type.y)) + geom_col() + facet_wrap(~Region, scales="free")



############### GRAPHS OF COMPONENTS OF CHANGE

#add up all of the components of change and graph them together (1 graph per region)
summary_components <- components_all %>%
  group_by(Region, year, componentType) %>% summarize(summaryvalue = sum(componentValue))
t <- summary_components %>%  # filter(Region == "CMAP Region") %>%
  ggplot(aes(x=year, y=summaryvalue, group = componentType, color = componentType)) +
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
  ggtitle("Components of Population Change by Sex 2025-2050", subtitle = paste("Target Net Migration values: ", tNMfile))
u

#deaths age distribution
deaths_graphs <- components_all %>% filter(componentType == "Deaths") %>%
  mutate(category = paste(year, Sex, sep="_"))
w <- deaths_graphs %>%
  #filter(Region == "External WI") %>% #filter to focus on one region
  ggplot(aes(x=Age, y=componentValue, shape = Sex, color = year, group = category)) +
  geom_point() + geom_line() + facet_wrap(~ Region, scales = "free") +
  ggtitle("Age Distribution of Projected Deaths", subtitle = paste("Target Net Migration values: ", tNMfile))


#combine population with components (Alexis' note: I forgot why I wanted to do this..)
comp_pop <- export %>% rename(componentValue = ProjectedPop_final) %>%
  mutate(componentType = "PopulationProjection") %>%
  bind_rows(components_all)

#graph the target net migration (forecast periods only)
x <- target_NM %>% ggplot(aes(x=Year, y= NetMigration)) + geom_col() + facet_wrap(~Region, scales = "free") +
  ggtitle("Target Net Migration Values", subtitle = paste("Name of file containing the values: ", tNMfile))

#import past net migration values
pastNM <- read_excel("Input/past_NetMigration.xlsx") %>% rename(Year = PeriodEnd) %>% mutate(Year = as.character(Year))
allNM <- bind_rows(target_NM, pastNM) %>%
  mutate(Source = case_when(is.na(Source) ~ "Forecast",
                            TRUE ~ Source))
y <- allNM %>% ggplot(aes(x=Year, y= NetMigration, fill = Source)) + geom_col(width = 0.7) + facet_wrap(~Region, scales = "free") +
  ggtitle("Net Migration, past and forecast", subtitle = paste("Target Net Migration values: ", tNMfile))
y
z <- allNM %>% filter(Source != "Forecast") %>% ggplot(aes(x=Year, y= NetMigration, color = Region, group = Region)) + geom_point() + geom_line() + facet_wrap(~Region, scales = "free")


#graph Base Migration rates (data from 2013-14 and 2018-19, Migration.R)
a <- NETMIGPROJ[[1]] %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  ggplot(aes(x=reorder(Age,x), y = NMRs, color = Sex, group = Sex)) + geom_point() + geom_line() +
  facet_wrap(~Region) +
  ggtitle("Base Net Migration Rate", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
a

#see how total actual Migration stacks up to the target Net Migration
target_NM <- target_NM %>% mutate(category = "target")
netmig_stackup <- components_all %>%
  filter(componentType == "NetMigrants") %>%
  group_by(year, Region) %>%
  summarize(NetMigration = sum(componentValue)) %>%
  mutate(category = "forecast") %>%
  rename(Year = year) %>%
  bind_rows(target_NM)
b <- netmig_stackup %>% #filter(Region == "CMAP Region") %>%
  ggplot(aes(x=Year, y= NetMigration, group = category, color = category)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales = "free") +
  ggtitle("Target vs Calculated Net Migration", subtitle = paste("Name of file containing the values: ", tNMfile))


#Net Migration age distribution
netmig_graphs <- components_all %>% filter(componentType == "NetMigrants") %>%
  #filter(Region == "External WI") %>%
  mutate(category = paste(year, Sex, sep="_"))
v <- netmig_graphs %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  #filter(Region == "External WI") %>% #filter to focus on one region
  ggplot(aes(x=reorder(Age,x), y=componentValue, shape = Sex, color = year, group = category)) +
  geom_point() + geom_line() + facet_wrap(~ Region, scales = "free") +
  ggtitle("Age Distribution of Net Migration", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
v

#take a look at how the net migration rates change
c <- projectedNetMigrationrates %>%
  mutate(category = paste(year, Sex, sep="_")) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  ggplot(aes(x=reorder(Age,x), y=NMRs, shape = Sex, color = year, group = category)) +
  geom_point() + geom_line() + facet_wrap(Region ~ Sex,  ncol = 2, scales = "free") +
  ggtitle("Net Migration Rates by Age", subtitle = paste("Target Net Migration values: ", tNMfile)) #+
  #theme(axis.text.x = element_text(angle = 45, hjust=1))
c

#import the full Berger NMRs and compare them
