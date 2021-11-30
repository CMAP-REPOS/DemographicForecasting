

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
alljobforecasts <- bind_rows( baseline_all, baseline_noloc) %>%
  ungroup() %>% mutate(Year = as.numeric(Year))

#alljobforecasts <- bind_rows(upside_all, upside_noloc, baseline_all, baseline_noloc, slowgrowth_all, slowgrowth_noloc) %>%
#  ungroup() %>% mutate(Year = as.numeric(Year))

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

#Graphing total population by region (EXcluding Census)
pop_totals2 <- pop_recandproj %>% group_by(Region, Year, type) %>% summarize(totpop = sum(Population)) %>% filter(Year > 2020)
ppnc <- pop_totals2 %>% ggplot(aes(x=Year, y=totpop, group = Region, shape = type)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population, estimated and projected, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom") + theme_cmap()



#graphing total population by sex and region
pop_totals <- pop_recandproj %>% group_by(Region, Sex, Year, type) %>% summarize(totpop = sum(Population)) %>% ungroup()
q <- pop_totals %>% ggplot(aes(x=Year, y=totpop, color = Sex, group = Sex, shape = type)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population, estimated and projected, 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom")

q2 <- pop_totals %>% filter(Year <= 2020) %>% ggplot(aes(x=Year, y=totpop, color = Sex, group = Sex, shape = type)) + geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") + ggtitle("Total Population by Sex, CMAP and External Regions", subtitle = "Census: 2010, 2025, 2020 (est)") +
  theme(legend.position = "bottom")


q3 <- POPrecent[['2020']] %>% filter(Region == "CMAP Region") %>%
  mutate(Age_Group = str_split_fixed(Age, " years", 2)[,1] ) %>%
  mutate(Age_Group = case_when(Age_Group == "85" ~ "85+",
                               TRUE ~ Age_Group)) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>% arrange(x)

q4 <- q3 %>%
  mutate(Age_Group = ordered(Age_Group)) %>%
  ggplot(aes(x=reorder(Age_Group,x), y=Population, color=Sex, group = Sex)) + geom_point() + geom_line() +
  ggtitle("Total Population by Sex and Age Group, CMAP Region", subtitle = "2020 Census, Redistricting Data") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


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



'
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
'
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
                              TRUE ~ forecast)) %>%
  ungroup() %>%
  mutate(valuetype = case_when(Year <= 2020 ~ "Known Data",
                           TRUE ~ "Forecasted Data") )
r <- workersandjobs_graph %>%   filter(Region == "CMAP Region") %>%
  filter(forecast != "baseline_jobs_minus_local_industries") %>%
  ggplot(aes(x=Year, y= value, shape = valuetype, group = forecast, color = forecast )) +
  geom_point() + geom_line() +
  facet_wrap(~Region, scales="free") +
  ggtitle("Projected Workers and Jobs, 2010-2050", subtitle = paste("Add'l info: ", tNMfile)) +
  theme(legend.position = "bottom", legend.direction = "vertical") + ylab("Number of People") + labs(tag = "1", color = NULL)


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
  group_by(Region, year, componentType) %>% summarize(summaryvalue = sum(componentValue)) %>%
  filter(componentType != "NetMigrants") %>%
  ungroup()
t <- summary_components %>%  filter(Region == "CMAP Region") %>%
  ggplot(aes(x=year, y=summaryvalue, group = componentType, color = componentType)) +
  #geom_point() +
  geom_line() +
  #facet_wrap( ~ Region, scales = "free") +
  ggtitle("Components of Population Change 2025-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  scale_color_brewer(palette = "Set1") + scale_y_continuous(limits = c(0,600000), labels = scales::comma) + theme_cmap()
finalize_plot(t,
              title = "CMAP Region, Projected Components of Change",
              caption = "Source: CMAP Demographic Model (2025-2050)")


#same as above, but break out each component by Sex
summary_components_bysex <- components_all %>%
  group_by(Region, year, Sex, componentType) %>% summarize(summaryvalue = sum(componentValue)) %>%
  mutate(category = paste(componentType,Sex,sep="_"))
u <- summary_components_bysex %>% ggplot(aes(x=year, y=summaryvalue,  color = componentType, shape = Sex, group = category)) +
  geom_point() + geom_line() +
  facet_wrap( ~ Region, scales = "free") +
  ggtitle("Components of Population Change by Sex 2025-2050", subtitle = paste("Target Net Migration values: ", tNMfile))


#deaths age distribution
deaths_graphs <- components_all %>% filter(componentType == "Deaths") %>%
  mutate(category = paste(year, Sex, sep="_"))
w <- deaths_graphs %>%
  filter(Region == "CMAP Region") %>% #filter to focus on one region
  ggplot(aes(x=Age, y=componentValue, shape = Sex, color = year, group = category)) +
  geom_point() + geom_line() +
  #facet_wrap(~ Region, scales = "free") +
  ggtitle("Age Distribution of Projected Deaths", subtitle = paste("Target Net Migration values: ", tNMfile))
w

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

z <- allNM %>% filter(Source != "Forecast") %>% ggplot(aes(x=Year, y= NetMigration, color = Region, group = Region)) + geom_point() + geom_line() + facet_wrap(~Region, scales = "free")


#graph Base Migration rates (data from 2013-14 and 2018-19, Migration.R)
a <- NETMIGPROJ[[1]] %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  ggplot(aes(x=reorder(Age,x), y = NMRs, color = Sex, group = Sex)) + geom_point() + geom_line() +
  facet_wrap(~Region) +
  ggtitle("Base Net Migration Rate", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


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


#take a look at how the net migration rates change
c <- projectedNetMigrationrates %>%
  mutate(category = paste(year, Sex, sep="_")) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  ggplot(aes(x=reorder(Age,x), y=NMRs, shape = Sex, color = year, group = category)) +
  geom_point() + geom_line() + facet_wrap(Region ~ Sex,  ncol = 2, scales = "free") +
  ggtitle("Net Migration Rates by Age", subtitle = paste("Target Net Migration values: ", tNMfile)) #+
  #theme(axis.text.x = element_text(angle = 45, hjust=1))
c

#import the full Berger pop totals and compare them

bergerpop <- read_excel("Input/berger_population.xlsx") %>%
  mutate(Source = "Berger")

pop_totals <- pop_recandproj %>% group_by(Region, Year, type) %>% summarize(totpop = sum(Population))
both_total <- bergerpop %>% group_by(Region, Year) %>% summarize(totpop = sum(Population)) %>%
  mutate(Source = "Berger's Model") %>%
  bind_rows(pop_totals %>% select(-type) %>% mutate(Source = "CMAP Model"))
ppp <- both_total %>%
  filter(Region == "CMAP Region") %>%
  ggplot(aes(x=Year, y=totpop, group = Source, color = Source, shape = Source)) +
  geom_line() +
  #facet_wrap(~ Region, ncol = 4, scales="free") +
  ggtitle("Total Population, CMAP Region, estimated and projected") +
  theme_cmap()
finalize_plot(ppp)



bothtotal2 <- both_total %>% group_by(Region, Year, Source) %>% summarize(totpop = sum(totpop))
p4 <- bothtotal2 %>% ggplot(aes(x=Year, y=totpop, group = Source, color = Source, shape = Source)) + geom_point() + geom_line() +
  facet_wrap( ~ Region, ncol = 2, scales="free") + ggtitle("Total Population, estimated and projected, CMAP vs Berger 2010-2050", subtitle = paste("Target Net Migration values: ", tNMfile)) +
  theme(legend.position = "bottom")


both_all <- pop_recandproj %>% group_by(Region, Year, Age, type) %>% summarize(Population = round(sum(Population),0)) %>%
  mutate(Source = "CMAP") %>% select(-type) %>%
  bind_rows(bergerpop %>% group_by(Region, Age, Year) %>% summarize(Population = round(sum(Population),0)) %>% mutate(Source="Berger") ) %>%
  mutate(age2 = as.numeric(substr(Age, 1, 2))) %>%
  mutate(age2 = case_when(is.na(age2) ~ 5,
                       TRUE ~ age2))
#Graph our projection vs Berger's projections, only on the tens (2020,2030,etc)
pops <- both_all %>%
  filter(Region == "CMAP Region") %>%
  filter(Year %% 10 == 0) %>%
  ggplot(aes(x=age2, y=Population, group = Source, color = Source)) + geom_point() + geom_line() +
  facet_wrap(Region ~ Year, ncol = 5,
             #scales = "free")
  ) +   ggtitle("Berger vs CMAP Popuation Forecast", subtitle = paste("In use:", tNMfile))

#similar to above, but just graphing each model result on one chart
pops2 <- both_all %>%
  filter(Region == "CMAP Region") %>%
  #filter(Source == "CMAP") %>%
  #filter(Year %% 10 == 0) %>%
  filter(Year == 2050 | Year == 2010) %>%
  ggplot(aes(x=age2, y=Population, group = as.character(Year), color = as.character(Year))) + geom_point() + geom_line() +
  facet_grid(~ Source) + scale_y_continuous(labels = scales::comma) + xlab("Age") +
  ggtitle("CMAP Region Total Popuation Forecast")
pops2

#calculate age dependency ratio = (0-19 + >65) / (20-64) for each projection and year
# Census predicts ~ 85 by 2050: https://www.census.gov/prod/2010pubs/p25-1138.pdf
agedepratio <- both_all %>%
  mutate(grouping = case_when(age2 <= 15 | age2 >= 65 ~ "Dependent",
                              TRUE ~ "Productive")) %>%
  group_by(Region, Year, Source, grouping) %>%
  summarize(total = sum(Population)) %>%
  pivot_wider(names_from = grouping, values_from = total) %>%
  rowwise() %>%
  mutate(dependency_ratio = (Dependent / Productive) * 100)
depratio <- agedepratio %>%
  ggplot(aes(x=Year, y=dependency_ratio, group = Source, color = Source)) + geom_point() + geom_line() +
  facet_wrap(~ Region, scales = "free")
#depratio

#summary calcs
temp <- both_all %>% filter(Region == "CMAP Region") %>% group_by(Year, Source, Region) %>% summarize(totpop = sum(Population))

temp2 <- both_all %>%
  mutate(grouping = case_when(age2 <= 10 ~ "0_to_14",
                              age2 > 10 & age2 < 65 ~ "15 to 64",
                              TRUE ~ "65_plus") ) %>%
  group_by(Year, Source, grouping) %>%
  summarize(totpop = sum(Population)) %>%
  pivot_wider(names_from = Source, values_from = totpop)
write.csv(temp2, file = "C:/Users/amcadams/Documents/R/popgroupings.csv")

temp3 <- components_all %>% filter(componentType == "Births") %>% group_by(Region, year) %>% summarize(Births = sum(componentValue))
temp0 <- both_all %>% filter(Region == "CMAP Region") %>% filter(Age == "0 to 4 years")
