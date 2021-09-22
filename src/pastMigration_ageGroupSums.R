# CMAP | Alexis McAdams, Mary Weber | 9/20/2021
#
# This script imports previous Net Migration data and calculates
# total net migration for different age categories. This process
# directly informs the K-factor calculations in the MigrationProjections.R script.
#

#import Berger and Migration.R output
pastNetMig <- read_excel("Input/NetMigration_Berger_Full_sexandage.xlsx")     #importing Berger data
load("Output/Base_Migration.Rdata") # named Base_Mig

recentNetMig <- Base_Mig %>% select(Region, Age, Sex, SurvMigrants2018) %>%   #importing and reshaping CMAP data
  mutate(Period = "2014-2018",
         Source = "CMAP") %>%
  rename(NetMigration = SurvMigrants2018)

pastNetMig <- bind_rows(pastNetMig, recentNetMig) %>%
  mutate(endyear = substr(Period, 6,9))

#define age groups
agegroups <- list( c('0 to 4 years', '5 to 9 years', '10 to 14 years', '15 to 19 years', '20 to 24 years'),
                   c('25 to 29 years', '30 to 34 years', '35 to 39 years'),
                   c('40 to 44 years', '45 to 49 years', '50 to 54 years', '55 to 59 years', '60 to 64 years', '65 to 69 years'),
                   c('70 to 74 years', '75 to 79 years', '80 to 84 years', '85 years and over') )

names(agegroups) <- c("0 to 24 years", "25 to 39 years", "40 to 69 years", "70 years and older")

#assign the agegroupings, then calculate sum totals by age group and sex
i <- 1
for(item in agegroups){
  pastNetMig <- pastNetMig %>%
    mutate(agegroup = case_when(Age %in% item ~ names(agegroups[i]),
                                TRUE ~ agegroup) )
  i <- i+1
}

netMigSums <- pastNetMig %>% group_by(Region, Period, Sex, Source, agegroup) %>%
  summarize(NetMigration = sum(NetMigration)) %>%
  select(Period,Region,NetMigration, Sex, agegroup, Source) %>%
  mutate(NetMigration = round(NetMigration, 0)) %>%
  rename(Age = agegroup)

#export
save(netMigSums, file="Output/pastMigration_ageGroupSums.Rdata")




# -----------

#side item - plot out the net migration totals for each age group
library(ggplot2)
p <- pastNetMig %>% #filter(Region == "CMAP Region") %>%
  ggplot(aes(x=endyear, y= NetMigration, shape = Source, color = Age, group = Age)) +
  geom_point() + geom_line() +
  facet_wrap(Region~Sex, scales = "free", ncol = 2)
#p

q <- netMigTotals %>%
  ggplot(aes(x=Period, y= NetMigration, shape = Source, color = agegroup, group = agegroup)) +
  geom_point() + geom_line() +
  facet_wrap(Region~Sex, scales = "free", ncol = 2)
#q
