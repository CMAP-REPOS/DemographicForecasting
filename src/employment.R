#Employment data

#this script reads in the baseline employment data from the consultant report and formats it for graphing

library(dplyr)
library(tidyverse)
library(readxl)

localindustries <- c("44-45", "71", "72", "81") #local industries that do not drive in-migration

baseline <- read_excel("C:/Users/amcadams/OneDrive - Chicago Metropolitan Agency for Planning/Documents/Demographic Model Project/employment/baseline_detailed.xlsx",
                       na = "0.0") %>%
  mutate(locsplit = case_when(industry_code %in% localindustries ~ "local",
                              TRUE ~ "non-local"))

#borrow the fips-to-county-and-region key from the PEP2020 input data
pepdata <- read_excel("Input/PEP2020.xlsx") %>%
  select(1:3, 8) %>%
  unique() %>%
  mutate(GEOID = as.character(GEOID))

baseline <- baseline %>%
  left_join(pepdata, by = c("area_fips" = "GEOID"))

baseline2 <- baseline %>%
  pivot_longer(starts_with("Emp"), names_to = "Year", values_to = "Employment") %>%
  #filter(locsplit == "non-local") %>% ###### HERE is where to decide if local industries is included or not
  group_by(Region, Year) %>%
  summarize(totemp = sum(Employment)) %>%
  mutate(Year = str_sub(Year, 5,8)) %>%
  mutate(totemp2 = totemp * 1.049)

#let's plot to see what it looks like
p <- baseline2 %>% ggplot(aes(x=Year, y = totemp2, group = Region)) + geom_point() + geom_line() + facet_wrap(~Region, scales = "free") + ggtitle("Number of Jobs, 2010-2050 \n Local industries excluded")
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
