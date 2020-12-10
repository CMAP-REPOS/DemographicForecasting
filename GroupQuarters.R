# CMAP | Mary Weber | 12/4/2020

#install.packages("tidycensus")
#install.packages("tidyverse")
library(tidycensus)
library(tidyverse)
library(dplyr)

#census_api_key("d94fbe16b1b053593223397765874bf147d1ae72", install = TRUE)

year <- 2010
counties <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
df_2010 <- load_variables(year, "sf1")
tibble(df_2010)

#returns GQ data for each group as outlined in model
tables <- c("PCO010","PCO009", "PCO008", "PCO006", "PCO005", "PCO004", "PCO003")
var_list <- vector()
for (i in 1:length(tables)){
  x <- grep(tables[i], df_2010$name)
  var_list <- c(var_list, x)
}
test <- df_2010[var_list,]

#cleanup
test$Category <- gsub(".*)!!","",test$label)
test$Category <- gsub("!!", " ", test$Category)

m <- tibble()
for (i in 1:length(names(counties))) {
a <- map_dfr(
      year,
    ~ get_decennial(geography = "county", variables = test$name, county = counties[[i]], state = names(counties[i]),
                    year = .x, survey = "sf1", cache_table = TRUE),
    .id = "year"
  )
  m = rbind(m, a)
}

GQ <- merge(m, test, by.x = "variable", by.y = "name")

#cleanup
GQ$Category[substr(GQ$Category, 1, 5) == "Total"] <- "County Total"
GQ$Category[GQ$Category == "Male"] <- "County Male Total"
GQ$Category[GQ$Category == "Female"] <- "County Female Total"
GQ$Year = 2010
GQ <- separate(data = GQ, col = NAME, into = c("County", "State"), sep = "\\,")
GQ <- subset(GQ, select = -c(label,GEOID, variable, year))

GQ$Region <- NA

CMAP <- c("Cook County", "DuPage County", "Kane County", "Kendall County", "Lake County", "McHenry County", "Will County")
OuterCounty <- c("Boone County", "DeKalb County", "Grundy County", "Kankakee County", "LaSalle County", "Lee County", "Ogle County", "Winnebago County")
GQ$Region[GQ$State == ' Wisconsin'] <- 'SE Wisconsin'
GQ$Region[GQ$State == ' Indiana'] <- 'NW Indiana'

GQ$Region[GQ$County %in% CMAP & GQ$State == " Illinois"] <- "CMAP"
GQ$Region[GQ$County %in% OuterCounty & GQ$State == " Illinois"] <- "IL Outer County"

#df for institutionalized only
GQ_inst <- filter(GQ, (GQ$concept == "GROUP QUARTERS POPULATION IN CORRECTIONAL FACILITIES FOR ADULTS BY SEX BY AGE") |
                    (GQ$concept == "GROUP QUARTERS POPULATION IN JUVENILE FACILITIES BY SEX BY AGE") |
                    (GQ$concept == "GROUP QUARTERS POPULATION IN OTHER INSTITUTIONAL FACILITIES BY SEX BY AGE"))

#df for non-institutionalized only
GQ_noninst <- filter(GQ, (GQ$concept == "GROUP QUARTERS POPULATION IN NURSING FACILITIES/SKILLED-NURSING FACILITIES BY SEX BY AGE") |
                       (GQ$concept == "GROUP QUARTERS POPULATION IN COLLEGE/UNIVERSITY STUDENT HOUSING BY SEX BY AGE") |
                       (GQ$concept == "GROUP QUARTERS POPULATION IN OTHER NONINSTITUTIONAL FACILITIES BY SEX BY AGE") |
                       (GQ$concept == "GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE"))


#save(GQ, GQ_inst, GQ_noninst, list= c("GQ", "GQ_inst", "GQ_noninst"), file="GQData.Rdata")
#load("~/Documents/GitHub/DemographicForecasting/GQData.Rdata")
#load("GQData.Rdata")
