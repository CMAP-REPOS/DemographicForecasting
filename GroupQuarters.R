# CMAP | Mary Weber | 11/23/2020

#install.packages("tidycensus")
#install.packages("tidyverse")
library(tidycensus)
library(tidyverse)

#census_api_key("d94fbe16b1b053593223397765874bf147d1ae72", install = TRUE)

year <- 2010
states <- c("IL", "IN", "WI")
counties = list(IL=c(31, 43, 89, 93, 97, 111, 197, 7, 37, 63, 91, 99, 103, 141, 201), IN=c(89,91,127), WI=c(59, 101, 127)) 

#only load data if it isn't already; 2010 is only year of interest for GQ
if (tryCatch((exists('df_2010') && is.data.frame(get('df_2010')))) == "FALSE") {
  df_2010 <- load_variables(year, "sf1")
  tibble(df_2010)
} 

#returns GQ data for each group as outlined in model
tables <- c("PCO010","PCO009", "PCO008", "PCO006", "PCO005", "PCO004", "PCO003")
var_list <- vector()
for (i in 1:length(tables)){
  x <- grep(tables[i], df$name)
  var_list <- c(var_list, x)
} 
test <- df_2010[var_list,]

#cleanup of category name
test$Category <- gsub(".*)!!","",test$label)
test$Category <- gsub("!!", " ", test$Category)

#takes about 10 seconds to load all of the GQ data, ~13k records
m <- tibble()
for (i in 1:length(names(counties))) {
a <- map_dfr(
      year,
    ~ get_decennial(
      geography = "county",
      variables = test$name,
      county = counties[[i]],
      state = names(counties[i]),
      year = .x,
      survey = "sf1",
      cache_table = TRUE
    ),
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

#rm(list = ls())
