# CMAP | Mary Weber | 12/4/2020

#install.packages(c("tidyverse", "tidycensus"))
library(tidyverse)
library(tidycensus)
#census_api_key("d94fbe16b1b053593223397765874bf147d1ae72", install = TRUE)


# Set parameters ----------------------------------------------------------

YEAR <- 2010
COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)
CMAP_GEOIDS <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")
GQ_TABLES <- c("PCO010", "PCO009", "PCO008", "PCO006", "PCO005", "PCO004", "PCO003")


# Compile data ------------------------------------------------------------

# Compile list of variables to download
SF1_VARS <- load_variables(YEAR, "sf1")
# tibble(SF1_VARS)

#returns GQ data for each group as outlined in model
# var_list <- vector()
# for (i in 1:length(GQ_TABLES)) {
#   x <- grep(GQ_TABLES[i], SF1_VARS$name)
#   var_list <- c(var_list, x)
# }
# GQ_VARS <- SF1_VARS[var_list,]
#
# #cleanup
# GQ_VARS$Category <- gsub(".*)!!", "", GQ_VARS$label)
# GQ_VARS$Category <- gsub("!!", " ", GQ_VARS$Category)
GQ_VARS <- SF1_VARS %>%
  filter(str_starts(name, paste0("^", paste(GQ_TABLES, collapse="|^")))) %>%
  mutate(
    Category = str_replace_all(label, ".*\\)!!", ""),
    Category = str_replace_all(Category, "!!", " ")
  )

# Download data for selected variables in all counties
GQ_DATA <- tibble()
# for (i in 1:length(names(COUNTIES))) {
#   a <- map_dfr(
#     YEAR,
#     ~ get_decennial(geography = "county", variables = GQ_VARS$name,
#                     county = COUNTIES[[i]], state = names(COUNTIES[i]),
#                     year = .x, survey = "sf1", cache_table = TRUE),
#     .id = "year"
#   )
#   GQ_DATA <- rbind(GQ_DATA, a)
# }
for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = GQ_VARS$name,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = YEAR, survey = "sf1", cache_table = TRUE)
  GQ_DATA <- bind_rows(GQ_DATA, TEMP)
}

# GQ <- merge(GQ_DATA, GQ_VARS, by.x = "variable", by.y = "name")
#
# #cleanup
# GQ$Category[substr(GQ$Category, 1, 5) == "Total"] <- "County Total"
# GQ$Category[GQ$Category == "Male"] <- "County Male Total"
# GQ$Category[GQ$Category == "Female"] <- "County Female Total"
# GQ$Year = 2010
# GQ <- separate(data = GQ, col = NAME, into = c("County", "State"), sep = "\\,")
# GQ <- subset(GQ, select = -c(label, GEOID, variable, year))
#
# GQ$Region <- NA
# CMAP <- c("Cook County", "DuPage County", "Kane County", "Kendall County", "Lake County", "McHenry County", "Will County")
# OuterCounty <- c("Boone County", "DeKalb County", "Grundy County", "Kankakee County", "LaSalle County", "Lee County", "Ogle County", "Winnebago County")
# GQ$Region[GQ$State == ' Wisconsin'] <- 'SE Wisconsin'
# GQ$Region[GQ$State == ' Indiana'] <- 'NW Indiana'
# GQ$Region[GQ$County %in% CMAP & GQ$State == " Illinois"] <- "CMAP"
# GQ$Region[GQ$County %in% OuterCounty & GQ$State == " Illinois"] <- "IL Outer County"

# Assemble final table
GQ <- GQ_DATA %>%
  left_join(GQ_VARS, by = c("variable" = "name")) %>%
  select(-label) %>%
  rename(Concept = concept,
         Value = value,
         Variable = variable) %>%
  separate(NAME, c("County", "State"), sep = "\\, ") %>%
  mutate(
    Year = YEAR,
    Category = case_when(str_starts(Category, "^Total") ~ "County Total",
                         Category == "Male" ~ "County Male Total",
                         Category == "Female" ~ "County Female Total",
                         TRUE ~ Category),
    Region = case_when(GEOID %in% CMAP_GEOIDS ~ "CMAP Region",
                       State == "Illinois" ~ "External IL",
                       State == "Indiana" ~ "External IN",
                       State == "Wisconsin" ~ "External WI")
  )

# Split out institutionalized and non-institutionalized
GQ_INST <- GQ %>%
  filter(Concept %in% c(
    "GROUP QUARTERS POPULATION IN CORRECTIONAL FACILITIES FOR ADULTS BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN JUVENILE FACILITIES BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN OTHER INSTITUTIONAL FACILITIES BY SEX BY AGE"
  ))

GQ_NONINST <- GQ %>%
  filter(Concept %in% c(
    "GROUP QUARTERS POPULATION IN NURSING FACILITIES/SKILLED-NURSING FACILITIES BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN COLLEGE/UNIVERSITY STUDENT HOUSING BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN OTHER NONINSTITUTIONAL FACILITIES BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE"
  ))

#save(GQ, GQ_INST, GQ_NONINST, file="GQData.Rdata")
#load("GQData.Rdata")
