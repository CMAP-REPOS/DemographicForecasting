# CMAP | Mary Weber | 12/4/2020

#install.packages(c("tidyverse", "tidycensus"))
library(tidyverse)
library(tidycensus)
#census_api_key("d94fbe16b1b053593223397765874bf147d1ae72", install = TRUE)

#16 to 64 and a 65+ category

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
GQ_VARS <- SF1_VARS %>%
  filter(str_starts(name, paste0("^", paste(GQ_TABLES, collapse="|^")))) %>%
  mutate(
    Category = str_replace_all(label, ".*\\)!!", ""),
    Category = str_replace_all(Category, "!!", " ")
  )

# Download data for selected variables in all counties
GQ_DATA <- tibble()
for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = GQ_VARS$name,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = YEAR, survey = "sf1", cache_table = TRUE)
  GQ_DATA <- bind_rows(GQ_DATA, TEMP)
}

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


# Adds columns for Sex and Age, but retains Category column
GQ <- GQ %>%
  separate(Category, into=c("Sex", "Age"), sep = " ", extra = "merge", remove = FALSE) %>%
  mutate(
    Sex = case_when(Sex == "County" & str_starts(Category, "County Male") ~ "Male",
                    Sex == "County" & str_starts(Category, "County Female") ~ "Female",
                    Sex =="County" ~ "All",
                    TRUE ~ Sex))


# Split out institutionalized and non-institutionalized
GQ_INST <- GQ %>%
  filter(Concept %in% c(
    "GROUP QUARTERS POPULATION IN CORRECTIONAL FACILITIES FOR ADULTS BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN JUVENILE FACILITIES BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN OTHER INSTITUTIONAL FACILITIES BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN NURSING FACILITIES/SKILLED-NURSING FACILITIES BY SEX BY AGE"
  ))

GQ_NONINST <- GQ %>%
  filter(Concept %in% c(
    "GROUP QUARTERS POPULATION IN COLLEGE/UNIVERSITY STUDENT HOUSING BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN OTHER NONINSTITUTIONAL FACILITIES BY SEX BY AGE",
    "GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE" #hold constant in projections
  ))

#save(GQ, GQ_INST, GQ_NONINST, file="Output/GQData.Rdata") #add GQE, too
#load("Output/GQData.Rdata")

