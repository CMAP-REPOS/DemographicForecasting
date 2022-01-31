##################

# CMAP | Alexis McAdams | 1/19/2022

###########  Final Control file info  ###############

# This script handles final formatting of the projection results,
# production of secondary forecast products (such as Household population
# and population by Race/Ethnicity), and export.

outputfolder <- "C:/Users/amcadams/Documents/R/testprojection_extIL"

# import forecast results

load("Output/PopProj.Rdata") # POPPROJ
load("Output/NMProj.Rdata") # NETMIGPROJ
load("Output/ComponentsOfChange.Rdata") # COMPONENTS
#load(file = "Output/MigTesting.Rdata") # MIG_DETAIL
load("Output/recordkeeping.Rdata") #SETTINGS

######### set options -----------------

#### headship method choice
    # this option changes the method of the headship (# of households) calculation in HH_Control.R
# if 1: model will use Approach A: 2010 Headship ratios by age and sex from Census bureau
# if 0: model will use Approach B: estimated 2019 Headship ratios by age estimated from PUMS data
#     WARNING: something is strange with the External IL 2019 Headship rates. Don't use until fixed!

HeadshipSource = 1

#### best available data option
# NOT YET IMPLEMENTED
    # this option will splice in known data for the years 2015 and 2020 when possible.
    # example: Group Quarters data by county is available for 2020, though not at age/sex
# if 1: model will fetch known data from the Census and work it into the appropriate summary tables.
# if 0: model will NOT grab known data and will only work with data and rates from the 2010 Census.

recentData = 1

######### additional setup ------------

# run add'l scripts

# workforce
source("src/workforce.R") # result: "workers"

# population by Race/Ethnicity
source("src/race_ethnicity_projection.R") # result: "raceeth_proj"

# Households and GQ
source("src/HH_Control.R")

# Households by Income quartile
source("src/income.R") # result: HH_incomes



################ formatting model results -----------

#unlist projection results (formerly called export)
results <- tibble()
i=1
for(item in POPPROJ){
  #print(item)
  temp <- item
  temp$year <- names(POPPROJ)[i]
  results <- bind_rows(results, temp)
  i <- i + 1
}

# unlist projection components
components_tbl <- tibble()
i=1
for(item in COMPONENTS){
  #print(item)
  temp <- item
  temp$year <- names(COMPONENTS)[i]
  components_tbl <- bind_rows(components_tbl, temp)
  i <- i + 1
}

# small projection summary
pop_summary <- results %>%
  group_by(year, Region) %>%
  summarize(population = sum(ProjectedPop_final), .groups ="drop")

#combine all race/eth population data into one table

load("Output/totalPop_RE_projection.Rdata") #raceeth_proj
race_eth <- raceeth_proj %>%
  rename(TotalPop = calcPop) %>%
  mutate(variable = case_when(HISP == "Hispanic" ~ "Hispanic",
                              RACE == "Asian alone" ~ "NH_Asian",
                              RACE == "Black alone" ~ "NH_Black",
                              RACE == "White alone" ~ "NH_White",
                              TRUE ~ "NH_Other"), .keep = "unused") %>%
  full_join(GQRE_pop, by = c("Region", "Year", "variable")) %>%
  rename(TotalGQPop = GQRE_pop_proj) %>%
  full_join(HHRE_pop, by = c("Region", "Year", "variable")) %>%
  rename(TotalHHPop = HHRE_pop_proj) %>%
  relocate(variable, .after = Year)

################ export files -----------

setwd(outputfolder)

###### General Summary Data
dir.create("1_summary")
setwd("1_summary")

write.csv(pop_summary, "totalpop_summary.csv")
write.csv(Households_summary, "Households_summary.csv")
write.csv(GQ_summarybyAge, "GQ_summarybyAge.csv")
write.csv(GQ_summary, "GQ_summary.csv")
write.csv(race_eth, "pop_by_race_ethnicity.csv")


###### Detailed Data
setwd(outputfolder)
dir.create("2_detail")
setwd("2_detail")

write.csv(results, "export_popproj.csv")
write.csv(components_tbl, "componentsofchange.csv")
write.csv(HeadofHH_by_Age, "HeadofHH_by_Age.csv")
write.csv(GQ_detailed, "GQ_detailed.csv")
write.csv(HouseholdPop_byAge, "HouseholdPop_byAge.csv")

#write.csv(raceeth_proj, "totalPop_raceandethnicity.csv")
#write.csv(GQRE_pop, "GQPop_byRaceEthnicity.csv")
#write.csv(HHRE_pop, "HHPop_byRaceEthnicity.csv")

###### Specialty Formatting (Travel Model)
setwd(outputfolder)
dir.create("3_travelmodel")
setwd("3_travelmodel")

write.csv(travelModelHHpop, "travelModelHHpop.csv")
write.csv(travelModelHeadsofHHs, "travelModelHeadsofHHs.csv")
#write.csv(travelModel_GQNonInst_byAge, "travelModel_GQNonInst_byAge.csv") #check if needed?
write.csv(travelModel_GQNIOther_byAge, "travelModel_GQNIOther_byAge.csv")
write.csv(travelModel_collegemil, "travelModel_collegemil.csv")

###### Specialty Formatting (Other)
setwd(outputfolder)
dir.create("4_otheroutput")
setwd("4_otheroutput")

write.csv(HouseholdSize, "HouseholdSize.csv")
write.csv(workers, "workers.csv")
write.csv(HH_incomes, "Households_IncomeQuantiles.csv")

#setwd("~/GitHub/DemographicForecasting")
