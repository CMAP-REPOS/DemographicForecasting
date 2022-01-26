##################

# CMAP | Alexis McAdams | 1/19/2022

###########  Final Control file info  ###############

# This script handles final formatting of the projection results,
# production of secondary forecast products (such as Household population
# and population by Race/Ethnicity), and export.


outputfolder <- "C:/Users/amcadams/Documents/R/testprojection"

# import forecast results

load(file="Output/PopProj.Rdata") # POPPROJ
load(file="Output/NMProj.Rdata") # NETMIGPROJ
load(file="Output/ComponentsOfChange.Rdata") # COMPONENTS
load(file = "Output/MigTesting.Rdata") # MIG_DETAIL
load(file = "Output/recordkeeping.Rdata") #SETTINGS

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

# run add'l scripts

#workforce
source("src/workforce.R")







projectedNetMigrationrates <- tibble()
i=1
for(item in NETMIGPROJ){
  print(item)
  temp <- item
  temp$year <- names(NETMIGPROJ)[i]
  projectedNetMigrationrates <- bind_rows(projectedNetMigrationrates, temp)
  i <- i + 1
}

components_all <- tibble()
i=1
for(item in COMPONENTS){
  temp <- item
  temp$year <- names(COMPONENTS)[i]
  components_all <- bind_rows(components_all, temp)
  i <- i + 1
}

# Mig_Proj is just POPPROJ delisted - need to find what scripts are dependent on this item (or "export")
Mig_Proj <- export %>% unique() %>% # we should think about renaming this variable - it's not really a migration projection, it's a population projection with migration included
  mutate(TNMtype = TNMnote) #add column that documents WHICH SET of target net migrant values were used for this projection

save(Mig_Proj, file="Output/Migration_Projections.Rdata")
