# CMAP | Mary Weber | 8/18/2021

# Parameters ---------------------------------------------------------

startyear = as.character(projstart)  #"2020"

# if/else statement decides if population is pulled from POP or MIG_PROJ and formats data accordingly
if(startyear <= projectionstart) {

  basepop <- POP[[startyear]] %>%
    group_by(Sex, Age, Region) %>%
    summarize(Population = sum(Population)) %>%
    ungroup()
  print(paste("Pulling past data:", startyear, sep=" "))

}else{

  basepop <- Mig_Proj %>%
    filter(year == startyear) %>%
    rename(Population = ProjectedPop_final) %>%
    select(Sex, Age, Region, Population) %>%
    ungroup()
  print(paste("Pulling projected data:", startyear, sep=" "))

}

# Joins GQ ratios to Population, calculates GQ totals
GQ_Pop <- full_join(basepop, GQratios, by=c("Sex", "Age", "Region")) %>%
  mutate(across(starts_with("GQ"), ~.*Population)) %>% #multiply every GQ column by Population
  left_join(GQ_Military, by=c("Region", "Sex", "Age")) %>% rename(GQ_NonInst_Military = Value) %>% #join the Military values
  rowwise() %>%
  mutate(Inst_GQ = round(sum(across(starts_with("GQ_Inst"))), 0),
         nonInst_GQ = round(sum(across(starts_with("GQ_NonInst"))),0) ) %>%
  mutate(totalGQ = sum(across(ends_with("GQ"))))

# Subtract GQ from Population, pivot table wider, join Headship ratio, calculate Heads of Household (aka Households)
HouseholdPop <- GQ_Pop %>% select(Age, Sex, Region, Population, totalGQ) %>%
  mutate(HH_Pop = Population - totalGQ) %>%
  select(-totalGQ) %>%
  pivot_wider(names_from = "Sex", values_from=c("Population", "HH_Pop")) %>%
  left_join(Headship, by=c("Age", "Region")) %>%
  mutate(Head_HH = round((HH_Pop_Male*Ratio_Adj)+(HH_Pop_Female*Ratio_Adj),0))

print(paste("Year", startyear, "households and GQ calculations complete!", sep=" "))


