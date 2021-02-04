#pull in IN births

#IN population for 2010-most recent (David ER said to shorten base period)
#filter population to just female, then create a named list with the age groupings and the values 
#need to remove the GQ???
#add 10-14 births to 15-19, add 45 & over births to 40-44 OR reduce fertile female range to 15-4 - David ER



#ASFFR


#for each year in list (i to ?) where year starts at i and increases i+1 each time, calculate per each item in the
#named list (number of births in cohort / number of females in household population in that age group)

#graph to make sure things look like they're on track

#talk to David ER about projection


#filter(POP[["1990"]], State == 'Indiana')
F_Years <- c(2015) #add in 2010 later
F <- list()

for (YEAR in F_Years) {
temp <- POP[[as.character(YEAR)]] %>%
  filter(State == 'Indiana' & SEX == 'Female')
  #left join on certain columns since columns don't all match (GEOID, County, State, Value, AGEGROUP/Category, Sex, Year, Region)
}





