# CMAP | Alexis McAdams, Mary Weber | 8/11/2021


         ###########  Projections control file  ###############
#The idea: run a loop to cycle through the MigrationProjections script over
#and over to get population projections in 5 year increments up to 2050.
#Projection period output will be exported at end of each loop,
#and certain pieces will be used as input for the next cycle.


########set overarching variables
baseyear <- 2020
startyear <- 2020
endyear <- 2050

projnums <- (endyear - startyear) / 5 #number of 5-year projection cycles to complete
projnums

series <- seq(from=startyear,
              to=endyear,
              by= 5)
series

########set up where the projection files will go
#create new file
POPPROJ <- list()
for(years in series){
  POPPROJ[[as.character(years)]] <- tibble()
}
NETMIGPROJ <- list()
for(years in series){
  NETMIGPROJ[[as.character(years)]] <- tibble()
}


#input baseyear into POPPROJ - can omit if confusing
POPPROJ[[as.character(baseyear)]] <- POP[[as.character(baseyear)]] %>%
  group_by(Age, Region, Sex) %>% summarise(baseyrpop = sum(Population)) %>%
  ungroup()

#set new folder as working directory

#copy in base projection files that will be needed (Output/...Rdata)
 # population, NMRs


#######
#Do a one-time import of the historical Net Migration data ahead of running the
NetMig <- read_excel("Input/NetMigration_Berger.xlsx") %>% filter(!is.na(Period)) %>% arrange(Period, Region, Sex)

######## run the loop

i <- 1
while(i <= projnums){

#set up variables that MigrationProjections needs
projstart <- series[i]
projend <- series[i+1]
projmidpoint  <- (projstart + projend) / 2
projyears <- seq(from=projstart,
                 to=projend - 1)

print(paste("Creating forecast for the period",projstart, "to", projend, sep=" "))

#run the migration code
source("src/MigrationProjections.R")

#save the final population projection

POPPROJ[[as.character(projend)]] <- Projections
save(POPPROJ, file="Output/PopProj.Rdata")

#save the Net Migration rates
NETMIGPROJ[[as.character(projend)]] <- Migration
save(NETMIGPROJ, file="Output/NMProj.Rdata")


#save the Components of Change (optional)

#save the outputs (besides population) that will become the inputs for the next cycle
  #population, NMRs



#-------
  i <- i+1
}


######## Final Step

#export projections
export <- tibble()
i=1
for(item in POPPROJ){
  print(item)
  temp <- item
  temp$year <- names(POPPROJ)[i]
  export <- bind_rows(export, temp)
  i <- i + 1
}
write.csv(export, file = "C:/Users/amcadams/Documents/R/projections_export.csv")

