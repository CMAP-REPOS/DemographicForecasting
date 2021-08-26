# CMAP | Alexis McAdams, Mary Weber | 8/11/2021

         ###########  Projections control file info  ###############
#The concept: run a loop to cycle through the MigrationProjections script over
#and over to get population projections in 5 year increments up to 2050.
#For each projection period, the Population projection and Net Migration
#are saved in lists (POPPROJ and NETMIGPROJ, respectively) and are used as
#input for the subsequent projection period.
#When the loop is complete, the POPPROJ list is reformatted into a table
#called "Mig_Proj".


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

#set which target net migration values you'd like to use for the projection (see target_netmigration folder for options)
target_NM <- read_excel("target_netmigration/TNM_flataverages.xlsx") %>%
  mutate(Year = as.character(Year))
#name which net migration values you're using (important for documentation!)
tNMfile <- "recentNetMig_constantflatavg"


######## set up the population projection and migration projection lists
#create new file
POPPROJ <- list()
for(years in series){
  POPPROJ[[as.character(years)]] <- tibble()
}
NETMIGPROJ <- list()
for(years in series){
  NETMIGPROJ[[as.character(years)]] <- tibble()
}

#import in Base Net Migration data (includes allocation by Sex and by +/- 55 years old for first 2 allocation periods)
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

#-------
  i <- i+1
}


######## Final Step: unlist the population projections, last bits of formatting

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
Mig_Proj <- export %>%  # we should think about renaming this variable - it's not really a migration projection, it's a population projection with migration included
  mutate(TNMtype = tNMfile) #add column that documents WHICH SET of target net migrant values were used for this projection

#save(Mig_Proj, file="Output/Migration_Projections.Rdata")
#write.csv(Mig_Proj, "/Users/mweber/Desktop/Mig_Proj.csv")
