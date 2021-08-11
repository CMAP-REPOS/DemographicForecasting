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

########set up projection location
#create new folder (on GitHub)

#set new folder as working directory

#copy in base projection files that will be needed (Output/...Rdata)
 # population, NMRs


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
#source("src/MigrationProjections.R")

#save the final population projection

#save the Components of Change (optional)

#save the outputs that will become the inputs for the next cycle
  #population, NMRs


#-------
  i <- i+1
}


######## Final Step: combine all of the population projections into one table










#### scrap pile below

projlist <- vector("list",length(series))
names(projlist) <- paste(series, "projection")

for (year in series) {
  projlist[[year]]
}

