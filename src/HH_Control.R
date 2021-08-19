


baseyr2 <- 2019
startyr2 <- 2019
endyear <- 2050
series <- c(2019, 2025, 2030, 2035, 2040, 2045, 2050)

#projnums <- (endyear - startyr2) / 5 #code to use when 2020 PEP available
projnums <- 6

#series <- seq(from=startyear,
#              to=endyear,
#              by= 5)
#series

HH_PROJ <- list()
for(years in series){
  HH_PROJ[[as.character(years)]] <- tibble()
}


i <- 1
while(i <= projnums){

  #set up variables that MigrationProjections needs
  projstart <- series[i]
  projend <- series[i+1]
  projyears <- seq(from=projstart,
                   to=projend - 1)

  print(paste("Creating forecast for the period",projstart, "to", projend, sep=" "))

  source("src/Household_Totals.R")

  HH_PROJ[[as.character(projend)]] <- Headship_Ratios

  save(HH_PROJ, file="Output/HH_Proj.Rdata")

  i <- i+1
}




