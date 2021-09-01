# CMAP | Alexis McAdams, Mary Weber | 8/18/2021

load("Output/GQData.Rdata")


baseyr2 <- 2019
startyr2 <- 2019
endyear <- 2050

projnums <- 7

series <- c(2019, 2025, 2030, 2035, 2040, 2045, 2050, 2060) #code to use when 2020 PEP available

HH_PROJ <- list()
for(years in series){
  HH_PROJ[[as.character(years)]] <- tibble()
}


i <- 1
while(i <= projnums){

  projstart <- series[i]
  projend <- series[i+1]
  projyears <- seq(from=projstart,
                   to=projend-1)

  print(paste("Creating forecast for the period",projstart, "to", projend, sep=" "))

  source("src/Household_Totals.R")

  HH_PROJ[[as.character(projstart)]] <- Head_of_HH

  save(HH_PROJ, file="Output/HH_Proj.Rdata")

  i <- i+1
}




export2 <- tibble()
i=1
for(item in HH_PROJ){
  print(item)
  temp2 <- item
  temp2$Year <- names(HH_PROJ)[i]
  export2 <- bind_rows(export2, temp2)
  i <- i + 1
}

View(export2)
write.csv(export2, "/Users/mweber/Desktop/export2.csv")
