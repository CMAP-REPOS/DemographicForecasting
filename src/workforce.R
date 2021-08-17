# Labor Force calculation
#

#get GQ ratios
load("Output/GQData.Rdata")

#define "too young to work" age groups (assumption)
kiddos <- c("0 to 4 years", "5 to 9 years", "10 to 14 years")

#import Labor Force Participation Rate projections
LFPRs <- read.csv("C:/Users/amcadams/OneDrive - Chicago Metropolitan Agency for Planning/Socioeconomic Forecasting/Data/Labor Force/LFPRs.csv")

#import unemployment rates (historical and projections)
unemp <- read.csv("C:/Users/amcadams/OneDrive - Chicago Metropolitan Agency for Planning/Socioeconomic Forecasting/Data/Labor Force/unemploymentrates.csv") %>%
  mutate(Year = as.character(Year))

#load in and format known population data (2010-2020)
POPrecent <- list()
POPrecent[["2010"]] <- POP[["2010"]]
POPrecent[["2015"]] <- POP[["2015"]]
POPrecent[["2020"]] <- POP[["2020"]]
for(i in names(POPrecent)){
  POPrecent[[i]] <- POPrecent[[i]] %>%
    group_by(Region, Sex, Age) %>%
    summarize(Population = sum(Population))
}

#load in and format projected population data (2025-2050, calculated on 16 Aug 2021)
POPPROJcopy <- POPPROJ
POPPROJcopy[[1]] <- NULL  #remove the blank 2020 from POPPROJ (investigate that later)
for(i in names(POPPROJcopy)){
  POPPROJcopy[[i]] <- POPPROJcopy[[i]] %>%
    rename(Population = ProjectedPop_final)
}

#combine the two lists above into one list
laborforce <- c(POPrecent, POPPROJcopy)

#join the GQ ratios to the population for every year
for(i in names(laborforce)){
  laborforce[[i]] <- left_join(laborforce[[i]], GQratios, by=c("Region","Age","Sex")) %>%
    mutate(GQpop = Population * GQratio,
           NonGQpop = round(Population - GQpop,0)) %>%
    filter(!Age %in% kiddos)
}

#join the LSPRs to each of the tables in laborforce
laborforce[["2010"]] <- laborforce[["2010"]] %>% left_join(LFPRs[c(1:2,3)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2010)
laborforce[["2015"]] <- laborforce[["2015"]] %>% left_join(LFPRs[c(1:2,4)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2015)
laborforce[["2020"]] <- laborforce[["2020"]] %>% left_join(LFPRs[c(1:2,5)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2020)
laborforce[["2025"]] <- laborforce[["2025"]] %>% left_join(LFPRs[c(1:2,6)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2025)

for(i in c("2030","2035","2040","2045","2050")){
  laborforce[[i]] <- laborforce[[i]] %>% left_join(LFPRs[c(1:2,7)], by=c("Age","Sex")) %>% rename(LFPR = LFPR2030)
}

#multiply NonGQpop * LFPR for each year
for(i in names(laborforce)){
  laborforce[[i]] <-  laborforce[[i]] %>%
    ungroup() %>%
    mutate(laborforce = NonGQpop * (LFPR/100))
}

#condense laborforce into one table
workers <- tibble()
i=1
for(item in laborforce){
  #print(item)
  temp <- item
  temp$year <- names(laborforce)[i]
  workers <- bind_rows(workers, temp)
  i <- i + 1
}

#total up laborforce, join with unemployment rate, calculate number of workers
workers <- workers %>%
  #select(-starts_with("LFPR")) %>%
  group_by(Region, year) %>%
  summarize(totlaborforce = sum(laborforce)) %>%
  left_join(unemp, by=c("year" = "Year")) %>%
  mutate(workers = round(totlaborforce * (1 - (Unemployment.Rate / 100)),0)) %>%
  select(-totlaborforce, -Unemployment.Rate)

#let's plot to see what that looks like
p <- workers %>% ggplot(aes(x=year, y=workers)) + geom_point() + facet_wrap(~Region, scales="free") + ggtitle("Number of Workers, 2010-2050")
p
