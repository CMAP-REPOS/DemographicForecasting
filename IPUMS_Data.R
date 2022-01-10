# IPUMS data
library(ipumsr)
ddi <- read_ipums_ddi("Input/usa_00001.xml")
data <- read_ipums_micro(ddi)

preview <- head(data)

library(tidycensus)
load_variables(2020,"sf1")
