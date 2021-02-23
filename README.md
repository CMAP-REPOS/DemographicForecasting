# DemographicForecasting

Code for the demographic portion of CMAP's forthcoming regional socioeconomic forecasting update.


### GroupQuarters.R
     
This code extracts the 2010 group quarters data from the Census API, and produces three datasets. 
	
1. `GQ` contains all group quarters data for 2010 for the respective states/counties
2. `GQ_INST` contains only institutionalized group quarters population data
3. `GQ_NONINST` contains only non-institutionalized group quarters population data
			
This code codes not contain any analysis for GQ data. Analysis can be found in 'GQ_Report.rmd' and html output of analysis is stored in 'GroupQuarters.html.' 


### Population.R

This code extracts 2000 and 2010 data from the Census API. 1990 data, not available via the API, is instead stored in **Pop1990.xlsx** (included in this repository). This file comes from the Berger report.
	
- `POP` is a named list containing the population data from each decennial census
	- `POP[["1990"]]` contains 1990 population data
  - `POP[["2000"]]` contains 2000 population data
  - `POP[["2010"]]` contains 2010 population data

### PEP.R

This code extracts 2015-2019 population estimates data from the Census Bureau's Population Estimates Program (PEP). 1995 and 2005 data is not available via the API. Instead, this data is stored in **Pop1995.xlsx** and **Pop2005.xlsx**, respectively (included in this repository). These files come from the Berger report. This data is also stored in the named list 'POP' along with the decenial census data. 

  - `POP[["1995"]]` contains 1990 population data
  - `POP[["2005"]]` contains 2000 population data
  - `POP[["2015"]]` contains 2015 population data
  - `POP[["2016"]]` contains 2016 population data
  - `POP[["2017"]]` contains 2017 population data
  - `POP[["2018"]]` contains 2018 population data
  - `POP[["2019"]]` contains 2019 population data
  
  
  
