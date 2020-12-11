# DemographicForecasting

Code for the demographic portion of CMAP's forthcoming regional socioeconomic forecasting update.


### GroupQuarters.R
     
This code extracts the 2010 group quarters data from the Census API, and produces three datasets. 
	
1. `GQ` contains all of the group quarters data for 2010
2. `GQ_INST` contains only the institutionalized group quarters population data
3. `GQ_NONINST` contains only the non-institutionalized group quarters population data
			
This code codes not contain any analysis for GQ data. Analysis is found in the Rmarkdown file. 


### Population.R

This code extracts 2000 and 2010 data from the Census API. 1990 data, not available via the API, is instead stored in **Pop1990.xlsx** (included in this repository). This file comes from the Berger report.
	
- `POP` is a named list containing the population data from each decennial census
	- `POP[["1990"]]` contains 1990 population data
  - `POP[["2000"]]` contains 2000 population data
  - `POP[["2010"]]` contains 2010 population data
