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
  
 ### Fertility.R
 
 This code calculates the Age Specific Fertility Rates (ASFRs) for women between the ages of 15-44 for years 2010-2019. ASFR projections are then calculated 
 out to 2050.
 
 A key part of this code is the removal of GQ female estimates from the 2011-2019 population estimates data. Population estimates data, unlike Decennial Census
 data, includes Group Quarters populations. We are working under the assumption that females in group quarters will not be pregnant, therefore we want to remove
 GQ estimates from the population estimates data. The analysis is done in the followingn steps: 
 
 
 Step 1: 
  - Calculate 2010 GQ totals by County (2010 GQ data is the only GQ data with Age/Sex details) 

Step 2: 
  - Calculate 2010 Female GQ totals by County, by age groups (15-44)

Step 3: 
  - Calculate proportions of total 2010 GQ population that are female (15-44)

Step 4: 
  - Read in GQ Census estimates by State, County for 2011-2019 (data stored in **GQE.xlsx**)

Step 5: 
  - For each year of GQ Census estimates (2011-2019), multiply County totals by the above proportions for each female age group (15-44)

Step 6: 
  - Filter ffemale Household population estimates for 2011-2019 for females, 15-44 years

Step 7:
  - For 2011-2019 population estimate data, subtract expected GQ values from county total for each age group to estimate County Household populations


 
  
