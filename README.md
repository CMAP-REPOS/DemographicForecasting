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

This code extracts 2011-2019 population estimates data from the Census Bureau's Population Estimates Program (PEP). 1995 and 2005 data is not available via the API. Instead, this data is stored in **Pop1995.xlsx** and **Pop2005.xlsx**, respectively (included in this repository). These files come from the Berger report. This data is also stored in the named list 'POP' along with the decenial census data. 

  - `POP[["1995"]]` contains 1995 population data
  - `POP[["2005"]]` contains 2005 population data
  - `POP[["2011"]]` contains 2011 population data
  - `POP[["2012"]]` contains 2012 population data....and so on through 2019
  
 ### Fertility.R
 
 **ASFR Calculations:** 
 Note: these steps are detailed in Excel spreadsheets for each region
 
 This code calculates the Age Specific Fertility Rates (ASFRs) for women between the ages of 15-44 for years 2010-2019. ASFR projections are then calculated 
 out to 2050.
 
 Existing data from Berger provided birth statistics from 1990-2014. More recent vital stats were requested from the Illinois, Wisconsin and Indiana health 
 departments. Depending on department, birth data was received through 2018 or 2020. Birth data for the youngest female group (10-14) were combined with the
 15-19 group and data for the oldest female group (45-49) were combined with 40-44 group. Note: IL provided birth data for females 50+. For consistency across states, 
 that data was not included. 
 
 A key part of this code is the removal of GQ female estimates from the 2011-2019 population estimates data. Population estimates data, unlike Decennial Census
 data, includes Group Quarters populations. We are working under the assumption that females in group quarters will not be pregnant, therefore we want to remove
 GQ estimates from the population estimates data. The analysis is done in the following steps:
 
 
 Step 1: 
  - Calculate 2010 GQ totals by County, excluding military 

Step 2: 
  - Calculate 2010 Female GQ totals by County, Sex, Age, excluding military (2010 GQ data is the only GQ data with Age/Sex details) 

Step 3: 
  - For each GQ female age group of interest (data from Step 2), calculate what proportion of each county's total 2010 GQ population they 
    represent (data from Step 1)

Step 4: 
  - Read in GQ Census estimates by State, County for 2011-2019 (data stored in **GQE.xlsx**) (not available by sex, age)

Step 5: 
  - For each year of GQ Census estimates (2011-2019), multiply County totals by the above proportions for each female age group (15-44) to estimate the number
    of women by county, by age group

Step 6: 
  - Filter female Household population estimates for 2011-2019 for females

Step 7:
  - For 2011-2019 population estimate data, subtract expected GQ values from county total for each age group to estimate County Household populations

Step 8: 
- Filter 2010 Decencial Census HH Data for females 15-44

Step 9:
- Merge 2010 Census HH Population data and 2011-2019 HH Population Estimates to form a complete table of Household Population data
 
**ASFR Projections**
Note: Following along with Excel workbook version may be useful here
 
**Sheet 1**

Step 1:
- Calculate a table of births by age of mother for years 2010-2018

Step 2: 
- For 2014, pull Census PEP data from the API (note: 2014 is the base year as it's the halfway point between 2010 and 2018)

Step 3: 
- Calculate Age-Specific Fertility Rates, Centered at 2014
- Calculation: Total births per age group from 2010-2018 / 2014 Population Estimate for age group /  9 (number of years in 2010-2018) 

Step 4: 
- Limit age groups to 15-44 (Remember, in analysis 10-14 births are combined with 15-19 and 45-49 births are combined with 40-44) 
- Re-Calculate ASFRs to combine 10-14 and 15-19: Total births for both groups from 2010-2018 / 2014 Population Estimate for 15-19 age group / 9
- Re-Calculate ASFRs to combine 40-44 and 45-49: Total births for both groups from 2010-2018 / 2014 Population Estimate for 40-44 age group / 9  

**Sheet 2**

Step 5: 
- Pull in projected Age-Specific Fertility Rates (ASFRs), U.S. Census Bureau National Projections, vintage 2014 

**Sheet 3**

Step 6: 
- By year, for each age in a given age group, sum Census Bureau's Projected 1-Year ASFRs (sheet 2); divide this value by 5, which is the number of ages in (most) age groups.    This calculates and average ASFR for each age group at the national level. There are two exceptions: 

For 15-19:  Since the Census Bureau's Projected 1-Year ASFRs start at 14, age 14 is included in the 15-19 age group calculation. Their sum is still divided by 5 (even though it includes 6 ages) because the ASFRs for 14 are so insignificant.  	

For 40-44: Since the Census Bureau's Projected 1-Year ASFRs end at age 54, ages 45-54 are included in the 40-44 age group calculation. Their sum is still divided by 5 (even though it includes 15 ages) because the ASFRs for 45+ are so insignificant.  

Step 7: 
- Calculate the ratios of the above calculated ASFRs to the launch year of calculated ASFRs 2014. Using the national projections from the Census, the ratios are between each of our projected years and our base year (2014). (divide each summed value in step 6 by the 2014 value for that age group)

Step 8:
 
 ### Mortality.R
 This code utilizes mortality data from 2014-2018 as well as Social Security Administration (SSA) data to project mortality rates out to 2060. The data inputs are: 
 
 - CMAPMortality1990-2019.xlsx: This data was received from each state's health department and the file is stored in the 'Input' folder 
 -  SSA.xlsx: This file utilizes SSA data, which provides historical and projected period life tables by single year of age, gender, and calendar year for years 1900 through 2095. This files is stored in the 'Input' folder. The calculations were done by David Egan-Roberston and the spreadsheet detailing his work is saved on OneDrive.

The steps for projecting mortality are: 

1. For years 2014-2018, split population age group 0-4 into 0-1 and 1-4 in order to match mortality age groups. This is done by assigning 1/5 of the original group to the 0-1 category and 4/5 of the original group to the 1-4 category. 
2. Join mortality data with the updated population data from the above step. This data is stored in 'MORT_DATA' 
3. Perform life table calculations as outlined in the each region's projections spreadsheet, saved on OneDrive (definitions and logic provided detailed here)
4. Join life tables with SSA tables 
5. Perform final projection calculation, which is multiplying each value in the SSA table with the corresponding values in the Sx column
6. CHECK IF ANY VALUES GO OVER 1 - if they do, must adjust these rates based on rules provided by Daivd E-R (NEED TO REQUEST HE WRITE THESE)
