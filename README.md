# DemographicForecasting
Code for Demographic portion of Socioeconomic Forecasting update

GroupQuarters.R: 
     This code extracts the 2010 group quarters data from the Census API. There are three dfs in this file. 
	
			1. GQ: contains all of the group quarters data for 2010
			2. GQ_inst: contains only the group quarters population that are institutionalized 
			3. GQ_noninst: contains only the group quarters population that is not institutionalized 
			
     This code codes not contain any analysis for GQ data. Anlysis is found in the markdown file. 
	
Population.R:

	This code extracts 2000 and 2010 data from the Census API. 1990 data is in .xlsx format stored in the repository with the code. This file comes 
	from the Berger report. 
	
     		1. pop_2000: contains 2000 population data
			2. pop_2010: contains 2010 population data
			3. Pop1990: contains 1990 population data
   
