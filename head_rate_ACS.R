library(tidycensus)

ACS_2005 <- load_variables(2005, "acs1")

ACS_2006 <- load_variables(2006, "acs1")

ACS_2007 <- load_variables(2007, "acs3")

ACS_2008 <- load_variables(2008, "acs3")

ACS_2009 <- load_variables(2009, "acs5") # 2009 ACS5 doesn't have ethnicity info for householders
# B25006_001 Householders Total
# B25006_002 White
# B25006_003 Black
# B25006_005 Asian

# B02001_001 Population Total
# B02001_002 White
# B02001_003 Black
# B02001_005 Asian

ACS_2010 <- load_variables(2010, "acs5")
# B25006_001 Householders Total
# B25006_002 White
# B25006_003 Black
# B25006_005 Asian
# B22005H_001 NH_White
# B22005I_001 Total Hispanic Householders (Data from Receipt of SNAP)

# B02001_001 Population Total
# B02001_002 White
# B02001_003 Black
# B02001_005 Asian
# B03002_003 NH_White
# B03002_012 Hispanic

ACS_2011 <- load_variables(2011, "acs5")
# Same as 2010

ACS_2012 <- load_variables(2012, "acs5")
# Same as 2010

ACS_2013 <- load_variables(2013, "acs5")
# Same as 2010

ACS_2014 <- load_variables(2014, "acs5")
# Same as 2010

ACS_2015 <- load_variables(2015, "acs5")
# Same as 2010

ACS_2016 <- load_variables(2016, "acs5")
# Same as 2010

ACS_2017 <- load_variables(2017, "acs5")
# Same as 2010

ACS_2018 <- load_variables(2018, "acs5")
# Same as 2010

ACS_2019 <- load_variables(2019, "acs5")
# Same as 2010
###################################################

# for (i in 2009:2019){
#   name1 = paste0("vars_",as.character(i))
#   name2 = paste0("concept_",as.character(i))
#     df1 = load_variables(i,"acs5")
#     df2 = as.data.frame(unique(df1$concept))
#     assign(name1, df1)
#     assign(name2, df2)
# }
