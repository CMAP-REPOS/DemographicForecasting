library(tidyverse)
library(janitor)

#can make this cleaner/a function

## employment
Moodys_Raw <- readxl::read_excel("Moodys Forecasts\\CMAP_Baseline_20230831.XLSX")

Transformed_Moodys <- Moodys_Raw %>% 
  t() 

Transformed_Moodys <- cbind(newColName = rownames(Transformed_Moodys), Transformed_Moodys)
rownames(Transformed_Moodys) <- 1:nrow(Transformed_Moodys)

Transformed_Moodys <- Transformed_Moodys %>% 
  row_to_names(row_number = 1)

col_names <- colnames(Transformed_Moodys)
col_names_numeric <- as.numeric(col_names)
col_names_not_numeric <- setdiff(col_names,col_names_numeric)

col_names_numeric <-  col_names_numeric[!is.na(col_names_numeric)]
col_names_numeric <- year(as.Date(col_names_numeric, origin = "1899-12-30"))

col_names_final <- union(col_names_not_numeric,col_names_numeric)

colnames(Transformed_Moodys) <- col_names_final

Transformed_Moodys <- as_tibble(Transformed_Moodys)

writexl::write_xlsx(Transformed_Moodys,"Moodys Forecasts\\CMAP_Baseline_Proc_20230831.xlsx")

#AG
ag_raw <- readxl::read_excel("Moodys Forecasts\\CMAP_Ag_Baseline_20230901.XLSX")

Transformed_ag <- ag_raw %>% 
  t()

Transformed_ag <- cbind(newColName = rownames(Transformed_ag), Transformed_ag)
rownames(Transformed_ag) <- 1:nrow(Transformed_ag)


Transformed_ag <- Transformed_ag %>% 
  row_to_names(row_number = 1)

col_names <- colnames(Transformed_ag)
col_names_numeric <- as.numeric(col_names)
col_names_not_numeric <- setdiff(col_names,col_names_numeric) 

col_names_numeric <-  col_names_numeric[!is.na(col_names_numeric)]
col_names_numeric <- year(as.Date(col_names_numeric, origin = "1899-12-30"))

col_names_final <- union(col_names_not_numeric,col_names_numeric)

colnames(Transformed_ag) <- col_names_final

Transformed_ag <- as_tibble(Transformed_ag) %>% 
  rename("FIP" = "FIP:",
         "Description" = "Description:")

writexl::write_xlsx(Transformed_ag,"Moodys Forecasts\\CMAP_Ag_Proc_20230901.xlsx")
