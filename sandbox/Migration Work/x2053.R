years <- "x2053"

ag_moodys_2050 <- read_xlsx("sandbox/Migration Work/CMAP_Ag_Proc_20230901.xlsx") %>%
  filter(str_length(FIP) == 5 &
           str_detect(Description, "Employment")) %>%
  janitor::clean_names() %>%
  mutate_at(c(years), as.numeric) %>%
  select(description, geography, all_of(years)) %>%
  mutate(region = case_when(str_detect(geography,"WI") ~ "WI",
                            str_detect(geography,"IN") ~ "IN",
                            geography %in% c("Cook County (IL)","DuPage County (IL)","Kane County (IL)",
                                             "Kendall County (IL)","Lake County (IL)","McHenry County (IL)","Will County (IL)") ~ "CMAP",
                            T ~ "External IL"),
         source = "ag")


payroll_Moodys_2050 <- read_xlsx("sandbox/Migration Work/CMAP_Baseline_Proc_20230831.xlsx") %>%
  janitor::clean_names() %>%
  mutate_at(c(years), as.numeric) %>%
  select(description, geography, all_of(years)) %>%
  mutate(region = case_when(str_detect(geography,"WI") ~ "WI",
                            str_detect(geography,"IN") ~ "IN",
                            geography %in% c("Cook County (IL)","DuPage County (IL)","Kane County (IL)",
                                             "Kendall County (IL)","Lake County (IL)","McHenry County (IL)","Will County (IL)") ~ "CMAP",
                            T ~ "External IL"),
         source = "payroll") %>%
  filter(description == "Employment: Total payroll employment (including non-BLS sectors), (Ths. #, SA)")


combined_2050 <- rbind(ag_moodys_2050, payroll_Moodys_2050) %>%
  group_by(region) %>%
  summarise(across(c(years), sum, na.rm = TRUE)) %>% #depreciated syntax
  mutate(across(years, ~ .x*1000)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("year") %>%
  janitor::row_to_names(row_number = 1) %>%
  mutate(year = region,
         emp_cmap = as.numeric(CMAP),
         emp_ex_il = as.numeric(`External IL`),
         emp_in = as.numeric(IN),
         emp_wi = as.numeric(WI)) %>%
  select(-c(CMAP, `External IL`, IN, WI))



average_ppj <- mean(df$ppl_per_emp_cmap)
recent_ppj <- df$ppl_per_emp_cmap[df$year==2022]
max_ppj <- max(df$ppl_per_emp_cmap)
min_ppj <- min(df$ppl_per_emp_cmap)

moodys_jobs_future <- combined_2050$emp_cmap
moodys_jobs_today <- df$emp_cmap[df$year==2022]

job_growth <- moodys_jobs_future - moodys_jobs_today

proj_average <- moodys_jobs_future * average_ppj
proj_recnet <- moodys_jobs_future * recent_ppj
proj_max <- moodys_jobs_future * max_ppj
proj_min <- moodys_jobs_future * min_ppj






