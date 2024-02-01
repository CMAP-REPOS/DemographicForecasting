years <- paste0("x",c(1970:2053))

ag_moodys_all <- read_xlsx("sandbox/Migration Work/CMAP_Ag_Proc_20230901.xlsx") %>%
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


payroll_Moodys_all <- read_xlsx("sandbox/Migration Work/CMAP_Baseline_Proc_20230831.xlsx") %>%
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


combined_all <- rbind(ag_moodys_all, payroll_Moodys_all) %>%
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
  select(-c(CMAP, `External IL`, IN, WI)) %>%
  mutate(year = str_remove_all(year,"x"))


plot_jobs <- ggplot(combined_all, aes(year, group = 1)) +
  geom_line(aes(y = emp_cmap, colour = "CMAP Region")) +
  geom_line(aes(y = emp_ex_il, colour = "External IL")) +
  geom_line(aes(y = emp_in, colour = "Wisconsin")) +
  geom_line(aes(y = emp_wi, colour = "Indiana")) +
  theme_cmap(axis.title = element_text(),
             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  expand_limits(y = 0) +
  ggtitle("Employment Over Time (Region)") +
  ylab("Employment Over Time") +
  xlab("Year")

plot_jobs


proj <- combined_all %>%
  mutate(average_ratio = emp_cmap*average_ppj,
         recent_ratio = emp_cmap*recent_ppj,
         max_ratio = emp_cmap*max_ppj,
         min_ratio = emp_cmap*min_ppj) %>%
  select(year, average_ratio, recent_ratio, max_ratio, min_ratio)




plot_proj <- ggplot(proj, aes(year, group = 1)) +
  geom_line(aes(y = average_ratio, colour = "Average (2010-2022)")) +
  geom_line(aes(y = recent_ratio, colour = "2022")) +
  geom_line(aes(y = max_ratio, colour = "Max (2010-2022)")) +
  geom_line(aes(y = min_ratio, colour = "Min (2010-2022)")) +
  theme_cmap(axis.title = element_text(),
             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  expand_limits(y = 0) +
  ggtitle("Raw Projections") +
  ylab("Pop") +
  xlab("Year")

plot_proj

