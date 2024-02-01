years <- paste0("x",c(1970:2053))

ag_moodys_county <- read_xlsx("sandbox/Migration Work/CMAP_Ag_Proc_20230901.xlsx") %>%
  filter(str_length(FIP) == 5 &
           str_detect(Description, "Employment")) %>%
  janitor::clean_names() %>%
  mutate_at(c(years), as.numeric) %>%
  select(description, geography, all_of(years)) %>%
  filter(geography %in% c("Cook County (IL)","DuPage County (IL)","Kane County (IL)",
                          "Kendall County (IL)","Lake County (IL)","McHenry County (IL)","Will County (IL)"))

payroll_Moodys_county <- read_xlsx("sandbox/Migration Work/CMAP_Baseline_Proc_20230831.xlsx") %>%
  janitor::clean_names() %>%
  mutate_at(c(years), as.numeric) %>%
  select(description, geography, all_of(years)) %>%
  filter(geography %in% c("Cook County (IL)","DuPage County (IL)","Kane County (IL)",
                          "Kendall County (IL)","Lake County (IL)","McHenry County (IL)","Will County (IL)")) %>%
  filter(description == "Employment: Total payroll employment (including non-BLS sectors), (Ths. #, SA)")


combined_all <- rbind(ag_moodys_county, payroll_Moodys_county) %>%
  group_by(geography) %>%
  summarise(across(c(years), sum, na.rm = TRUE)) %>% #depreciated syntax
  mutate(across(years, ~ .x*1000)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("year") %>%
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names() %>%
  mutate(year = str_remove(geography, "x")) %>%
  mutate_at(c(2:8), as.numeric)



plot_jobs <- ggplot(combined_all, aes(year, group = 1)) +
  # geom_line(aes(y = cook_county_il, colour = "Cook")) +
  # geom_line(aes(y = du_page_county_il, colour = "DuPage")) +
  # geom_line(aes(y = kane_county_il, colour = "Kane")) +
  geom_line(aes(y = kendall_county_il, colour = "Kendall")) +
  # geom_line(aes(y = lake_county_il, colour = "Lake")) +
  geom_line(aes(y = mc_henry_county_il, colour = "McHenry")) +
  geom_line(aes(y = will_county_il, colour = "Will")) +
  theme_cmap(axis.title = element_text(),
             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  expand_limits(y = 0) +
  ggtitle("Employment Over Time (County)") +
  ylab("Employment Over Time") +
  xlab("Year")

plot_jobs
