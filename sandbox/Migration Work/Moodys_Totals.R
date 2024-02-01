rm(list = ls())

options(scipen=999)

library(tidyverse)
library(tidycensus)
library(readxl)
library(cmapgeo)
library(janitor)
library(cmapplot)

years <- paste0("x",c(2010:2022, 2053))

ag_moodys <- read_xlsx("sandbox/Migration Work/CMAP_Ag_Proc_20230901.xlsx") %>%
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


payroll_Moodys <- read_xlsx("sandbox/Migration Work/CMAP_Baseline_Proc_20230831.xlsx") %>%
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


combined <- rbind(ag_moodys, payroll_Moodys) %>%
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

source("sandbox/Migration Work/workforce_benchmark.R")

rm(list=setdiff(ls(), c("all_wf_data","combined")))

df <- all_wf_data %>%
  mutate(year = paste0("x",year)) %>%
  left_join(combined) %>%
  clean_names() %>%
  mutate(year = str_remove_all(year,"x"))



name_list <- names(df)

name_list <- name_list %>% str_replace("external","ex")
name_list <- name_list %>% str_replace("ex_in","in")
name_list <- name_list %>% str_replace("ex_wi","wi")
name_list <- name_list %>% str_replace("cmap_region","cmap")

names(df) <- name_list

for (i in c("cmap","ex_il","in","wi")) {

pop_var <- paste0("total_pop_",i)
emp_var <- paste0("emp_",i)
crude_var <- paste0("crude_workforce_",i)
labor <- paste0("totlaborforce_",i)
workers <- paste0("workers_",i)

ratio_1 <- paste0("ppl_per_emp_",i)
ratio_2 <- paste0("crude_wf_per_emp_",i)
ratio_3 <- paste0("lf_per_emp_",i)
ratio_4 <- paste0("workers_per_emp_",i)

df[[ratio_1]] <- (df %>% pull(pop_var)) / (df %>% pull(emp_var))
df[[ratio_2]] <- (df %>% pull(crude_var)) / (df %>% pull(emp_var))
df[[ratio_3]] <- (df %>% pull(labor)) / (df %>% pull(emp_var))
df[[ratio_4]] <- (df %>% pull(workers)) / (df %>% pull(emp_var))

}


plot1 <- ggplot(df, aes(year, group = 1)) +
  geom_line(aes(y = ppl_per_emp_cmap, colour = "CMAP Region")) +
  geom_line(aes(y = ppl_per_emp_ex_il, colour = "External IL")) +
  geom_line(aes(y = ppl_per_emp_wi, colour = "Wisconsin")) +
  geom_line(aes(y = ppl_per_emp_in, colour = "Indiana")) +
  theme_cmap(axis.title = element_text()) +
  expand_limits(y = 0) +
  ggtitle("People Per Job") +
  ylab("People Per Job") +
  xlab("Year")

plot1


plot2 <- ggplot(df, aes(year, group = 1)) +
  geom_line(aes(y = crude_wf_per_emp_cmap, colour = "CMAP Region")) +
  geom_line(aes(y = crude_wf_per_emp_ex_il, colour = "External IL")) +
  geom_line(aes(y = crude_wf_per_emp_wi, colour = "Wisconsin")) +
  geom_line(aes(y = crude_wf_per_emp_in, colour = "Indiana")) +
  theme_cmap(axis.title = element_text()) +
  expand_limits(y = 0) +
  ggtitle("Crude Workforce Per Job") +
  ylab("Crude Workforce Per Job") +
  xlab("Year")

plot2



plot3 <- ggplot(df %>% filter(!is.na(lf_per_emp_cmap)), aes(year, group = 1)) +
  geom_line(aes(y = lf_per_emp_cmap, colour = "CMAP Region")) +
  geom_line(aes(y = lf_per_emp_ex_il, colour = "External IL")) +
  geom_line(aes(y = lf_per_emp_wi, colour = "Wisconsin")) +
  geom_line(aes(y = lf_per_emp_in, colour = "Indiana")) +
  theme_cmap(axis.title = element_text()) +
  expand_limits(y = 0) +
  ggtitle("Labor Force Per Job") +
  ylab("Labor Force Per Job") +
  xlab("Year")

plot3


plot4 <- ggplot(df %>% filter(!is.na(lf_per_emp_cmap)), aes(year, group = 1)) +
  geom_line(aes(y = workers_per_emp_cmap, colour = "CMAP Region")) +
  geom_line(aes(y = workers_per_emp_ex_il, colour = "External IL")) +
  geom_line(aes(y = workers_per_emp_wi, colour = "Wisconsin")) +
  geom_line(aes(y = workers_per_emp_in, colour = "Indiana")) +
  geom_line(aes(y = 1, colour = "One")) +
  theme_cmap(axis.title = element_text()) +
  expand_limits(y = 0) +
  ggtitle("Employee Per Job") +
  ylab("Employee Per Job") +
  xlab("Year")

plot4






