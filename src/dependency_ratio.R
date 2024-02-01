library(tidyverse)


load("Output/POP_PEP.Rdata") # POP

View(POP$`2012`)

dep_ratio_df <- list()

for (YEAR in names(POP)) {
  dep_ratio_df[[YEAR]] <-   POP[[YEAR]] |>
    mutate(age_group = case_when(
    Age %in% c("0 to 4 years","5 to 9 years","10 to 14 years","15 to 19 years") ~ "Youth",
    Age %in% c("65 to 69 years","70 to 74 years","75 to 79 years","80 to 84 years","85 years and older","85 years and over",
               "85 years and over") ~ "Retired",
    T ~ "Working_Age"
                                 )
          ) |>
    group_by(Region, age_group) |>
    summarize(number = sum(Population)) |>
    mutate(year = YEAR)
}

dep_df <- tibble()
for (YEAR in names(POP)) {
  dep_df <- rbind(dep_df, dep_ratio_df[[YEAR]])
}

dep_df_wide <- dep_df |>
  pivot_wider(names_from = age_group, values_from = number) |>
  mutate(youth_ratio = Youth/Working_Age,
         retired_ratio = Retired/Working_Age,
         combined_ratio =  (Retired+Youth) / Working_Age,
         .keep = "unused") |>
  pivot_wider(names_from = Region, values_from = c("youth_ratio","retired_ratio","combined_ratio")) |>
  janitor::clean_names()

dep_ratio_cmap <- dep_df_wide |>
  select(year, ends_with("cmap_region")) |>
  mutate(year = as.numeric(year))

quick_linear <- lm(combined_ratio_cmap_region ~ year,dep_ratio_cmap)
quick_linear

dep_ratio_51 <- -3.962447 + (2051*0.002289)



load(r"(C:\Users\abahls\Downloads\PopProj (1).Rdata)")

proj_2050 <- POPPROJ$`2050`

dep_2050 <- proj_2050 |>
  filter(Region == "CMAP Region") |>
  mutate(age_group = case_when(
    Age %in% c("0 to 4 years","5 to 9 years","10 to 14 years","15 to 19 years") ~ "Youth",
    Age %in% c("65 to 69 years","70 to 74 years","75 to 79 years","80 to 84 years","85 years and older","85 years and over",
               "85 years and over") ~ "Retired",
    T ~ "Working_Age"
  )
  ) |>
  group_by(age_group) |>
  summarize(number = sum(ProjectedPop_final)) |>
  pivot_wider(names_from = age_group, values_from = number)

dep_ratio_50 <- (dep_2050$Retired + dep_2050$Youth) / dep_2050$Working_Age

workers_per_job_22 <- 5017999/(4.29*10^6)
projected_workers_51 <- (4.27*10^6)*workers_per_job_22
projcted_pop_22 <- 5017999*(1+0.6738311)
projcted_pop_51_dep_22 <- projected_workers_51*(1+0.6738311)
projcted_pop_51_dep_51 <- projected_workers_51*(1+dep_ratio_51) #linear trend
projcted_pop_51_dep_50 <- projected_workers_51*(1+dep_ratio_50) #CMAP last time


