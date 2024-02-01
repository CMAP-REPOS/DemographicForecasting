library(tidyverse)
library(tidycensus)

PEP_TEMP <- get_estimates(product="characteristics", geography = "county",
                          state = "IL", breakdown = c("SEX", "AGEGROUP"),
                          breakdown_labels = TRUE, year = 2022, show_call=TRUE)



pop_data_2022 <- PEP_TEMP |>
  filter(! AGEGROUP %in% c("All ages","Age 0 to 4 years", "Age 5 to 9 years","Age 10 to 14 years")) |>
  select(!c("GEOID","NAME","year")) |>
  rename(Sex = SEX, Age = AGEGROUP) |>
  mutate(Age = str_remove(Age,"Age "),
         Age = ifelse(Age == "85 years and older","85 years and over",Age))

load("Output/GQData2.Rdata")

new_gq <- GQratios |> select(!starts_with("GQ_N")) |>  mutate_at(vars(GQ_Inst_Juv:GQ_Inst_Nurs),  replace_na, 0) |> filter(Region == "CMAP Region") |> select(!Region) |>
  mutate(total_GQ = GQ_Inst_Juv + GQ_Inst_Other + GQ_Inst_Corr + GQ_Inst_Nurs) |> select(!starts_with("GQ_I"))

non_inst_pop <- pop_data_2022 |>
  left_join(new_gq) |>
  mutate(new_pop = value * (1-total_GQ))

total_labor_force_denom <- sum(non_inst_pop$new_pop)

jobs <- 4337.22*1000
workers <- jobs/1.049
unemp_rate <- 0.047
labor_force <- workers/(1-unemp_rate)

lfpr <- labor_force/total_labor_force_denom
