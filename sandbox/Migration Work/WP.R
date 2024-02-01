
library(tidyverse)

wp <- read_excel("sandbox/WP/WP_Agg.xlsx")
wp <- wp %>%
  mutate_at(c(2:8), ~ .*1000) %>%
  mutate(region = rowSums(wp[2:8]))

pop_today <- wp$region[wp$Year == 2022]
pop_2053 <- 10000000
year_diff <- 2053-2022

pop2000 <- wp$region[wp$Year == 2000]

new_year_diff <- 2022-2000
average_change <- (pop_today - pop2000)/new_year_diff

average_growth <- (pop_2053-pop_today)/year_diff

wp <- wp %>%
  mutate(region10 = case_when(Year<= 2022 ~ region,
                              T ~ pop_today + (average_growth*(Year-2022))),
         region_2000 = case_when(Year<= 2022 ~ region,
                                 T ~ pop_today + (average_change*(Year-2022)))) %>%
  mutate(growth = region - lag(region),
         indicator = case_when(growth > average_growth ~ 1,
                               T ~ 0))



#1970 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 12% of years

plot_wp <- ggplot(wp, aes(Year, group = 1)) +
  # geom_line(aes(y = Cook, colour = "Cook")) +
  geom_line(aes(y = DuPage, colour = "DuPage")) +
  geom_line(aes(y = Kane, colour = "Kane")) +
  geom_line(aes(y = Kendall, colour = "Kendall")) +
  geom_line(aes(y = Lake, colour = "Lake")) +
  geom_line(aes(y = McHenry, colour = "McHenry")) +
  geom_line(aes(y = Will, colour = "Will")) +
  theme_cmap(axis.title = element_text(),
             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  expand_limits(y = 0) +
  ggtitle("Population Over Time (WP)") +
  ylab("Population Over Time (WP)") +
  xlab("Year")

plot_wp

plot_wp_region <- ggplot(wp, aes(Year, group = 1)) +
  geom_line(aes(y = region, colour = "CMAP Region (WP)"))  +
  geom_line(aes(y = region10, colour = "CMAP Region 10 Million (WP)"))  +
  geom_line(aes(y = region_2000, colour = "CMAP Region 2000 Growth (WP)"))  +
  theme_cmap(axis.title = element_text(),
             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  expand_limits(y = 0) +
  ggtitle("Population Over Time (WP)") +
  ylab("Population Over Time (WP)") +
  xlab("Year")

plot_wp_region
