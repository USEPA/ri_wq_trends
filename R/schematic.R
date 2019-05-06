library(readr)
library(dplyr)
schematic <- read_csv(here::here("data/schematic.csv"))

station_year_mean <- schematic %>% 
  group_by(station, year) %>%
  summarize(station_year_mean = mean(meas)) %>%
  ungroup()

station_year_z <- station_year_mean %>%
  group_by(station) %>%
  mutate(station_year_z = scale(station_year_mean),
         long_term_mean = mean(station_year_mean),
         long_term_sd = sd(station_year_mean)) %>%
  ungroup()
  
mean_z <- station_year_z %>%
  group_by(year) %>%
  summarize(mean_z = mean(station_year_z),
            sd_z = sd(station_year_z),
            n = n())
