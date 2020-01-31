library(readr)
library(dplyr)
library(here)

schematic <- read_csv(here("data/schematic.csv"))

station_year_mean <- schematic %>% 
  group_by(station, year) %>%
  summarize(station_year_mean = mean(meas)) %>%
  round(2) %>%
  ungroup()

station_year_anomaly <- station_year_mean %>%
  group_by(station) %>%
  mutate(long_term_mean = mean(station_year_mean),
         long_term_sd = sd(station_year_mean)) %>%
  mutate(station_year_anomaly = station_year_mean - long_term_mean)
  round(2) %>%
  ungroup()
  
mean_anomaly <- station_year_anomaly %>%
  group_by(year) %>%
  summarize(mean_anomaly = mean(station_year_anomaly),
            sd_anomaly = sd(station_year_anomaly),
            n = n()) %>%
  round(2) %>%
  ungroup()
