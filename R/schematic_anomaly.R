library(readr)
library(dplyr)
library(here)

schematic <- read_csv(here("data/schematic.csv"))

station_year_median <- schematic %>% 
  group_by(station, year) %>%
  summarize(station_year_median = median(meas)) %>%
  round(2) %>%
  ungroup()

station_year_anomaly <- station_year_median %>%
  group_by(station) %>%
  mutate(long_term_median = median(station_year_median),
         long_term_iqr = IQR(station_year_median)) %>%
  mutate(station_year_anomaly = station_year_median - long_term_median) %>%
  round(2) %>%
  ungroup()
  
median_anomaly <- station_year_anomaly %>%
  group_by(year) %>%
  summarize(median_anomaly = median(station_year_anomaly),
            iqr_anomaly = IQR(station_year_anomaly),
            n = n()) %>%
  round(2) %>%
  ungroup()

