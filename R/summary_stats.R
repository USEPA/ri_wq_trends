source(here::here("R/functions.R"))
ww_all<-read_csv(here("data/ww_lake_trend_data.csv"))
param_ranges <- ww_all %>%
  filter(param == "chla" |
           param == "temp" |
           param == "total_n" |
           param == "total_p") %>%
  group_by(param) %>%
  summarize(min = round(min(mn_measurement, na.rm = T), 2),
            percentile_25 = round(quantile(mn_measurement, probs = 0.25, na.rm = T), 2),
            mean = round(mean(mn_measurement, na.rm = T), 2),
            median = round(median(mn_measurement, na.rm = T), 2),
            percentile_75 = round(quantile(mn_measurement, probs = 0.75, na.rm = T), 2),
            max = round(max(mn_measurement, na.rm = T), 2),
            sd = round(sd(mn_measurement, na.rm = T), 2)) 

param_anomaly_ranges <- ww_all %>%
  filter(param == "chla" |
           param == "temp" |
           param == "total_n" |
           param == "total_p") %>%
  group_by(param) %>%
  summarize(min = round(min(measurement_anmly, na.rm = T), 2),
            percentile_25 = round(quantile(measurement_anmly, probs = 0.25, na.rm = T), 2),
            mean = round(mean(measurement_anmly, na.rm = T), 2),
            median = round(median(measurement_anmly, na.rm = T), 2),
            percentile_75 = round(quantile(measurement_anmly, probs = 0.75, na.rm = T), 2),
            max = round(max(measurement_anmly, na.rm = T), 2),
            sd = round(sd(measurement_anmly, na.rm = T), 2))
