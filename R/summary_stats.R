source(here::here("R/functions.R"))

ww_all<-read.csv(here("data/ww_lake_trend_data.csv"), stringsAsFactors = FALSE)
ww_param_ranges <- ww_all %>%
  filter(param == "chla" |
           param == "temp" |
           param == "total_n" |
           param == "total_p" |
           param == "np_ratio") %>%
  mutate(param = factor(param,levels = c("temp", "total_n", "total_p", "np_ratio","chla"))) %>%
  group_by(param) %>%
  summarize(min = round(min(mn_measurement, na.rm = T), 2),
            percentile_25 = round(quantile(mn_measurement, probs = 0.25, na.rm = T), 2),
            mean = round(mean(mn_measurement, na.rm = T), 2),
            median = round(median(mn_measurement, na.rm = T), 2),
            percentile_75 = round(quantile(mn_measurement, probs = 0.75, na.rm = T), 2),
            max = round(max(mn_measurement, na.rm = T), 2),
            sd = round(sd(mn_measurement, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(units = c("celsius", "µg/l", "µg/l", "", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )

ww_param_anomaly_ranges <- ww_all %>%
  filter(param == "chla" |
           param == "temp" |
           param == "total_n" |
           param == "total_p"|
           param == "np_ratio") %>%
  mutate(param = factor(param,levels = c("temp", "total_n", "total_p", "np_ratio", "chla"))) %>%
  group_by(param) %>%
  summarize(min = round(min(measurement_anmly, na.rm = T), 2),
            percentile_25 = round(quantile(measurement_anmly, probs = 0.25, na.rm = T), 2),
            mean = round(mean(measurement_anmly, na.rm = T), 2),
            median = round(median(measurement_anmly, na.rm = T), 2),
            percentile_75 = round(quantile(measurement_anmly, probs = 0.75, na.rm = T), 2),
            max = round(max(measurement_anmly, na.rm = T), 2),
            sd = round(sd(measurement_anmly, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(units = c("celsius", "µg/l", "µg/l", "", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )

lagos_all <- read_csv(here("data/lagos_lake_trend_data.csv"))

lagos_param_ranges <- lagos_all %>%
  filter(param == "chla" |
           param == "total_n" |
           param == "total_p" |
           param == "np_ratio") %>%
  mutate(param = factor(param,levels = c("total_n", "total_p", "np_ratio","chla"))) %>%
  group_by(param) %>%
  summarize(min = round(min(measurement, na.rm = T), 2),
            percentile_25 = round(quantile(measurement, probs = 0.25, na.rm = T), 2),
            mean = round(mean(measurement, na.rm = T), 2),
            median = round(median(measurement, na.rm = T), 2),
            percentile_75 = round(quantile(measurement, probs = 0.75, na.rm = T), 2),
            max = round(max(measurement, na.rm = T), 2),
            sd = round(sd(measurement, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(units = c("µg/l", "µg/l", "", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )

lagos_param_anomaly_ranges <- lagos_all %>%
  filter(param == "chla" |
           param == "total_n" |
           param == "total_p" |
           param == "np_ratio") %>%
  mutate(param = factor(param,levels = c("total_n", "total_p", "np_ratio", "chla"))) %>%
  group_by(param) %>%
  summarize(min = round(min(measurement_anmly, na.rm = T), 2),
            percentile_25 = round(quantile(measurement_anmly, probs = 0.25, na.rm = T), 2),
            mean = round(mean(measurement_anmly, na.rm = T), 2),
            median = round(median(measurement_anmly, na.rm = T), 2),
            percentile_75 = round(quantile(measurement_anmly, probs = 0.75, na.rm = T), 2),
            max = round(max(measurement_anmly, na.rm = T), 2),
            sd = round(sd(measurement_anmly, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(units = c("µg/l", "µg/l", "", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )
