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
  summarize(min = round(min(station_year_mean, na.rm = T), 2),
            percentile_25 = round(quantile(station_year_mean, probs = 0.25, na.rm = T), 2),
            mean = round(mean(station_year_mean, na.rm = T), 2),
            median = round(median(station_year_mean, na.rm = T), 2),
            percentile_75 = round(quantile(station_year_mean, probs = 0.75, na.rm = T), 2),
            max = round(max(station_year_mean, na.rm = T), 2),
            sd = round(sd(station_year_mean, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(units = c("°C", "µg/l", "µg/l", "molar", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )

# All about the base...Enforce ww sig digits
ww_param_ranges[ww_param_ranges$Parameter == "temp",][,3:8] <- 
  round(ww_param_ranges[ww_param_ranges$Parameter == "temp",][,3:8], 1)

ww_param_ranges[ww_param_ranges$Parameter == "total_n",][,3:8] <- 
  round(ww_param_ranges[ww_param_ranges$Parameter == "total_n",][,3:8]/5)*5

ww_param_ranges[ww_param_ranges$Parameter == "total_p",][,3:8] <- 
  round(ww_param_ranges[ww_param_ranges$Parameter == "total_p",][,3:8], 0)

ww_param_ranges[ww_param_ranges$Parameter == "np_ratio",][,3:8] <- 
  round(ww_param_ranges[ww_param_ranges$Parameter == "np_ratio",][,3:8], 2)

ww_param_ranges[ww_param_ranges$Parameter == "chla",][,3:8] <- 
  round(ww_param_ranges[ww_param_ranges$Parameter == "chla",][,3:8], 1)

ww_param_ranges <- ww_param_ranges %>%
  mutate(Parameter = c("Temperature", "Total Nitrogen", "Total Phosphorus", "N:P", "Chlorophyll"))

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
  mutate(units = c("°C", "µg/l", "µg/l", "molar", "µg/l")) %>%
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
  summarize(min = round(min(station_year_mean, na.rm = T), 2),
            percentile_25 = round(quantile(station_year_mean, probs = 0.25, na.rm = T), 2),
            mean = round(mean(station_year_mean, na.rm = T), 2),
            median = round(median(station_year_mean, na.rm = T), 2),
            percentile_75 = round(quantile(station_year_mean, probs = 0.75, na.rm = T), 2),
            max = round(max(station_year_mean, na.rm = T), 2),
            sd = round(sd(station_year_mean, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(units = c("µg/l", "µg/l", "molar", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )

# All about the base...Enforce ww sig digits
lagos_param_ranges[lagos_param_ranges$Parameter == "temp",][,3:8] <- 
  round(lagos_param_ranges[lagos_param_ranges$Parameter == "temp",][,3:8], 1)

lagos_param_ranges[lagos_param_ranges$Parameter == "total_n",][,3:8] <- 
  round(lagos_param_ranges[lagos_param_ranges$Parameter == "total_n",][,3:8]/5)*5

lagos_param_ranges[lagos_param_ranges$Parameter == "total_p",][,3:8] <- 
  round(lagos_param_ranges[lagos_param_ranges$Parameter == "total_p",][,3:8], 0)

lagos_param_ranges[lagos_param_ranges$Parameter == "np_ratio",][,3:8] <- 
  round(lagos_param_ranges[lagos_param_ranges$Parameter == "np_ratio",][,3:8], 2)

lagos_param_ranges[lagos_param_ranges$Parameter == "chla",][,3:8] <- 
  round(lagos_param_ranges[lagos_param_ranges$Parameter == "chla",][,3:8], 1)

lagos_param_ranges <- lagos_param_ranges %>%
  mutate(Parameter = c("Total Nitrogen", "Total Phosphorus", "N:P", "Chlorophyll"))

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
  mutate(units = c("µg/l", "µg/l", "molar", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )
