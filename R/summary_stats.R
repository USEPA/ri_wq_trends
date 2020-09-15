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
  summarize(min = round(min(station_year_median, na.rm = T), 2),
            percentile_25 = round(quantile(station_year_median, probs = 0.25, na.rm = T), 2),
            mean = round(mean(station_year_median, na.rm = T), 2),
            median = round(median(station_year_median, na.rm = T), 2),
            percentile_75 = round(quantile(station_year_median, probs = 0.75, na.rm = T), 2),
            max = round(max(station_year_median, na.rm = T), 2),
            sd = round(sd(station_year_median, na.rm = T), 2),
            n = n()) %>%
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
            sd = round(sd(measurement_anmly, na.rm = T), 2),
            n = n()) %>%
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
  summarize(min = round(min(station_year_median, na.rm = T), 2),
            percentile_25 = round(quantile(station_year_median, probs = 0.25, na.rm = T), 2),
            mean = round(mean(station_year_median, na.rm = T), 2),
            median = round(median(station_year_median, na.rm = T), 2),
            percentile_75 = round(quantile(station_year_median, probs = 0.75, na.rm = T), 2),
            max = round(max(station_year_median, na.rm = T), 2),
            sd = round(sd(station_year_median, na.rm = T), 2),
            n = n()) %>%
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
            sd = round(sd(measurement_anmly, na.rm = T), 2),
            n = n()) %>%
  ungroup() %>%
  mutate(units = c("µg/l", "µg/l", "molar", "µg/l")) %>%
  select(Parameter = param, Units = units, 
         "25th Percentile" = percentile_25,
         Mean = mean, Median = median, 
         "75th Percentile" = percentile_75,
         Max = max, "Std. Dev" = sd )

# Long Term Chlorophyll Trophic State Summary

ww_ts <- ww_all %>% 
  select(station_name, trophic_state) %>% 
  unique() %>%
  filter(!is.na(trophic_state)) %>%
  group_by(trophic_state) %>%
  summarize(percent = round(n()/nrow(.),3)*100) %>%
  mutate(trophic_state = factor(trophic_state, 
                                levels = c("oligotrophic", "mesotrophic", 
                                           "eutrophic", "hypereutrophic"))) %>%
  arrange(trophic_state) %>%
  spread(trophic_state,percent) %>%
  mutate(scale = "URIWW")

lagos_ts <- lagos_all %>% 
  select(station_name, trophic_state) %>% 
  unique() %>%
  filter(!is.na(trophic_state)) %>%
  group_by(trophic_state) %>%
  summarize(percent = round(n()/nrow(.), 3)*100) %>%
  mutate(trophic_state = factor(trophic_state, 
                                levels = c("oligotrophic", "mesotrophic", 
                                           "eutrophic", "hypereutrophic"))) %>%
  arrange(trophic_state) %>%
  spread(trophic_state,percent) %>%
  mutate(scale = "LAGOSNE")

ts_table <- rbind(ww_ts, lagos_ts) %>%
  select(Source = scale, Oligotrophic = oligotrophic, Mesotrophic = mesotrophic,
         Eutrophic = eutrophic, Hypereutrophic = hypereutrophic) 

# Summary of lake context/morpho - response to Reviewer 2
lagos_dt <- lagosne_load()
all_lakes_context <- left_join(lagos_dt$locus, lagos_dt$lakes_limno) %>%
  left_join(lagos_dt$state) %>%
  left_join(lagos_dt$buffer500m.lulc) %>%
  select(lagoslakeid, lake_area_ha, maxdepth, state, 
         contains("nlcd2011_pct")) %>%
  mutate(Water = buffer500m_nlcd2011_pct_11,
         Developed = buffer500m_nlcd2011_pct_21 + buffer500m_nlcd2011_pct_22 +
           buffer500m_nlcd2011_pct_23,
         Barren = buffer500m_nlcd2011_pct_31, 
         Forest = buffer500m_nlcd2011_pct_41 + buffer500m_nlcd2011_pct_42 +
           buffer500m_nlcd2011_pct_43,
         Shrubland = buffer500m_nlcd2011_pct_52,
         Herbaceous = buffer500m_nlcd2011_pct_71,
         Agriculture = buffer500m_nlcd2011_pct_81 + buffer500m_nlcd2011_pct_82,
         Wetlands = buffer500m_nlcd2011_pct_90 + buffer500m_nlcd2011_pct_95) %>%
  select(lagoslakeid, state, lake_area_ha, maxdepth, Water, Developed, Barren, 
         Forest, Shrubland, Herbaceous, Agriculture, Wetlands) %>%
  gather("param", "value", lake_area_ha:Wetlands) %>%
  group_by(lagoslakeid, state, param) %>%
  summarize(value = max(value)) %>%
  filter(!is.na(value))

all_context_summ <- all_lakes_context %>% 
  group_by(param) %>%
  summarize(min = min(value),
            `25th percentile` = quantile(value, 0.25),
            med = median(value),
            avg = mean(value),
            `75th percentile` = quantile(value, 0.75),
            max = max(value)) %>%
  filter(param %in% c("Agriculture", "Forest", "Developed", "lake_area_ha", 
                      "maxdepth")) %>%
  mutate(param = case_when(param == "lake_area_ha" ~
                             "Lake Area (ha)", 
                           param == "maxdepth" ~
                             "Maximum Depth (m)",
                           param == "Agriculture" ~
                             "Agriculture (%)",
                           param == "Forest" ~
                             "Forest (%)",
                           param == "Developed" ~
                             "Developed (%)",
                           TRUE ~ param),
         units = c("%", "%", "%", "ha", "m")) %>%
  mutate(Source = "LAGOSNE") %>%
  select(Source, param, avg) %>%
  spread(param, avg)
  

ww_context_summ <- all_lakes_context %>% 
  filter(state == "RI") %>% 
  group_by(param) %>%
  summarize(min = min(value),
            `25th percentile` = quantile(value, 0.25),
            med = median(value),
            avg = mean(value),
            `75th percentile` = quantile(value, 0.75),
            max = max(value)) %>%
  filter(param %in% c("Agriculture", "Forest", "Developed", "lake_area_ha", 
                      "maxdepth")) %>%
  mutate(param = case_when(param == "lake_area_ha" ~
                             "Lake Area (ha)", 
                           param == "maxdepth" ~
                             "Maximum Depth (m)",
                           param == "Agriculture" ~
                             "Agriculture (%)",
                           param == "Forest" ~
                             "Forest (%)",
                           param == "Developed" ~
                             "Developed (%)",
                           TRUE ~ param),
         units = c("%", "%", "%", "ha", "m")) %>%
  mutate(Source = "URIWW") %>%
  select(Source, param, avg) %>%
  spread(param, avg) 
            
context_summ <- rbind(ww_context_summ, all_context_summ)
  
  
