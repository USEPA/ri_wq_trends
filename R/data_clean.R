# load functions and packages
source(here::here("R/functions.R"))

# read in data
ww_all <- read_csv(here("data/ww_all.csv"), guess_max = 300000)

# read in sites
ww_sites <- read_excel(here("data/RIDOH Supporting Files.xlsx"), "Sites") %>%
  mutate(station_name = case_when(str_length(`WW_Station`) == 4 ~ 
                                                str_replace(`WW_Station`, "WW", "WW0"),
                                              TRUE ~ `WW_Station`))

params <- c("Temperature - 00011",
            "Secchi Depth - 00069",
            "Phosphorus, Total - 00665",
            "Nitrogen, Total (unfiltered) - 00600",
            "Enterococci - 31639",
            "Dissolved Oxygen - 00300",
            "Chlorophyll a, water, fluorometric method, corrected for pheophytin - 32209",
            "pH - 00400",
            "Chloride - 00940")

# read in missing nutrient data

missing_nutrients_1 <- read_csv(here("data/WW_96_nutrients_nds.csv")) %>%
  rbind(read_csv(here("data/WW_97_nutrients.csv"))) %>%
  rbind(read_csv(here("data/WW_98_nutrients.csv"))) %>%
  rbind(read_csv(here("data/WW_99_nutrients.csv"))) %>%
  rbind(read_csv(here("data/WW_00_nutrients.csv"))) %>%
  rbind(read_csv(here("data/WW_01_nutrients.csv"))) %>%
  mutate(Day = 15) # Need this to conform to existing format - none of these had a day so assuming middle of month

missing_nutrients <- missing_nutrients_1 %>%
  transmute(`Station Name` = `Station Name`,
            Date = ymd(paste(Year, Month, Day, sep="-")),
            Time = NA,
            `Sample Type` = NA,
            `Sample Media` = "Water",
            Depth = Depth,
            Parameter = Parameter,
            Concentration = Concentration,
            Unit = NA,
            `Qualifier Code` = NA,
            `Detection Limit` = 0, #zero becuase NA results getting filtered out
            `Detection Limit Unit` = NA,
            `Quantitation Level` = NA,
            `Quantitation Level Unit` = NA,
            `Lab Name` = NA,
            `Analytical Method Number` = NA,
            Comments = "Day assumed to be 15th, missing for original data",
            Location = Location) #%>%
  #filter(Concentration != "ND") %>%
  #mutate(Concentration = as.numeric(Concentration))

missing_nutrients <- missing_nutrients %>%
  mutate(Concentration = as.numeric(Concentration))

ww_all <- rbind(missing_nutrients, ww_all)

# strip down to data for paper
ww_lake_trend_data <- ww_all %>%
  mutate(year = year(ymd(Date)),
         month = month(ymd(Date)),
         day = day(ymd(Date)),
         `Station Name` = case_when(str_length(`Station Name`) == 4 ~ 
                                      str_replace(`Station Name`, "WW", "WW0"),
                                    TRUE ~ `Station Name`)) %>% #Fixes 4 digit ids
  #filter(`Station Name` %in% filter_year(., 25)) %>% # Get stations with 20+ years - moved to plots...
  filter(year >= 1993) %>% #only get data for 1993 and beyond as that is earliest TN data for WW.  Make sure all ranges match
  #filter(year <= 2013) %>% #only get data from 2013 and prior to match Oliver
  filter(Parameter %in% params) %>% #Filter out subset of parameters
  #filter(Concentration != `Detection Limit`) %>% #filters out measurements at detect limit (temporary)
  mutate(Parameter = case_when(Parameter == params[1] ~ "temp", #NAMES!!!!!
                               Parameter == params[2] ~ "secchi",
                               Parameter == params[3] ~ "total_p",
                               Parameter == params[4] ~ "total_n",
                               Parameter == params[5] ~ "enterococci",
                               Parameter == params[6] ~ "do",
                               Parameter == params[8] ~ "ph",
                               Parameter == params[9] ~ "chloride",
                               Parameter == params[7] ~ "chla",
                               TRUE ~ Parameter)) %>%
  filter(Parameter %in% c("temp","total_p", "total_n","chla")) %>%
  filter(`Sample Media` == "Water")
                               

# Look at Detction Limit
det_limit <- ww_lake_trend_data %>% 
  mutate(year = lubridate::year(Date)) %>% 
  select(station_name = `Station Name`, year, param = Parameter, 
         det_limit = `Detection Limit`)

# Several typos in detection limits.  This assumes that the most common limit
# Listed per year and parameter is what is should have been.
det_limits <- det_limit %>%
  group_by(year,param, det_limit) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year, param) %>%
  mutate(max_count = max(count)) %>%
  ungroup() %>%
  filter(count == max_count) %>%
  mutate(det_limit = case_when(param == "total_n" ~
                                 det_limit * 1000,
                               TRUE ~ det_limit)) %>%
  select(year, param, det_limit)
  

# Final Clean
ww_lake_trend_data <- ww_lake_trend_data %>%
  select(station_name = `Station Name`, year, month, day, depth = Depth, 
         location = Location, param = Parameter, 
         measurement = Concentration) %>%
  filter(depth <=2 | is.na(depth)) %>%
  filter(!is.na(measurement)) %>%
  filter(!is.na(station_name)) %>%
  mutate(measurement = case_when(param == "total_n" ~ 
                                   measurement * 1000,
                                 T ~ measurement)) %>% # Convert TN to ug/l
  #filter(year >= 1990) %>%
  group_by(station_name, year, month,day,param,location) %>%
  summarize(mn_measurement = mean(measurement, na.rm = TRUE))%>%
  ungroup() 


# Enforce sig digits

ww_lake_trend_data <- ww_lake_trend_data %>%
  mutate(mn_measurement = case_when(param == "temp"  ~ 
                                      round(mn_measurement, 1),
                                    param == "chla" ~
                                      round(mn_measurement, 1),
                                    param == "total_p" ~
                                      round(mn_measurement, 0),
                                    param == "total_n" ~
                                      round(mn_measurement/5, 0)*5,
                                    TRUE ~ mn_measurement))
  
# Almy Pond, 2016-07-23, Sample likely contaminated with bottom sediment per notes in data
# Removed from analysis
idx <- ww_lake_trend_data$year == "2016" & ww_lake_trend_data$month == 7 & ww_lake_trend_data$day == 23 & ww_lake_trend_data$station_name == "WW199"
idx <- !idx
ww_lake_trend_data <- ww_lake_trend_data %>% 
  filter(idx)

# Reviewer 1 comment re: samples across the season
ww_lake_trend_data <- ww_lake_trend_data %>%
 filter_months(.)

# Calculate and add n:p 
# Need to make molar conversions here...
ww_np <- ww_lake_trend_data %>%
  filter(param == "total_n" | param == "total_p") %>% 
  spread(key = param, value = mn_measurement) %>%
  mutate(np_ratio = (total_n/total_p)*2.21) %>% #calc ratio and convert to molar
  filter(!is.na(np_ratio)) %>%
  filter(!is.infinite(np_ratio)) %>%
  gather(key = "param", value = "mn_measurement", np_ratio) %>%
  select(station_name, year, month, day, param, location, mn_measurement)

ww_lake_trend_data <- select(ww_lake_trend_data, -may_jun, -jul_aug, -sep_oct) %>%
  rbind(ww_np)

write_csv(ww_lake_trend_data, here("data/ww_all_mostly_cleaned.csv"))

# Calculates station/year/param stats
ww_lake_trend_data <- ww_lake_trend_data %>%
  filter(month >= 5 & month <= 10) %>%
  group_by(station_name, year, param) %>%
  summarize(station_year_mean = mean(mn_measurement)) %>%
  ungroup() %>%
  group_by(station_name, param) %>%
  mutate(measurement_scale = scale(station_year_mean),
         measurement_anmly = station_year_mean - mean(station_year_mean),
         lt_mean = mean(station_year_mean),
         lt_sd = sd(station_year_mean),
         lt_n = n()) %>%
  ungroup() %>%
  left_join(ww_sites) %>%
  filter(WB_Type == "Lake or Pond" | WB_Type == "Reservoir") %>%
  select(station_name:lt_n, site_descr = Site_DESCR, 
         town = Town, county = COUNTY, state = State, lon_dd = LON_DD, 
         lat_dd = LAT_DD, huc_12 = HUC_12, huc_10_name = HUC_10_NAME, 
         huc_12_name = HUC_12_NAME)

# Enforcing sig digits again for station_year_mean, measurment_anmly, lt_mean, 
# and lt_sd
ww_lake_trend_data <- ww_lake_trend_data %>%
  mutate(station_year_mean = case_when(param == "temp"  ~ 
                                       round(station_year_mean, 1),
                                     param == "chla" ~
                                       round(station_year_mean, 1),
                                     param == "total_p" ~
                                       round(station_year_mean, 0),
                                     param == "total_n" ~
                                       round(station_year_mean/5, 0)*5,
                                     param == "np_ratio" ~
                                       round(station_year_mean, 0),
                                     TRUE ~ station_year_mean),
         measurement_anmly = case_when(param == "temp"  ~ 
                                         round(measurement_anmly, 1),
                                       param == "chla" ~
                                         round(measurement_anmly, 1),
                                       param == "total_p" ~
                                         round(measurement_anmly, 0),
                                       param == "total_n" ~
                                         round(measurement_anmly/5, 0)*5,
                                       param == "np_ratio" ~
                                         round(measurement_anmly, 0),
                                       TRUE ~ measurement_anmly),
         lt_mean = case_when(param == "temp"  ~ 
                               round(lt_mean, 1),
                             param == "chla" ~
                               round(lt_mean, 1),
                             param == "total_p" ~
                               round(lt_mean, 0),
                             param == "total_n" ~
                               round(lt_mean/5, 0)*5,
                             param == "np_ratio" ~
                               round(lt_mean, 0),
                             TRUE ~ lt_mean),
         lt_sd = case_when(param == "temp"  ~ 
                             round(lt_sd, 1),
                           param == "chla" ~
                             round(lt_sd, 1),
                          param == "total_p" ~
                             round(lt_sd, 0),
                          param == "total_n" ~
                             round(lt_sd/5, 0)*5,
                          param == "np_ratio" ~
                            round(lt_sd, 0),
                          TRUE ~ lt_sd))

ww_lake_trend_data <- ww_lake_trend_data %>%
  left_join(det_limits)

# In response to reviewer comment 1-2: remove sites/param that may have only been 
# early or late in 1993-2016
ww_lake_trend_data <- ww_lake_trend_data %>%
  filter_early_late(2004)

# Add TSIs
ww_lake_trend_data <- ww_lake_trend_data %>%
  mutate(tsi = case_when(param == "chla" ~
                           tsi(station_year_mean, "chla"),
                         param == "total_p" ~
                           tsi(station_year_mean, "tp"),
                         param == "total_n" ~
                           tsi(station_year_mean, "tn"),
                         TRUE ~ NA_real_),
         trophic_state = case_when(tsi <= 40 ~
                                     "oligotrophic",
                                   tsi > 40 & tsi <= 50 ~
                                     "mesotrophic",
                                   tsi > 50 & tsi <= 60 ~
                                     "eutrophic",
                                   tsi > 60 ~
                                     "hypereutrophic",
                                   TRUE ~
                                     NA_character_))
write_csv(ww_lake_trend_data, here("data/ww_lake_trend_data.csv"))

# Prep LAGOS data
lagosne_get(dest_folder = LAGOSNE:::lagos_path())
lagos_data <- lagosne_select(table = "epi_nutr", 
                             vars = c("lagoslakeid", "programname","sampledate",
                                      "chla","tp","tn","secchi")) %>%
  mutate(year = as(year(ymd(sampledate)), "integer"),
         month = as(month(ymd(sampledate)), "integer"),
         day = as(day(ymd(sampledate)), "integer"),
         `Station Name` = lagoslakeid, np_ratio = (tn/tp)*2.21) %>%
  filter(year >= 1993) %>% #only get data for 1993 and beyond as that is earliest TN data for WW.  Make sure all ranges match
  #filter(year <= 2013) %>% #only get data from 2013 and prior to match Oliver
  select(`Station Name`, programname, year, month, day, chla, total_p = tp, total_n = tn, np_ratio) %>%
  gather(param, measurement, chla:np_ratio) %>%
  filter(!is.na(measurement)) %>%
  filter(!is.infinite(measurement)) %>%
  #filter(year >= 1990) %>%
  #filter(`Station Name` %in% filter_year(., 20)) %>% #moved to plots and 10 years is min...
  filter(month >= 5 & month <= 10) %>%
  select(station_name = `Station Name`,year, month, day, param, measurement) 

# Reviewer 1 comment re: samples across the season
lagos_data <- lagos_data %>%
  filter_months(.)

lagos_data <- lagos_data %>%
  #This should take care of pseudoreplication by using the per site/year means
  #results in n for years being equal to number of sites per year
  group_by(station_name, year, param) %>%
  summarize(station_year_mean = mean(measurement)) %>%
  ungroup() %>%
  group_by(station_name, param) %>%
  mutate(measurement_scale = scale(station_year_mean),
         measurement_anmly = station_year_mean - mean(station_year_mean),
         lt_mean = mean(station_year_mean),
         lt_sd = sd(station_year_mean),
         lt_n = n())

# Enforcing WW sig digits here as well
lagos_data <- lagos_data %>%
  mutate(station_year_mean = case_when(param == "temp"  ~ 
                                         round(station_year_mean, 1),
                                       param == "chla" ~
                                         round(station_year_mean, 1),
                                       param == "total_p" ~
                                         round(station_year_mean, 0),
                                       param == "total_n" ~
                                         round(station_year_mean/5, 0)*5,
                                       param == "np_ratio" ~
                                         round(station_year_mean, 0),
                                       TRUE ~ station_year_mean),
         measurement_anmly = case_when(param == "temp"  ~ 
                                         round(measurement_anmly, 1),
                                       param == "chla" ~
                                         round(measurement_anmly, 1),
                                       param == "total_p" ~
                                         round(measurement_anmly, 0),
                                       param == "total_n" ~
                                         round(measurement_anmly/5, 0)*5,
                                       param == "np_ratio" ~
                                         round(measurement_anmly, 0),
                                       TRUE ~ measurement_anmly),
         lt_mean = case_when(param == "temp"  ~ 
                               round(lt_mean, 1),
                             param == "chla" ~
                               round(lt_mean, 1),
                             param == "total_p" ~
                               round(lt_mean, 0),
                             param == "total_n" ~
                               round(lt_mean/5, 0)*5,
                             param == "np_ratio" ~
                               round(lt_mean, 0),
                             TRUE ~ lt_mean),
         lt_sd = case_when(param == "temp"  ~ 
                             round(lt_sd, 1),
                           param == "chla" ~
                             round(lt_sd, 1),
                           param == "total_p" ~
                             round(lt_sd, 0),
                           param == "total_n" ~
                             round(lt_sd/5, 0)*5,
                           param == "np_ratio" ~
                             round(lt_sd, 0),
                           TRUE ~ lt_sd))

# In response to reviewer comment 1-2: remove sites/param that may have only been 
# early or late in 1993-2016
lagos_data <- lagos_data %>%
  filter_early_late(2002)

lagos_data <- lagos_data %>%
  mutate(tsi = case_when(param == "chla" ~
                           tsi(station_year_mean, "chla"),
                         param == "total_p" ~
                           tsi(station_year_mean, "tp"),
                         param == "total_n" ~
                           tsi(station_year_mean, "tn"),
                         TRUE ~ NA_real_),
         trophic_state = case_when(tsi <= 40 ~
                                     "oligotrophic",
                                   tsi > 40 & tsi <= 50 ~
                                     "mesotrophic",
                                   tsi > 50 & tsi <= 60 ~
                                     "eutrophic",
                                   tsi > 60 ~
                                     "hypereutrophic",
                                   TRUE ~
                                     NA_character_))

write_csv(lagos_data, here("data/lagos_lake_trend_data.csv"))
