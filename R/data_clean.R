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
  rbind(read_csv(here("data/WW_00_nutrients.csv"))) %>%
  mutate(Day = 15) # Need this to conform to existing format - none of these had a day so assuming middle of month

missing_nutrients <- missing_nutrients_1 %>%
  transmute(`Station Name` = `Station Name`,
            Date = ymd(paste(Year, Month, Day, sep="-")),
            Time = NA,
            `Sample Type` = NA,
            `Sample Media` = NA,
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
  filter(Concentration != `Detection Limit`) %>% #filters out measurements at detect limit (temporary)
  mutate(Parameter = case_when(Parameter == params[1] ~ "temp", #NAMES!!!!!
                               Parameter == params[2] ~ "secchi",
                               Parameter == params[3] ~ "total_p",
                               Parameter == params[4] ~ "total_n",
                               Parameter == params[5] ~ "enterococci",
                               Parameter == params[6] ~ "do",
                               Parameter == params[7] ~ "chla",
                               Parameter == params[8] ~ "ph",
                               Parameter == params[9] ~ "chloride")) %>% 
  select(station_name = `Station Name`, year, month, day, depth = Depth, 
         location = Location, param = Parameter, 
         measurement = Concentration) %>%
  filter(depth <=2 | is.na(depth)) %>%
  filter(!is.na(measurement)) %>%
  filter(!is.na(station_name)) %>%
  mutate(measurement = case_when(param == "total_n" ~ measurement * 1000,
                                 T ~ measurement)) %>% # Convert TN to ug/l
  #filter(year >= 1990) %>%
  group_by(station_name, year, month,day,param,location) %>%
  summarize(mn_measurement = mean(measurement, na.rm = TRUE))%>%
  ungroup() 

# Calculate and add n:p
ww_np <- ww_lake_trend_data %>%
  filter(param == "total_n" | param == "total_p") %>% 
  unique() %>% #repeats from somewhere...
  spread(key = param, value = mn_measurement) %>%
  mutate(np_ratio = total_n/total_p) %>%
  filter(!is.na(np_ratio)) %>%
  filter(!is.infinite(np_ratio)) %>%
  gather(key = "param", value = "mn_measurement", np_ratio) %>%
  select(station_name, year, month, day, param, location, mn_measurement)

ww_lake_trend_data <- ww_lake_trend_data %>%
  rbind(ww_np)

  
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
  left_join(ww_sites) %>%
  filter(WB_Type == "Lake or Pond" | WB_Type == "Reservoir") %>%
  select(station_name:lt_n, site_descr = Site_DESCR, 
         town = Town, county = COUNTY, state = State, lon_dd = LON_DD, 
         lat_dd = LAT_DD, huc_12 = HUC_12, huc_10_name = HUC_10_NAME, 
         huc_12_name = HUC_12_NAME)
  

write_csv(ww_lake_trend_data, here("data/ww_lake_trend_data.csv"))

# Prep LAGOS data
lagosne_get(dest_folder = LAGOSNE:::lagos_path())
lagos_data <- lagosne_select(table = "epi_nutr", 
                             vars = c("lagoslakeid", "programname","sampledate",
                                      "chla","tp","tn","secchi")) %>%
  mutate(year = as(year(ymd(sampledate)), "integer"),
         month = as(month(ymd(sampledate)), "integer"),
         day = as(day(ymd(sampledate)), "integer"),
         `Station Name` = lagoslakeid, np_ratio = tn/tp) %>%
  filter(year >= 1993) %>% #only get data for 1993 and beyond as that is earliest TN data for WW.  Make sure all ranges match
  #filter(year <= 2013) %>% #only get data from 2013 and prior to match Oliver
  select(`Station Name`, programname, year, month, day, chla, total_p = tp, total_n = tn, np_ratio) %>%
  gather(param, measurement, chla:np_ratio) %>%
  filter(!is.na(measurement)) %>%
  filter(!is.infinite(measurement)) %>%
  #filter(year >= 1990) %>%
  #filter(`Station Name` %in% filter_year(., 20)) %>% #moved to plots and 10 years is min...
  filter(month >= 5 & month <= 10) %>%
  select(station_name = `Station Name`,year, month, day, param, measurement) %>%
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

write_csv(lagos_data, here("data/lagos_lake_trend_data.csv"))
  

         