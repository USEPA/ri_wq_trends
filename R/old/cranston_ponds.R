source("R/functions.R")

# read in data
ww_all <- read_csv(here("data/ww_all.csv"),guess_max = 300000)

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


# strip down to data for paper
ww_lake_trend_data <- ww_all %>%
  mutate(year = year(mdy(Date)),
         month = month(mdy(Date)),
         day = day(mdy(Date)),
         `Station Name` = case_when(str_length(`Station Name`) == 4 ~ 
                                      str_replace(`Station Name`, "WW", "WW0"),
                                    TRUE ~ `Station Name`)) %>% #Fixes 4 digit ids
  #filter(`Station Name` %in% filter_year(., 25)) %>% # Get stations with 20+ years
  filter(Parameter %in% params) %>% #Filter out subset of parameters
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
         param = Parameter, measurement = Concentration, location = Location) %>%
  filter(depth <=2 | is.na(depth)) %>%
  filter(!is.na(measurement)) %>%
  #filter(year >= 2002) %>%
  group_by(station_name, year, month,day,param,location) %>%
  summarize(mn_measurement = mean(measurement))%>%
  ungroup() %>%
  left_join(ww_sites) %>%
  filter(WB_Type == "Lake or Pond" |
           WB_Type == "Reservoir") %>%
  select(station_name:location, site_descr = Site_DESCR, mn_measurement, town = Town, county = COUNTY, state = State, 
         lon_dd = LON_DD, lat_dd = LAT_DD, huc_12 = HUC_12, 
         huc_10_name = HUC_10_NAME, huc_12_name = HUC_12_NAME) %>%
  filter(month >= 5 & month <= 10) %>%
  group_by(station_name, param) %>%
  mutate(measurement_scale = scale(mn_measurement)) %>%
  ungroup()

cranston_ponds <- ww_lake_trend_data %>%
  filter(site_descr == "Blackamore Pond" |
           site_descr == "Spectacle Pond" |
           site_descr == "Mashapaug Pond")

cranston_ponds_yrly <- cranston_ponds %>%
  filter(year > 2010) %>%
  group_by(site_descr) %>%
  summarize(mn_chla = mean(mn_measurement),
            min_chla = min(mn_measurement),
            max_chla = max(mn_measurement))
cranston_ponds_yrly  
