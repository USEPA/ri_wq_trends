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
  mutate(Day = 15)

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
            `Detection Limit` = 0, #In as zero becuase NA results in getting filtered out
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
         param = Parameter, measurement = Concentration, location = Location) %>%
  filter(depth <=2 | is.na(depth)) %>%
  filter(!is.na(measurement)) %>%
  mutate(measurement = case_when(param == "total_n" ~ measurement * 1000,
                                 T ~ measurement)) %>% # Convert TN to ug/l
  filter(year >= 1990) %>%
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
  mutate(measurement_scale = scale(mn_measurement),
         measurement_anmly = mn_measurement - mean(mn_measurement),
         lt_mean = mean(mn_measurement))
  

write_csv(ww_lake_trend_data, here("data/ww_lake_trend_data.csv"))

# Prep LAGOS data

lagos_data <- lagosne_select(table = "epi_nutr", 
                             vars = c("lagoslakeid", "programname","sampledate", "chla","tp",
                                      "tn","secchi")) %>%
  mutate(year = as(year(mdy(sampledate)), "integer"),
         month = as(month(mdy(sampledate)), "integer"),
         day = as(day(mdy(sampledate)), "integer"),
         `Station Name` = lagoslakeid) %>%
  select(`Station Name`, programname, year, month, day, chla, total_p = tp, total_n = tn) %>%
  gather(param, measurement, chla:total_n) %>%
  filter(!is.na(measurement)) %>% 
  filter(year >= 1990) %>%
  #filter(`Station Name` %in% filter_year(., 20)) %>% #moved to plots and 10 years is min...
  filter(month >= 5 & month <= 10) %>%
  select(station_name = `Station Name`,year, month, day, param, measurement) %>%
  group_by(station_name, param) %>%
  mutate(measurement_scale = scale(measurement),
         measurement_anmly = measurement - mean(measurement),
         lt_mean = mean(measurement))

write_csv(lagos_data, here("data/lagos_lake_trend_data.csv"))
  

         