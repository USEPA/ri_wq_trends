library(tidyverse)
library(readxl)
library(lubridate)
library(here)
ww_all <- read_csv("data/ww_all.csv", guess_max = 200000)
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

ww_all_clean <- ww_all %>%
  mutate(year = year(ymd(Date)),
         month = month(ymd(Date)),
         day = day(ymd(Date)),
         `Station Name` = case_when(str_length(`Station Name`) == 4 ~ 
                                      str_replace(`Station Name`, "WW", "WW0"),
                                    TRUE ~ `Station Name`)) %>% 
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
  filter(!is.na(measurement)) %>%
  mutate(measurement = case_when(param == "total_n" ~ measurement * 1000,
                                 T ~ measurement)) %>% # Convert TN to ug/l
  left_join(ww_sites) %>%
  filter(WB_Type == "Lake or Pond" |
           WB_Type == "Reservoir") %>%
  select(station_name:location, site_descr = Site_DESCR, town = Town, county = COUNTY, state = State, 
         lon_dd = LON_DD, lat_dd = LAT_DD, huc_12 = HUC_12, 
         huc_10_name = HUC_10_NAME, huc_12_name = HUC_12_NAME)

mash <- ww_all_clean %>%
  filter(site_descr == "Mashapaug Pond")

mash %>%
  #filter(year == 2016) %>%
  #filter(month == 11) %>%
  filter(param == "chla") %>%
  ggplot(aes(x = measurement)) +
  geom_density()

