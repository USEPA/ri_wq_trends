library(readr)
library(dplyr)
library(readxl)
library(lubridate)
library(here)
library(stringr)
library(sf)
library(tidyr)

ww_all <- read_csv("data/ww_all.csv")
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
ww <- ww_all %>%
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
                               TRUE ~ Parameter)) 
ww <- ww %>%
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
  left_join(ww_sites) %>%
  filter(complete.cases(cbind(LON_DD, LAT_DD)))

ww <- st_as_sf(x = ww,                         
         coords = c("LON_DD", "LAT_DD"),
         crs = 4386)

ww_2016 <- ww %>%
  filter(year >= 2016) %>%
  group_by(station_name, year, param) %>%
  summarize(mean_measurement = mean(measurement, na.rm = TRUE)) %>%
  spread(param, mean_measurement)
  

mapview::mapview(ww_2016)
