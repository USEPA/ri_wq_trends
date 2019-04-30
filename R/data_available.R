#' script to generate figures
#' load up functions and packages
source(here::here("R/functions.R"))

ww_data <- read_csv(here("data/ww_lake_trend_data.csv"))

chla_10yr_sites <- ww_data %>%
  filter(state == "RI",
         param == "chla") %>%
  filter_year(10)

total_n_10yr_sites <- ww_data %>%
  filter(state == "RI",
         param == "total_n") %>%
  filter_year(10)

total_p_10yr_sites <- ww_data %>%
  filter(state == "RI",
         param == "total_p") %>%
  filter_year(10)

np_10yr_sites <- ww_data %>%
  filter(state == "RI",
         param == "np_ratio") %>%
  filter_year(10)
  
temp_10yr_sites <- ww_data %>%
  filter(state == "RI",
         param == "temp") %>%
  filter_year(10)

possible_sites <- unique(c(chla_10yr_sites,total_p_10yr_sites, 
                           total_n_10yr_sites, temp_10yr_sites, np_10yr_sites))
data_avail <- data_frame(station_name = possible_sites, 
                         chla_avail = possible_sites %in% chla_10yr_sites,
                         total_n_avail = possible_sites %in% total_n_10yr_sites, 
                         total_p_avail = possible_sites %in% total_p_10yr_sites,
                         np_avail = possible_sites %in% np_10yr_sites,
                         temp_avail = possible_sites %in% temp_10yr_sites) %>%
  mutate(params_avail = case_when(chla_avail == TRUE & total_n_avail == TRUE &
                                    total_p_avail == TRUE & np_avail == TRUE &
                                    temp_avail == TRUE ~ 
                                    "chla, total_p, total_n, np, temp",
                                  chla_avail == TRUE & total_n_avail == TRUE &
                                    total_p_avail == TRUE & temp_avail == TRUE ~ 
                                    "chla, total_p, total_n, temp",
                                  chla_avail == TRUE & total_n_avail == FALSE & 
                                    total_p_avail == FALSE & temp_avail == TRUE ~
                                      "chla, temp",
                                  chla_avail == FALSE & total_n_avail == TRUE &
                                    total_p_avail == TRUE & temp_avail == FALSE ~
                                    "total_p, total_n",
                                  chla_avail == TRUE & total_n_avail == FALSE &
                                    total_p_avail == TRUE & temp_avail == TRUE ~
                                    "chla, total_p, temp"))
ww_avail_data_locations <- ww_data %>%
  select(station_name, lon_dd, lat_dd) %>%
  unique() %>%
  right_join(data_avail) %>%
  select(station_name, params_avail) %>%
  write_csv(here("data/ww_avail_data_stations.csv"))
