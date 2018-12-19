#' script to generate figures
#' load up functions and packages
source(here::here("R/functions.R"))

ww_data <- read_csv(here("data/ww_lake_trend_data.csv"))

chla_gg <- ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg( "chla", yvar = "measurement_scale", 
                       y = "Average Yearly Scaled Chlorophyll", x = "Year",
               write = here("data/chla_data.csv"))
ggsave(here("figures/ww_chla_trends.jpg"), chla_gg, width = 8, height = 6, 
       units = "in", dpi = 600)

tn_gg <-  ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("total_n", yvar = "measurement_scale", 
                      y = "Average Yearly Scaled Total Nitrogen", x = "Year",
              write = here("data/total_n_data.csv"))
ggsave(here("figures/ww_tn_trends.jpg"), tn_gg, width = 8, height = 6, 
       units = "in", dpi = 600)

tp_gg <-  ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("total_p", yvar = "measurement_scale", 
                      y = "Average Yearly Scaled Total Phosphorus", x = "Year",
                      write = here("data/total_p_data.csv"))
ggsave(here("figures/ww_tp_trends.jpg"), tp_gg, width = 8, height = 6, 
       units = "in", dpi = 600)

temp_gg <- ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("temp", yvar = "measurement_scale", 
                       y = "Average Yearly Scaled Temperature", x = "Year",
              write = here("data/temp_data.csv"))
ggsave(here("figures/ww_temp_trends.jpg"), temp_gg, width = 8, height = 6, 
       units = "in", dpi = 600)

lagos_data <- read_csv(here("data/lagos_lake_trend_data.csv"))

lagos_chla_gg <- wq_trend_gg(lagos_data, "chla", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Chlorophyll", x = "Year")
ggsave(here("figures/lagos_chla_trends.jpg"), lagos_chla_gg, width = 8, height = 6, 
       units = "in", dpi = 600)

lagos_tn_gg <- wq_trend_gg(lagos_data, "total_n", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Total Nitrogen", x = "Year")
ggsave(here("figures/lagos_tn_trends.jpg"), lagos_tn_gg, width = 8, height = 6, 
       units = "in", dpi = 600)

lagos_tp_gg <- wq_trend_gg(lagos_data, "total_p", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Total Phosphorus", x = "Year")
ggsave(here("figures/lagos_tp_trends.jpg"), lagos_tp_gg, width = 8, height = 6, 
       units = "in", dpi = 600)

# WW Map
ri <- us_states(resolution = "high", states = "Rhode Island")
avail_data <- read_csv(here("data/ww_avail_data_stations.csv"))
ww_pts <- ww_data %>%
  filter(state == "RI") %>%
  filter(!is.na(lon_dd)) %>%
  select(station_name, lat_dd, lon_dd) %>%
  unique() %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), 
        crs = st_crs(ri), agr = "constant") %>%
  left_join(avail_data) %>%
  #yanks out all sites with less than 10 years data.
  filter(!is.na(params_avail)) %>%
  mutate(what_params = case_when(params_avail != "chla, total_p, total_n, temp" ~
                              "Subset of parameters",
                            TRUE ~ "All parameters"))
  
ww_map <- ggplot(st_geometry(ri)) + 
  geom_sf(size = 0.75, fill = "grey95", show.legend = FALSE) +
  geom_sf(data = ww_pts, aes(color = what_params), size = 2, show.legend = "point") +
  scale_color_manual(values = c("red3","darkblue")) +
  scale_x_continuous(breaks = seq(-72.0, -71.0, by  = 0.2)) +
  scale_y_continuous(breaks = seq(41.0, 42.0, by = 0.2)) +
  theme_ipsum() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave(here("figures/ww_map.jpg"), plot = ww_map, width = 8, height = 6.5, 
         units = "in", dpi = 600)

