#' script to generate figures
#' load up functions and packages
source(here::here("R/functions.R"))

ww_data <- read_csv(here("data/ww_lake_trend_data.csv"))

chla_gg <- ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg( "chla", yvar = "measurement_scale", 
                       y = "Average Yearly Scaled Chlorophyll", x = "Year",
               write = here("data/chla_data.csv"), title = "A.URI Watershed Watch Chlorophyll")
ggsave(here("figures/ww_chla_trends.jpg"), chla_gg[[1]], width = 7.5, height = 5.625, 
       units = "in", dpi = 600)

ww_chla_kt <- chla_gg[[2]]
ww_chla_df <- chla_gg[[3]]
ww_chla_rl <- chla_gg[[4]]

tn_gg <-  ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("total_n", yvar = "measurement_scale", 
                      y = "Average Yearly Scaled Total Nitrogen", x = "Year",
              write = here("data/total_n_data.csv"), , title = "A.URI Watershed Watch Total Nitrogen")
ggsave(here("figures/ww_tn_trends.jpg"), tn_gg[[1]], width = 7.5, height = 5.625, 
       units = "in", dpi = 600)

ww_tn_kt <- tn_gg[[2]]
ww_tn_df <- tn_gg[[3]]
ww_tn_rl <- tn_gg[[4]]

tp_gg <-  ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("total_p", yvar = "measurement_scale", 
                      y = "Average Yearly Scaled Total Phosphorus", x = "Year",
                      write = here("data/total_p_data.csv"), title = "A.URI Watershed Watch Total Phosphorus")
ggsave(here("figures/ww_tp_trends.jpg"), tp_gg[[1]], width = 7.5, height = 5.625, 
       units = "in", dpi = 600)

ww_tp_kt <- tp_gg[[2]]
ww_tp_df <- tp_gg[[3]]
ww_tp_rl <- tp_gg[[4]]

temp_gg <- ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("temp", yvar = "measurement_scale", 
                       y = "Average Yearly Scaled Temperature", x = "Year",
              write = here("data/temp_data.csv"), , title = "A.URI Watershed Watch Temperature")
ggsave(here("figures/ww_temp_trends.jpg"), temp_gg[[1]], width = 7.5, height = 5.625, 
       units = "in", dpi = 600)

ww_temp_kt <- temp_gg[[2]]
ww_temp_df <- temp_gg[[3]]
ww_temp_rl <- temp_gg[[4]]

np_gg <- ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("np_ratio", yvar = "measurement_scale", 
              y = "Average Yearly Scaled N:P", x = "Year",
              write = here("data/np_data.csv"), , title = "A.URI Watershed Watch Nitrogen:Phosphorus")
ggsave(here("figures/ww_np_trends.jpg"), np_gg[[1]], width = 7.5, 
       height = 5.625, 
       units = "in", dpi = 600)

ww_np_kt <- np_gg[[2]]
ww_np_df <- np_gg[[3]]
ww_np_rl <- np_gg[[4]]

lagos_data <- read_csv(here("data/lagos_lake_trend_data.csv"))

lagos_chla_gg <- wq_trend_gg(lagos_data, "chla", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Chlorophyll", x = "Year",
                             write = here("data/chla_data_lagos.csv"), title = "B. LAGOSNE Chlorophyll")
ggsave(here("figures/lagos_chla_trends.jpg"), lagos_chla_gg[[1]], width = 7.5, height = 5.625, 
       units = "in", dpi = 600)
lagos_chla_kt <- lagos_chla_gg[[2]]
lagos_chla_df <- lagos_chla_gg[[3]]
lagos_chla_rl <- lagos_chla_gg[[4]]

lagos_tn_gg <- wq_trend_gg(lagos_data, "total_n", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Total Nitrogen", x = "Year",
                           write = here("data/total_n_data_lagos.csv"), title = "B. LAGOSNE Total Nitrogen")
ggsave(here("figures/lagos_tn_trends.jpg"), lagos_tn_gg[[1]], width = 7.5, height = 5.625, 
       units = "in", dpi = 600)
lagos_tn_kt <- lagos_tn_gg[[2]]
lagos_tn_df <- lagos_tn_gg[[3]]
lagos_tn_rl <- lagos_tn_gg[[4]]


lagos_tp_gg <- wq_trend_gg(lagos_data, "total_p", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Total Phosphorus", x = "Year",
                           write = here("data/total_n_data_lagos.csv"), title = "B. LAGOSNE Total Phosphorus")
ggsave(here("figures/lagos_tp_trends.jpg"), lagos_tp_gg[[1]], width = 7.5, height = 5.625, 
       units = "in", dpi = 600)

lagos_tp_kt <- lagos_tp_gg[[2]]
lagos_tp_df <- lagos_tp_gg[[3]]
lagos_tp_rl <- lagos_tp_gg[[4]]

lagos_np_gg <- wq_trend_gg(lagos_data, "np_ratio", yvar = "measurement_scale", 
              y = "Average Yearly Scaled N:P", x = "Year",
              write = here("data/np_data_lagos.csv"), title = "B. LAGOSNE Nitrogen:Phosphorus")
ggsave(here("figures/lagos_np_trends.jpg"), lagos_np_gg[[1]], width = 7.5, 
       height = 5.625, 
       units = "in", dpi = 600)

lagos_np_kt <- lagos_np_gg[[2]]
lagos_np_df <- lagos_np_gg[[3]]
lagos_np_rl <- lagos_np_gg[[4]]

# Build Paired Plots
cowplot::plot_grid(chla_gg[[1]],lagos_chla_gg[[1]],align = "h")

avail_data <- read_csv(here("data/ww_avail_data_stations.csv"))
ri <- us_states(resolution = "high", states = "Rhode Island")
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
                                 TRUE ~ "All Parameters"))


ww_map <- ggplot(st_geometry(ri)) +
  geom_sf(size = 0.75, fill = "grey95", show.legend = FALSE) +
  geom_sf(data = ww_pts, aes(color = what_params, shape = what_params), size = 3.5, show.legend = "point") +
  scale_color_manual(values = c("grey40","grey60")) +
  scale_shape_manual(values = c(16,17)) +
  scale_x_continuous(breaks = seq(-72.0, -71.0, by  = 0.2)) +
  scale_y_continuous(breaks = seq(41.0, 42.0, by = 0.2)) +
  theme_ipsum() +
  theme(legend.position = "bottom", legend.title = element_blank())
ww_map %>%  
  ggsave(here("figures/ww_map.jpg"), ., width = 7.5, height = 9.8,
         units = "in", dpi = 600)

