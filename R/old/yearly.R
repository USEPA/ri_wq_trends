source(here::here("R/functions.R"))

# Changes to make
# Single figures
# Add hrbrthemes
# Just anomaly, no scale since individual figures.  Keeps y-axis in units
# Color points based on sign - = blue and + = red
# On each figure add long-term mean
# kendal tau and p-value
# figures for RI: chla, temp, tn, tp
# figures for region: chla, tn, tp
# Map of samples

scale_df <- read_csv(here("data/ww_lake_trend_data.csv"))

scale_years <- scale_df %>%
  filter(param %in% c("chla","temp","total_n","total_p", "chloride", "secchi", "do","ph")) %>%
  group_by(year, param) %>%
  summarize(mn_scale = mean(measurement_scale),
            mn_meas = mean(mn_measurement)) %>%
  ungroup()

year_scale_gg <- scale_years %>%
  ggplot(aes(x=year,y=mn_scale)) +
  facet_grid(param ~ .) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~param)
ggsave(here("figures/yearly_scaled_trend.jpg"),year_scale_gg, width = 8, 
       height = 10, units = "in")


