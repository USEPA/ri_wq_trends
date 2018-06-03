source(here::here("functions.R"))

scale_df <- read_csv(here("data/ww_lake_trend_data.csv"))

scale_months <- scale_df %>%
  group_by(year, month, param) %>%
  summarize(mn_scale = mean(measurement_scale, na.rm = TRUE)) %>%
  ungroup()

month_chla_gg <- scale_months %>%
  filter(param == "chla") %>%
  ggplot(aes(x = year, y = mn_scale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~month)
ggsave(here("figures/long_term_montly_chla.jpg"),month_chla_gg, width = 8, 
       height = 10, units = "in") 

month_tn_gg <- scale_months %>%
  filter(param == "total_n") %>%
  ggplot(aes(x = year, y = mn_scale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~month)
ggsave(here("figures/long_term_montly_tn.jpg"),month_tn_gg, width = 8, 
       height = 10, units = "in") 