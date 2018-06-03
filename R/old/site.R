source(here::here("R/functions.R"))

scale_df <- read_csv(here("data/ww_lake_trend_data.csv"))
chla_lt_means <- scale_df %>%
  filter(param == "chla") %>%
  group_by(site_descr) %>%
  summarize(lt_mean = mean(mn_measurement))

sites_chla_gg <- scale_df %>%
  filter(param == "chla") %>%
  group_by(site_descr, year) %>%
  summarize(yrly_mean = mean(measurement_anmly)) %>% #in theory only one value for these anyway
  ggplot(aes(x = year, y = yrly_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site_descr) #+
  #geom_text(data=data.frame(x=1995, y=40, label=chla_lt_means$lt_mean),
  #          aes(x,y,label=label), inherit.aes=FALSE)
ggsave(here("figures/sites_chla_trends.jpg"), sites_chla_gg, height = 8, 
            width = 8, units = "in")

temp_lt_means <- scale_df %>%
  filter(param == "temp") %>%
  group_by(site_descr) %>%
  summarize(lt_mean = mean(mn_measurement))

sites_temp_gg <- scale_df %>%
  filter(param == "temp") %>%
  group_by(site_descr, year) %>%
  summarize(yrly_mean = mean(measurement_anmly)) %>%
  ggplot(aes(x = year, y = yrly_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site_descr)
ggsave(here("figures/sites_temp_trends.jpg"), sites_temp_gg, height = 8, 
       width = 8, units = "in")

tp_lt_means <- scale_df %>%
  filter(param == "total_p") %>%
  group_by(site_descr) %>%
  summarize(lt_mean = mean(mn_measurement))

sites_tp_gg <- scale_df %>%
  filter(param == "total_p") %>%
  filter(site_descr != "Belleville Pond-Upper") %>%
  group_by(site_descr, year) %>%
  summarize(yrly_mean = mean(measurement_anmly)) %>%
  ggplot(aes(x = year, y = yrly_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site_descr)
ggsave(here("figures/sites_tp_trends.jpg"), sites_tp_gg, height = 8, 
       width = 8, units = "in")

tn_lt_means <- scale_df %>%
  filter(param == "total_n") %>%
  group_by(site_descr) %>%
  summarize(lt_mean = mean(mn_measurement))

sites_tn_gg <- scale_df %>%
  filter(param == "total_n") %>%
  filter(site_descr != "Belleville Pond-Upper") %>%
  group_by(site_descr, year) %>%
  summarize(yrly_mean = mean(measurement_anmly)) %>%
  ggplot(aes(x = year, y = yrly_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site_descr)
ggsave(here("figures/sites_tn_trends.jpg"), sites_tn_gg, height = 8, 
       width = 8, units = "in")

lm_slope <- function(df){
  x<-lm(df[,3]~df[,2])
  coef(x)[2]
}

scale_df %>%
  filter(param == "chla") %>%
  group_by(site_descr, year) %>%
  summarize(yrly_mean = mean(measurement_scale)) %>%
  ungroup() %>%
  split(.$site_descr) %>%
  purrr::map(function(x) lm(yrly_mean ~ year, data = x)) %>%
  purrr::map(summary) %>%
  purrr::map_dbl(c("r.squared"))


