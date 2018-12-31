#' script to generate figures
#' load up functions and packages
source(here::here("R/functions.R"))

ww_data <- read_csv(here("data/ww_lake_trend_data.csv"))

chla_gg <- ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg( "chla", yvar = "measurement_scale", 
                       y = "Average Yearly Scaled Chlorophyll", x = "Year",
               write = here("data/chla_data.csv"))
ggsave(here("figures/ww_chla_trends.jpg"), chla_gg, width = 5, height = 3.75, 
       units = "in", dpi = 600)

tn_gg <-  ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("total_n", yvar = "measurement_scale", 
                      y = "Average Yearly Scaled Total Nitrogen", x = "Year",
              write = here("data/total_n_data.csv"))
ggsave(here("figures/ww_tn_trends.jpg"), tn_gg, width = 5, height = 3.75, 
       units = "in", dpi = 600)

tp_gg <-  ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("total_p", yvar = "measurement_scale", 
                      y = "Average Yearly Scaled Total Phosphorus", x = "Year",
                      write = here("data/total_p_data.csv"))
ggsave(here("figures/ww_tp_trends.jpg"), tp_gg, width = 5, height = 3.75, 
       units = "in", dpi = 600)

temp_gg <- ww_data %>%
  filter(state == "RI") %>%
  wq_trend_gg("temp", yvar = "measurement_scale", 
                       y = "Average Yearly Scaled Temperature", x = "Year",
              write = here("data/temp_data.csv"))
ggsave(here("figures/ww_temp_trends.jpg"), temp_gg, width = 5, height = 3.75, 
       units = "in", dpi = 600)

lagos_data <- read_csv(here("data/lagos_lake_trend_data.csv"))

lagos_chla_gg <- wq_trend_gg(lagos_data, "chla", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Chlorophyll", x = "Year",
                             write = here("data/chla_data_lagos.csv"))
ggsave(here("figures/lagos_chla_trends.jpg"), lagos_chla_gg, width = 5, height = 3.75, 
       units = "in", dpi = 600)

lagos_tn_gg <- wq_trend_gg(lagos_data, "total_n", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Total Nitrogen", x = "Year",
                           write = here("data/total_n_data_lagos.csv"))
ggsave(here("figures/lagos_tn_trends.jpg"), lagos_tn_gg, width = 5, height = 3.75, 
       units = "in", dpi = 600)

lagos_tp_gg <- wq_trend_gg(lagos_data, "total_p", yvar = "measurement_scale", 
                             y = "Average Yearly Scaled Total Phosphorus", x = "Year",
                           write = here("data/total_n_data_lagos.csv"))
ggsave(here("figures/lagos_tp_trends.jpg"), lagos_tp_gg, width = 5, height = 3.75, 
       units = "in", dpi = 600)

