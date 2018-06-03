library(dplyr)
library(tidyr)
#devtools::install_github("cont-limno/LAGOSNE")
library(LAGOSNE)
library(ggplot2)
dt <- lagosne_load("1.087.1")
lagosne <- lagosne_select(table = "epi_nutr", 
                          vars = c("lagoslakeid", "sampleyear", "chla","tp",
                                   "tn","secchi"))

lagosne_scaled <- lagosne %>%
  gather(param, measurement, chla:secchi) %>%
  group_by(lagoslakeid, param) %>%
  mutate(measurement_scaled = scale(measurement)) %>%
  filter(!is.na(measurement_scaled)) %>%
  ungroup()

lagosne_gg <- lagosne_scaled %>%
  group_by(sampleyear, param) %>%
  summarize(mn_scaled_measurment = mean(measurement_scaled)) %>%
  filter(sampleyear >= 1990) %>%
  ggplot(aes(x = sampleyear, y = mn_scaled_measurment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~param)
lagosne_gg
ggsave("figures/yearly_scaled_trend_lagosne.jpg",lagosne_gg, width = 8, 
       height = 10, units = "in")
