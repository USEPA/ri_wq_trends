library(dplyr)
library(tidyr)
devtools::install_github("cont-limno/LAGOSNE")

library(ggplot2)
dt <- lagosne_load("1.087.1")
lagosne <- lagosne_select(table = "epi_nutr", 
                          vars = c("lagoslakeid", "sampleyear", "chla","tp",
                                   "tn","secchi"))

x<-lagosne %>%
  gather(param, measurement, chla:secchi) %>%
  filter(param == "chla", !is.na(measurement), sampleyear > 1990) %>%
  group_by(sampleyear) %>%
  summarize(bloom_count = sum(measurement > 23),
            num = n()) %>%
  mutate(bloom_perc = bloom_count/num)
plot(x$sampleyear, x$bloom_perc)
cor.test(x$sampleyear, x$bloom_perc, method = "kendall")

lagosne_scaled <- lagosne %>%
  gather(param, measurement, chla:secchi) %>%
  group_by(lagoslakeid, param) %>%
  mutate(measurement_scaled = scale(measurement),
         measurement_anomaly = measurement - mean(measurement, na.rm = TRUE)) %>%
  filter(!is.na(measurement_scaled)) %>%
  ungroup()

lagosne_gg <- lagosne_scaled %>%
  group_by(sampleyear, param) %>%
  summarize(mn_scaled_measurement = mean(measurement_scaled),
            mn_anomaly_measurement = mean(measurement_anomaly)) %>%
  filter(sampleyear >= 1990) %>%
  ggplot(aes(x = sampleyear, y = mn_anomaly_measurement)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~param)
lagosne_gg
ggsave("figures/yearly_scaled_trend_lagosne.jpg",lagosne_gg, width = 8, 
       height = 10, units = "in")
