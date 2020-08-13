library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(hrbrthemes)

set.seed(42)

site1 <- tibble(site=rep("site 1",10),
                year = 2011:2020,
                rep_1 = rnorm(10,10,2.5),
                rep_2 = rep_1*rnorm(10, 1, 0.05),
                rep_3 = rep_1*rnorm(10, 1, 0.05),
                inc_1 = seq(min(c(rep_1, rep_2, rep_3)),
                            max(c(rep_1, rep_2, rep_3)),
                            length.out = 10) *rnorm(10, 1, 0.05),
                inc_2 = inc_1*rnorm(10, 1, 0.05),
                inc_3 = inc_1*rnorm(10, 1, 0.05),
                dec_1 = seq(max(c(rep_1, rep_2, rep_3)),
                            min(c(rep_1, rep_2, rep_3)),
                            length.out = 10) *rnorm(10, 1, 0.05),
                dec_2 = dec_1*rnorm(10, 1, 0.05),
                dec_3 = dec_1*rnorm(10, 1, 0.05)) %>%
  pivot_longer(rep_1:dec_3, names_to = "replicate", values_to = "values") %>%
  mutate(trend = case_when(grepl("rep", replicate) ~
                             "flat",
                           grepl("inc", replicate) ~
                             "increase",
                           grepl("dec", replicate) ~
                             "decrease",
                           TRUE ~ NA_character_),
         replicate = str_extract(replicate, "[0-9]"))

site2 <- tibble(site=rep("site 2",6),
                year = 2011:2016,
                rep_1 = rnorm(6,3,1),
                rep_2 = rep_1 * rnorm(6, 1, 0.05),
                rep_3 = rep_1 * rnorm(6, 1, 0.05),
                inc_1 = seq(min(c(rep_1, rep_2, rep_3)),
                            max(c(rep_1, rep_2, rep_3)),
                            length.out = 6) * rnorm(6, 1, 0.05),
                inc_2 = inc_1*rnorm(6, 1, 0.05),
                inc_3 = inc_1*rnorm(6, 1, 0.05),
                dec_1 = seq(max(c(rep_1, rep_2, rep_3)),
                            min(c(rep_1, rep_2, rep_3)),
                            length.out = 6) *rnorm(6, 1, 0.05),
                dec_2 = dec_1*rnorm(6, 1, 0.05),
                dec_3 = dec_1*rnorm(6, 1, 0.05)) %>%
  pivot_longer(rep_1:dec_3, names_to = "replicate", values_to = "values") %>%
  mutate(trend = case_when(grepl("rep", replicate) ~
                             "flat",
                           grepl("inc", replicate) ~
                             "increase",
                           grepl("dec", replicate) ~
                             "decrease",
                           TRUE ~ NA_character_),
         replicate = str_extract(replicate, "[0-9]"))

site3 <- tibble(site=rep("site 3",6),
                year = 2015:2020,
                rep_1 = rnorm(6, 17, 1),
                rep_2 = rep_1*rnorm(6, 1, 0.05),
                rep_3 = rep_1*rnorm(6, 1, 0.05),
                inc_1 = seq(min(c(rep_1, rep_2, rep_3)),
                            max(c(rep_1, rep_2, rep_3)),
                            length.out = 6) *rnorm(6, 1, 0.05),
                inc_2 = inc_1*rnorm(6, 1, 0.05),
                inc_3 = inc_1*rnorm(6, 1, 0.05),
                dec_1 = seq(max(c(rep_1, rep_2, rep_3)),
                            min(c(rep_1, rep_2, rep_3)),
                            length.out = 6) *rnorm(6, 1, 0.05),
                dec_2 = dec_1*rnorm(6, 1, 0.05),
                dec_3 = dec_1*rnorm(6, 1, 0.05)) %>%
  pivot_longer(rep_1:dec_3, names_to = "replicate", values_to = "values") %>%
  mutate(trend = case_when(grepl("rep", replicate) ~
                             "flat",
                           grepl("inc", replicate) ~
                             "increase",
                           grepl("dec", replicate) ~
                             "decrease",
                           TRUE ~ NA_character_),
         replicate = str_extract(replicate, "[0-9]"))                

examp <- rbind(site1, site2, site3) %>%
  mutate(year = ymd(paste0(year,"01-01")))

site_gg <- examp %>%
  ggplot(aes(x = year, y = values)) +
  geom_point() + 
  facet_grid(trend ~ site) +
  theme_ipsum_rc()
site_gg


examp_site_summ <- examp %>% 
  group_by(site,trend) %>%
  mutate(lt_mean = mean(values)) %>%
  ungroup() %>%
  group_by(site, year, trend) %>%
  summarize(measured_value = mean(values),
            anomaly = mean(values-lt_mean)) %>%
  ungroup() %>%
  pivot_longer(measured_value:anomaly, "method")

examp_yr_summ <- examp_site_summ %>%
  group_by(year, trend, method) %>%
  summarize(values = mean(value)) %>%
  ungroup() 

simulated_trends_gg <- ggplot(examp_yr_summ, 
                              aes(x = year, y = values, color = method)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(trend ~ .) +
  scale_color_manual(values = c("darkblue", "darkred"), labels = c("Anomaly","Measured Values")) +
  labs(y = "Yearly Average Value") +
  theme_ipsum_rc()
simulated_trends_gg