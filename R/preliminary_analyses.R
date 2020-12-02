source(here::here("R/functions.R"))
library(magrittr)
ww_all<-read.csv(here("data/ww_all_mostly_cleaned.csv"), stringsAsFactors = FALSE)

ww_all_temp_gte25 <- ww_all %>%
  filter(param == "temp") %>%
  filter(mn_measurement > 25) %>%
  mutate(day_of_year = yday(ymd(paste(year, month, day, "-")))) %>%
  group_by(year) %>%
  summarize(min_first_day = min(day_of_year))

ww_all_temp_gte25 %>%
  filter(year <= 1995) %>%
  pull(min_first_day) %>%
  mean() -> early
as.Date("1995-01-01") -1 + early


ww_all_temp_gte25 %>%
  filter(year >= 2014) %>%
  pull(min_first_day) %>%
  mean() -> late
as.Date("2014-01-01") -1 + late
