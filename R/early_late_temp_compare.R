source(here::here("R/functions.R"))

ww_all <- read_csv(here("data/ww_all.csv"), 
                   guess_max = 300000)
ww_all_temp_month <- ww_all %>%
  select(date = "Date", param = "Parameter", conc = "Concentration") %>%
  mutate(month = month(date),
         year = year(date),
         early_late = case_when(year < 1996 & year >= 1993 ~
                                  "early",
                                year > 2013 ~
                                  "late")) %>%
  filter(param == "Temperature - 00011",
         month == 5 | month == 10,
         !is.na(early_late)) %>%
  group_by(early_late, month) %>%
  summarize(temp_mean = mean(conc, na.rm = TRUE)) %>%
  arrange(month)
