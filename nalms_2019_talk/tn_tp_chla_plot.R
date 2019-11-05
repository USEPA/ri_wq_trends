library(dplyr)
library(readr)
library(ggplot2)
library(hrbrthemes)

nla_2007_url <- "https://www.epa.gov/sites/production/files/2014-10/nla2007_chemical_conditionestimates_20091123.csv"
nla_2012_url <- "https://www.epa.gov/sites/production/files/2017-02/nla12_keyvariables_data.csv"

nla_2007 <- read_csv(nla_2007_url)

nla_2012 <- read_csv(nla_2012_url)

nla_2007_nutr_chla <- nla_2007 %>%
  mutate(year = 2007) %>%
  rename_all(tolower) %>%
  select(year, state = st, site_id, total_p = ptl, 
         total_n = ntl, chla)

nla_2012_nutr_chla <- nla_2012 %>%
  mutate(year = 2012) %>%
  rename_all(tolower) %>%
  select(year, state, site_id, total_p = ptl_result, 
         total_n = ntl_result, chla = chlx_result) %>%
  mutate(total_n = total_n * 1000)

nla <- rbind(nla_2007_nutr_chla, nla_2012_nutr_chla)  

tn_tp_chla_gg <- nla %>%
  group_by(year, state) %>%
  summarize(mean_total_p = mean(total_p, na.rm = TRUE),
            mean_total_n = mean(total_n, na.rm = TRUE),
            mean_chla = mean(chla, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = log1p(mean_total_p), 
             y = log1p(mean_total_n),
             color = log1p(mean_chla), 
             size = log1p(mean_chla))) +
  geom_point() +
  scale_color_continuous(low = "royalblue", high = "springgreen4", name = "Log10 Chlorophyll") +
  scale_size_continuous(range = c(0.1, 10), guide = FALSE) +
  labs(x = "Log10 Total Phosphorus", 
       y = "Log10 Total Nitrogen",
       title = "NLA 2007 and 2012 - State Averages") +
  theme_ipsum_rc(base_size = 14, axis_title_size = 14,
                 axis_text_size = 14)
  
ggsave("nalms_2019_talk/figures/tn_tp_chla_plot.jpg", tn_tp_chla_gg, width = 12, height = 9, dpi = 300)  
  
