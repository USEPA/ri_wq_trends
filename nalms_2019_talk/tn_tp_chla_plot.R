library(dplyr)
library(readr)
library(ggplot2)
library(hrbrthemes)

nla_2007_url <- "https://www.epa.gov/sites/production/files/2014-10/nla2007_chemical_conditionestimates_20091123.csv"
nla_2012_url <- "https://www.epa.gov/sites/production/files/2017-02/nla12_keyvariables_data.csv"

nla_2007 <- read_csv(nla_2007_url)

nla_2012 <- read_csv(nla_2012_url)

nla_temp <- read_csv("../lake_photic_zone/data/nla_base.csv") %>%
  select(nla_id, tmean_2m)

nla_2007_nutr_chla <- nla_2007 %>%
  mutate(year = 2007) %>%
  rename_all(tolower) %>%
  select(year, state = st, site_id, total_p = ptl, 
         total_n = ntl, chla) %>%
  left_join(nla_temp, by = c("site_id" = "nla_id")) %>%
  filter(tmean_2m < 75) %>% #some REALLY hot lakes - filtering out
  filter(total_p < 4000) #same site in NV with REALLY high TP 

nla_2012_nutr_chla <- nla_2012 %>%
  mutate(year = 2012) %>%
  rename_all(tolower) %>%
  select(year, state, site_id, total_p = ptl_result, 
         total_n = ntl_result, chla = chlx_result) %>%
  mutate(total_n = total_n * 1000) %>%
  left_join(nla_temp, by = c("site_id" = "nla_id"))  %>%
  filter(tmean_2m < 75) %>% #some REALLY hot lakes - filtering out
  filter(total_p < 4000) #same site in NV with REALLY high TP 

nla <- rbind(nla_2007_nutr_chla, nla_2012_nutr_chla)  

tn_tp_chla_gg <- nla %>%
  group_by(year, state) %>%
  summarize(mean_total_p = mean(total_p, na.rm = TRUE),
            mean_total_n = mean(total_n, na.rm = TRUE),
            mean_chla = mean(chla, na.rm = TRUE),
            mean_temp = mean(tmean_2m, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = mean_total_p, 
             y = mean_total_n,
             color = mean_chla, 
             size = mean_chla)) +
  geom_point() +
  scale_color_continuous(low = "royalblue", high = "springgreen4", name = "Chlorophyll") +
  scale_size_continuous(range = c(0.1, 10), guide = FALSE) +
  labs(x = "Total Phosphorus", 
       y = "Total Nitrogen",
       title = "NLA 2007 and 2012 - State Averages") +
  theme_ipsum_rc(base_size = 14, axis_title_size = 14,
                 axis_text_size = 14)
tn_tp_chla_gg  
ggsave("nalms_2019_talk/figures/tn_tp_chla_plot.jpg", tn_tp_chla_gg, width = 12, height = 9, dpi = 300)  

temp_chla_gg <- nla %>%
  group_by(year, state) %>%
  summarize(mean_temp = mean(tmean_2m, na.rm = TRUE),
            mean_chla = mean(chla, na.rm = TRUE)) %>%
  ggplot(aes(x = mean_temp, y = mean_chla)) +
  geom_point(size = 3) +
  labs(x = "Temperature", 
                    y = "Chlorophyll",
                    title = "NLA 2007 and 2012 - State Averages") +
  theme_ipsum_rc(base_size = 14, axis_title_size = 14,
                 axis_text_size = 14)
temp_chla_gg
ggsave("nalms_2019_talk/figures/temp_chla_plot.jpg", temp_chla_gg, width = 12, height = 9, dpi = 300) 
  
