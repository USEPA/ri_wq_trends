us_pop <- state_laea %>%
  left_join(get_acs("state", "B01003_001")) %>%
  mutate(area = set_units(st_area(.), "mi^2"),
         pop_density = estimate/area) %>%
  arrange(desc(pop_density))
us_pop

ri_nlcd <- us_pop %>%
  filter(NAME == "Rhode Island") %>%
  get_nlcd(label = "ri", dataset = "Land_Cover")

ri_nlcd_data <- data.frame(table(raster::values(ri_nlcd))) %>%
  mutate(codes = as.numeric(as.character(Var1))) %>%
  left_join(nsink:::n_load_idx_lookup) %>%
  mutate(total_cells = sum(Freq)) %>%
  group_by(nlcd_class_1) %>%
  summarize(lulc_perc = round(sum(Freq)/max(total_cells)*100),0) %>%
  ungroup()
ri_nlcd_data
