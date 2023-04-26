

library(tidyverse)
library(targets)

tar_glimpse()
tar_load(od_commute_subset)
od_commute_subset_minimal = od_commute_subset %>% 
  transmute(route_id)
sf::write_sf(od_commute_subset_minimal, "od_commute_subset_minimal.geojson")
fs::file_size("od_commute_subset_minimal.geojson")
od_commute_subset_minimal_100 = od_commute_subset_minimal %>% 
  slice(1:100)
cyclestreets::batch(od_commute_subset_minimal_100, wait = FALSE, username = "robinlovelace")
cyclestreets::batch(od_commute_subset_minimal, wait = FALSE, username = "robinlovelace")

system("gh release list")
system("gh release upload batch od_commute_subset_minimal.geojson")
