# The aim of this script is to add open datasets to data-raw
library(targets)
library(tidyverse)
library(sf)

# Save study_area target
tar_load(study_area)
study_area$geometry
sf::write_sf(study_area, "data-raw/study_area.geojson")
tar_load(zones)
plot(zones$geometry)
zones = rmapshaper::ms_simplify(zones, keep = 0.04)
sf::write_sf(zones, "data-raw/DataZones.geojson")
fs::file_size("data-raw/DataZones.geojson") # 400 KB

tar_load(od_data)
tar_load(od_commute_subset)
waldo::compare(names(od_data), names(od_commute_subset))
od_data = od_data |>
  filter(paste(geo_code1, geo_code2) %in% paste(od_commute_subset$geo_code1, od_commute_subset$geo_code2))
summary(od_data)
# Randomise values:
add_normally_distributed_noise = function(x, sd = 5, runif_max = 10) {
  x_norm = x + rnorm(length(x), mean = 0, sd = sd * sd(x))
  x_runif = x_norm + runif(length(x), min = -runif_max, max = runif_max)
  # Any values below 0 are set to 0:
  x_runif[x_runif < 0] = 0  
  round(x_runif, 0)
}
od_data_open = od_data |>
  # Randomise values, but keep the same distribution:
    mutate(across(where(is.numeric), add_normally_distributed_noise)) 
summary(od_data_open)
# Save:
write_csv(od_data_open, "data-raw/od_data_dz_synthetic.csv")
