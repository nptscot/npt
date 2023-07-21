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
