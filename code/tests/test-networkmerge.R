# Test merging network on a larger dataset, after basics fixed...

# See https://github.com/nptscot/npt/issues/64 for details

# Load the package
library(sf)
library(tidyverse)
remotes::install_dev("stplanr")
library(stplanr) # recent version

princes_street_example = sf::read_sf("https://github.com/nptscot/networkmerge/raw/main/data/rnet_princes_street.geojson")
princes_street_example # 1k rows and 5 columns

# Larger example:
unzip("./combined_network_tile.zip")
list.files("outputdata", pattern = "geojson")
# Date of the file:
file.info("outputdata/combined_network_tile.geojson")$mtime
# File size: 
fs::file_size("outputdata/combined_network_tile.geojson")
# Read in (use geojsonsf for speed):
rnet_to_visualise = sf::read_sf("outputdata/combined_network_tile.geojson")
names(rnet_to_visualise)

# Get a case study area around Edinburgh:
zones_edinburgh = zonebuilder::zb_zone("Edinburgh")
mapview::mapview(zones_edinburgh)
zones_3km = zones_edinburgh |>
  filter(circle_id < 3)

# 1km buffer:
zones_1km = zones_edinburgh |>
  filter(circle_id < 2)
rnet_1km_buffer = rnet_to_visualise[zones_1km, ]

rnet_3km_buffer = rnet_to_visualise[zones_3km, ]
plot(rnet_3km_buffer$geometry)

nrow(rnet_3km_buffer) ~20k
nrow(rnet_to_visualise) / nrow(rnet_3km_buffer) # 1.5k
