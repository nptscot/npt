# Explore results for Edinburgh
library(targets)
library(tmap)
tmap_mode("view")
tar_load(rnet_commute_list)
ed = sf::read_sf("data-raw/zones_edinburgh.geojson")
rnet_ed = rnet_commute_list$balanced[ed, ]
tmap_mode("view")
tar_load(zones)
tar_load(subpoints_origins)
tar_load(subpoints_destinations)

tm_shape(rnet_ed) +
  tm_lines(lwd = "bicycle_go_dutch", scale = 19, col = "Quietness", palette = "magma") 
# tm_shape(ed) +
#   tm_borders(col = "red") +
#   tm_shape(subpoints_origins) +
#   tm_dots(col = "green") +
#   tm_shape(subpoints_destinations) +
#   tm_dots(col = "blue")
