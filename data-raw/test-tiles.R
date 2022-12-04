# See https://walker-data.com/mapboxapi/articles/creating-tiles.html

remotes::install_cran("mapboxapi")
library(tidyverse)
library(mapboxapi)
library(leaflet)
source("R/utils.R")
tippecanoe(input = "overline.geojson", output = "overline.mbtiles")
# mb_access_token(token = "xxx")
upload_tiles("overline.mbtiles", username = "robinlovelacep")
# clb9aw5ph14u823qe3foh98z1

# Test with Leaflet
leaflet() %>%
  addTiles()

scotland_lads = readRDS("inputdata/lads_scotland.Rds")
edinburgh_boundary = scotland_lads %>% 
  filter(str_detect(lau118nm, "Edin"))
bb = as.numeric(edinburgh_bb)
edinburgh_bb = sf::st_bbox(edinburgh_boundary)

m1 = leaflet() %>%
  addTiles() %>% 
  addPolylines(data = scotland_lads, color = "white", opacity = 0.2, weight = 2) %>% 
  addMapboxTiles(username = "robinlovelacep", layerId = "clb9bp00i0plf21ouq9hlds5m",
                 style_url = "mapbox://styles/robinlovelacep/clb9b4nbh001v14ticxn7p6sn") %>% 
  flyToBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
m1

htmlwidgets::saveWidget(m1, "m1.html")
# piggyback::pb_upload(file = "m1.html", tag = "v1")
gh_release_upload(file = "m1.html", tag = "v1")
