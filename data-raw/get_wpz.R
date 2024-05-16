sf::sf_use_s2(FALSE)
library(tidyverse)

# IZ zones 2011 -----------------------------------------------------------

lads_scot = readRDS("inputdata/lads_scotland.Rds")
boundary_edinburgh = lads_scot |>
  filter(str_detect(lau118nm, "Edinb"))

od_data = readRDS("inputdata/od_izo.Rds")
zones_national = readRDS("inputdata/iz_scotlands_uk.Rds")
mapview::mapview(zones_national[1, ]) # v. detailed
zones_national = rmapshaper::ms_simplify(zones_national, keep = 0.03)
mapview::mapview(zones_national[1, ]) # less detailed
zones_national = zones_national |> sf::st_transform("EPSG:4326")
saveRDS(zones_national, "inputdata/zones_national_simple.Rds")
summary(zones_national$InterZone %in% od_data$geo_code1)
zones_national_on_surface = zones_national |> sf::st_point_on_surface()
points_edinburgh = zones_national_on_surface[boundary_edinburgh, ]
zones_edinburgh = zones_national |>
  filter(InterZone %in% points_edinburgh$InterZone)
sf::write_sf(zones_edinburgh, "data-raw/zones_edinburgh.geojson", delete_dsn = TRUE)

# get wpz -----------------------------------------------------------------


source("R/download.R")
dl_nrs()
unzip("workplaces-zones-2011-scotland-centroids.zip")
wpz_centroids = sf::read_sf("WorkplacesZones2011ScotlandCentroids.shp")
plot(wpz_centroids$geometry)
saveRDS(wpz_centroids, "inputdata/wpz_centroids.Rds")

zones_scotland = readRDS("inputdata/zones_national_simple.Rds")
nrow(wpz_centroids) / nrow(zones_scotland) # 4 workplace destinations for every zone..
wpz_centroids = sf::st_transform(wpz_centroids, "EPSG:4326")
zones_with_wpz = zones_scotland[wpz_centroids, ]
nrow(zones_scotland) - nrow(zones_with_wpz) # 13 zones lack wpz centroid
# # Fails:
zones_without_wpz = zones_scotland |>
  filter(!InterZone %in% zones_with_wpz$InterZone)
mapview::mapview(zones_without_wpz) # all of them are small -> geo centroid is fine
zones_without_wpz_centroids = sf::st_point_on_surface(zones_without_wpz)

sf::write_sf(wpz_centroids, "inputdata/wpz_centroids.geojson", delete_dsn = TRUE)
workplaces_simple = rbind(
  wpz_centroids |> transmute(code = WPZ),
  zones_without_wpz_centroids |> transmute(code = paste0("d", InterZone))
)
saveRDS(workplaces_simple, "inputdata/workplaces_simple.Rds")
workplaces_simple = workplaces_simple[zones, ]
sf::write_sf(workplaces_simple, "data-raw/workplaces_simple_edinburgh.geojson", delete_dsn = TRUE)

# output areas ------------------------------------------------------------

# https://www.nrscotland.gov.uk/statistics-and-data/geography/our-products/census-datasets/2011-census/2011-boundaries
u = "https://www.nrscotland.gov.uk/files/geography/output-area-2011-pwc.zip"
f = basename(u)
download.file(u, f)
unzip(f)
oas = sf::read_sf("OutputArea2011_PWC.shp")
oas = oas |>
  transmute(code = code) |>
  sf::st_transform("EPSG:4326")
saveRDS(oas, "inputdata/oas.Rds")
oas = oas[zones, ]
sf::write_sf(oas, "data-raw/oas.geojson")
