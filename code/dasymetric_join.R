library(sf)
library(dplyr)


zones_map = geojsonsf::geojson_sf("outputs/data_zones.geojson")
zones_map$area = as.numeric(st_area(zones_map)) / 10000
zones_map$population_density = round(zones_map$Total_population / zones_map$area)
zones_map$area = NULL
st_write(zones_map, "outputs/data_zones.geojson", delete_dsn = TRUE)


b_high = st_read("C:/tiles/daysmetric_test/high.geojson")
b_med = st_read("C:/tiles/daysmetric_test/med.geojson")
b_low = st_read("C:/tiles/daysmetric_test/low.geojson")
b_verylow = st_read("C:/tiles/daysmetric_test/verylow.geojson")

# b_high$geometry <- st_as_sfc(b_high$geometry, crs = 4326)
# b_med$geometry <- st_as_sfc(b_med$geometry, crs = 4326)
# b_low$geometry <- st_as_sfc(b_low$geometry, crs = 4326) 
# b_verylow$geometry <- st_as_sfc(b_verylow$geometry, crs = 4326) 

b_high = b_high[substr(b_high$geo_code,1,1) == "S",]
b_med = b_med[substr(b_med$geo_code,1,1) == "S",]
b_low = b_low[substr(b_low$geo_code,1,1) == "S",]
b_verylow = b_verylow[substr(b_verylow$geo_code,1,1) == "S",]


zones_map = st_drop_geometry(zones_map)

b_high$GRP_CD <- NULL
b_med$GRP_CD <- NULL
b_low$GRP_CD <- NULL
b_verylow$GRP_CD <- NULL

b_high = left_join(b_high, zones_map, by = c("geo_code" = "DataZone"))
b_med = left_join(b_med, zones_map, by = c("geo_code" = "DataZone"))
b_low = left_join(b_low, zones_map, by = c("geo_code" = "DataZone"))
b_verylow = left_join(b_verylow, zones_map, by = c("geo_code" = "DataZone"))

st_precision(b_high) <- 10000000
st_precision(b_med) <- 10000000
st_precision(b_low) <- 10000000
st_precision(b_verylow) <- 10000000

st_write(b_high, "outputs/dasymetric_high.geojson", delete_dsn = TRUE)
st_write(b_med, "outputs/dasymetric_med.geojson", delete_dsn = TRUE)
st_write(b_low, "outputs/dasymetric_low.geojson", delete_dsn = TRUE)
st_write(b_verylow, "outputs/dasymetric_verylow.geojson", delete_dsn = TRUE)

