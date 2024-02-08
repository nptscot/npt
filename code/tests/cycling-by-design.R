# Cycling by design compliance

osm_highways = readRDS("./inputdata/osm_highways_2023-08-09.Rds")
names(osm_highways)
summary(osm_highways)

osm_cbd = osm_highways %>%
    select(osm_id, name, highway, maxspeed, bicycle, cycleway, cycleway_left, cycleway_right, cycleway_both, lanes, segregated)
