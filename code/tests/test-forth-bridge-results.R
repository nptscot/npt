# Aim: debug issue 154

library(targets)
library(tidyverse)
library(stplanr)

rnet_head = slice_max(rnet_commute_fastest, order_by = bicycle, n = 20)
m = mapview(rnet_head)
m
# Shows Main Street to Dalmeny is highest, not plausible...
# main_street_transect = mapedit::editMap(m)

# Create point on main street
points_from_rnet_head = sf::st_cast(rnet_head, to = "POINT")
nrow(points_from_rnet_head) # 141
points_from_rnet_head = points_from_rnet_head %>% 
  mutate(id = seq(n()))
mapview(points_from_rnet_head)
point_mid_main_street = points_from_rnet_head %>% 
  filter(id == 88)
route_segments_on_main_street = r_commute$fastest[point_mid_main_street, ]
nrow(route_segments_on_main_street) # 2046

route_segments_on_main_street %>% 
  pull(bicycle) %>% 
  sum() # 946

length(unique(route_segments_on_main_street$geometry))
route_segments_by_geometry_type = route_segments_on_main_street %>% 
  group_by(geometry) %>% 
  summarise(n = n(), bicycle = sum(bicycle))

route_segments_by_geometry_type
route_segments_by_geometry_type %>% 
  select(bicycle) %>% 
  mapview() # they all overlap

route_segments_by_geometry_type %>% 
  select(bicycle) %>% 
  slice_max(bicycle) %>% 
  mapview() # they all overlap

# Find OD pair contributing to most cycling, cross check in desire lines:
od_pairs_main_street = route_segments_on_main_street %>% 
  group_by(geo_code1, geo_code2) %>% 
  sf::st_drop_geometry() %>% 
  summarise(n = n())
nrow(od_pairs_main_street) # 1776

tar_load(od_commute_subset)

nrow(od_commute_subset)
# od_commute_main_street = left_join(od_pairs_main_street, od_commute_subset)
nrow(od_commute_main_street)

route_segments_on_main_street = route_segments_on_main_street %>% 
  mutate(od_id = paste(geo_code1, geo_code2))

od_commute_subset = od_commute_subset %>% 
  mutate(od_id = paste(geo_code1, geo_code2))

od_commute_main_street = od_commute_subset %>% 
  filter(od_id %in% unique(route_segments_on_main_street$od_id))

sum(od_commute_main_street$bicycle) # 1399.867

class(od_commute_main_street)
od_commute_main_street = sf::st_as_sf(od_commute_main_street)
mapview(od_commute_main_street)
summary(od_commute_main_street$bicycle)


# Highest level of flow:
od_test_highest = od_commute_main_street %>% 
  ungroup() %>% 
  slice_max(order_by = bicycle, n = 200)
nrow(od_test_highest)

summary(od_test_highest$geo_code1 == od_test_highest$geo_code2)
mapview(od_test_highest) # None of these should use Main Street...

# Pull out associates routes
routes_test_highest = route_segments_on_main_street %>% 
  filter(od_id %in% od_test_highest$od_id)
nrow(routes_test_highest)
mapview(routes_test_highest)

routes_test = route(l = od_test_highest, route_fun = cyclestreets::journey, plan = "fastest")
mapview(routes_test %>% sample_n(1000))
plot(routes_test$geometry)

routes_batch = get_routes(od = od_test_highest, plans = "fastest", purpose = "commute", nrow_batch = 1000)

plot(routes_batch$fastest$geometry)

od_max = od_commute_subset %>% 
  slice_max(bicycle, n = 100)
mapview(od_max) # These are all in central Edinburgh

tar_load(zones) # the zones are centred on the forth bridge
# zones in fife only (plus 4 zones around Bo'ness)
fife = zones %>% 
  filter(str_detect(zones$InterZone, pattern = "S020017"))
# od pairs with at least one end north of the forth bridge (plus a few to Bo'ness)
od_fife = od_commute_subset %>% 
  filter(geo_code1 %in% fife$InterZone | geo_code2 %in% fife$InterZone)




rnet_head1 = slice_max(rnet_head, order_by = bicycle, n = 1)
mapview(rnet_head1)
routes_intersect = r_commute[rnet_head1,]
routes_intersect = r_commute[rnet_head1]
routes_intersect = r_commute$fastest[rnet_head1]
routes_intersect = r_commute$fastest[rnet_head1,]
tar_load(r_commute)
routes_intersect = r_commute$fastest[rnet_head1,]
summary(routes_intersect$bicycle)
mapview(routes_intersect)
sum(routes_intersect$bicycle)
table(routes_intersect$length_route)
table(routes_intersect$id)
table(routes_intersect$distances)
routes_intersect_3km = routes_intersect %>%
filter(distances == 3169)
sum(routes_intersect_3km$bicycle)
rnet_test = overline(r_commute$fastest, attrib = "bicycle")
test_max = slice_max(rnet_test, order_by = bicycle, n = 20)
mapview(test_max)
route_max = slice_max(r_commute$fastest, order_by = bicycle, n = 100)
mapview(route_max)
