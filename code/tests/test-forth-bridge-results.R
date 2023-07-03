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
