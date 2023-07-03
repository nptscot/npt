# Aim: debug issue 154

library(targets)
library(tidyverse)
library(stplanr)

rnet_head = slice_max(rnet_commute_fastest, order_by = bicycle, n = 20)
mapview(rnet_head)
# Shows Main Street to Dalmeny is highest, not plausible...


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
