# Aim: test shopping trips are consistent
# See https://github.com/nptscot/npt/issues/369
# See https://github.com/nptscot/npt/issues/355

# Starting point: run tar_make() in the root directory of the project, with low number of routes

library(tidyverse)
library(targets)
library(sf)

tar_load(uptake_utility_fastest)

routes = uptake_utility_fastest

dutch = routes |> 
  select(grep("dutch", names(routes)), purpose)

ebike = routes |> 
  select(grep("ebike", names(routes)), purpose)

base = routes |> 
  select(!grep("dutch", names(routes)) & !grep("ebike", names(routes)))


# Test consistency between scenarios: shopping ----------------------------
# these are not consistent

shopping_base = base |> 
  filter(purpose == "shopping")

shopping_orig_base = shopping_base |> 
  ungroup() |> 
  sf::st_drop_geometry() |> 
  select(bicycle, car, foot, taxi, public_transport)
sum(shopping_orig_base, na.rm = T)
# [1] 19312.9

routes_shopping = routes |> 
  filter(purpose == "shopping")
sum(routes_shopping$all, na.rm = T)
# [1] 19312.9

shopping_dutch = dutch |> 
  filter(purpose == "shopping")

shopping_orig_dutch = shopping_dutch |> 
  sf::st_drop_geometry() |> 
  select(bicycle_go_dutch, car_go_dutch, foot_go_dutch, taxi_go_dutch, public_transport_go_dutch)
sum(shopping_orig_dutch, na.rm = T)
# [1] 19312.9

shopping_ebike = ebike |> 
  filter(purpose == "shopping")

shopping_orig_ebike = shopping_ebike |> 
  sf::st_drop_geometry() |> 
  select(bicycle_ebike, car_ebike, foot_ebike, taxi_ebike, public_transport_ebike)
sum(shopping_orig_ebike, na.rm = T)
# [1] 19312.9



# Test consistency between scenarios: visiting ----------------------------
# these are consistent

visiting_base = base |> 
  filter(purpose == "visiting")

visiting_orig_base = visiting_base |> 
  ungroup() |> 
  sf::st_drop_geometry() |> 
  select(bicycle, car, foot, taxi, public_transport)
sum(visiting_orig_base, na.rm = T)
# [1] 5307.008

routes_visiting = routes |> 
  filter(purpose == "visiting")
sum(routes_visiting$all, na.rm = T)
# [1] 5307.008

visiting_dutch = dutch |> 
  filter(purpose == "visiting")

visiting_orig_dutch = visiting_dutch |> 
  sf::st_drop_geometry() |> 
  select(bicycle_go_dutch, car_go_dutch, foot_go_dutch, taxi_go_dutch, public_transport_go_dutch)
sum(visiting_orig_dutch, na.rm = T)
  # [1] 5307.008


visiting_ebike = ebike |> 
  filter(purpose == "visiting")

visiting_orig_ebike = visiting_ebike |> 
  sf::st_drop_geometry() |> 
  select(bicycle_ebike, car_ebike, foot_ebike, taxi_ebike, public_transport_ebike)
sum(visiting_orig_ebike, na.rm = T)
# [1] 5307.008