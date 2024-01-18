library(tmap)
tmap_mode("view")
tmap_options(check.and.fix = TRUE)
library(sf)
library(targets)
library(tidyverse)

tar_load(oas)
tar_load(os_pois)
tar_load(grid)
tar_load(trip_purposes)
tar_load(parameters)
tar_load(study_area)

odjitter_location = parameters$odjitter_location
os_retail = os_pois %>% 
  dplyr::filter(groupname == "Retail") # 26279 points
os_retail = os_retail %>% 
  sf::st_transform(27700)
shopping = os_retail %>% 
  dplyr::mutate(grid_id = sf::st_nearest_feature(os_retail, grid))
# calculate weighting of each grid point
shopping_grid = shopping %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(grid_id) %>% 
  dplyr::summarise(size = n())
grid_df = data.frame(grid)
grid_df = tibble::rowid_to_column(grid_df, "grid_id")
shopping_join = dplyr::inner_join(grid_df, shopping_grid)
shopping_grid = sf::st_as_sf(shopping_join)
shopping_grid = sf::st_transform(shopping_grid, 4326)

tm_shape(shopping_grid) + tm_dots()
