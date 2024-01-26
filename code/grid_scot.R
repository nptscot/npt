library(sf)
zones = readRDS("inputdata/DataZones.Rds")
scot_zones = sf::st_transform(zones, 27700) # Use existing zones
grid = sf::st_make_grid(scot_zones, cellsize = 500, what = "centers")
grid = grid[all_of(scot_zones)]

saveRDS(grid, "./inputdata/grid_scot.Rds")
