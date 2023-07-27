library(tidyverse)
library(osmextract)
devtools::install_github("robinlovelace/ukboundaries")
library(ukboundaries)
devtools::install_github("robinlovelace/simodels")
library(simodels)
source("R/gravity_model.R")

# Add osm highways for scotland


# Get shopping destinations from secure OS data
path_teams = Sys.getenv("NPT_TEAMS_PATH")
os_pois = readRDS(file.path(path_teams, "secure_data/OS/os_poi.Rds"))
os_retail = os_pois %>% 
  filter(groupname == "Retail") # 26279 points

# unique(os_retail$categoryname)
# mapview::mapview(os_retail)

os_retail = os_retail %>% 
  st_transform(27700)

# Not needed now
# # Get shopping destinations from OSM
# scotland_points = oe_get("Scotland", layer = "points", extra_tags = "shop")
# shop_points = scotland_points %>% 
#   filter(!is.na(shop)) # 14917 points
# scotland_polygons = oe_get("Scotland", layer = "multipolygons", extra_tags = "shop")
# shop_polygons = scotland_polygons %>% 
#   filter(!is.na(shop)) # 4550 polygons
# 
# shop_points = shop_points %>% 
#   st_transform(27700)
# 
# saveRDS(shop_points, "./inputdata/shop_points.Rds")
# saveRDS(shop_polygons, "./inputdata/shop_polygons.Rds")
# 
# # mapview::mapview(shop_polygons)
# # mapview::mapview(shop_points)
# 
# # Remove polygons that have points inside them
# 
# # Join remaining polygons and points together

# Make a grid of shop density
# f = system.file("extdata", "data_sources.csv", package = "ukboundaries")
# data_sources = readr::read_csv(f)
# scot = getData("GADM", country = "UK", level = 1)
# scot_zones = ukboundaries::duraz(u = "https://api.os.uk/downloads/v1/products/BoundaryLine/downloads?area=GB&format=GeoPackage&redirect")
# scot_zones = ukboundaries::duraz(u = "https://hub.arcgis.com/datasets/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_-9179620948196964406.gpkg")


# create 500m grid covering whole of scotland
# Geographic data downloaded from https://hub.arcgis.com/datasets/ons::scottish-parliamentary-constituencies-december-2022-boundaries-sc-bgc-2/
scot_zones = st_read("./data-raw/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_-9179620948196964406.gpkg")
grid = st_make_grid(scot_zones, cellsize = 500, what = "centers")
# tm_shape(grid) + tm_dots() # to check (looks solid black)
grid_df = data.frame(grid)
grid_df = tibble::rowid_to_column(grid_df, "grid_id")


# assign points in shopping (can include duplicates) to their nearest grid point
# shopping = shop_points %>% 
#   mutate(grid_id = st_nearest_feature(shop_points, grid))
shopping = os_retail %>% 
  mutate(grid_id = st_nearest_feature(os_retail, grid))

# calculate weighting of each grid point
shopping_grid = shopping %>% 
  st_drop_geometry() %>% 
  group_by(grid_id) %>% 
  summarise(size = n())

# assign grid geometry
shopping_join = inner_join(grid_df, shopping_grid)
shopping_sf = st_as_sf(shopping_join)
shopping_sf = st_transform(shopping_sf, 4326)
# tm_shape(shopping_sf) + tm_dots("size") # check points look right

saveRDS(shopping_sf, "./inputdata/shopping_grid.Rds")


shopping_grid = readRDS("./inputdata/shopping_grid.Rds")

# Estimate number of shopping trips from each origin zone
# Calculate number of trips / number of cyclists
trip_purposes = read.csv("./data-raw/scottish-household-survey-2012-19.csv")
shop_percent = trip_purposes %>% 
  filter(Purpose =="Shopping") %>% 
  select(Mean)
shop_percent = shop_percent[[1]]/100

# need to improve on this figure:
# 2019 mean distance travelled is 9.6km (from transport-and-travel-in-scotland-2019-local-authority-tables.xlsx)
# find which % of these journeys are by bicycle

zones = targets::tar_load(zones)
zones_shopping = zones %>%
  mutate(shopping_km = ResPop2011 * 9.6 * shop_percent) # resident population (should use 18+ only) * km travelled per person * percent of trips (should be kms) that are for shopping

# Get cycle mode shares
cycle_mode_share = 0.012 # (can get this by local authority)

zones_shopping = zones_shopping %>% 
  mutate(shopping_cycle = shopping_km * cycle_mode_share)

# Spatial interaction model of journeys
max_length_euclidean_km = 5
od_shopping = si_to_od(zones_shopping, shopping_grid, max_dist = max_length_euclidean_km * 1000)
od_interaction = od_shopping %>% 
  si_calculate(fun = gravity_model, 
               m = origin_shopping_cycle,
               n = destination_size,
               d = distance_euclidean,
               beta = 0.5,
               constraint_production = origin_shopping_cycle)
od_interaction = od_interaction %>% 
  filter(quantile(interaction, 0.9) < interaction)


# Jittering
shopping_polygons = sf::st_buffer(shopping_grid, dist = 0.0001)

od_interaction_jittered = odjitter::jitter(
  od = od_interaction,
  zones = zones,
  zones_d = shopping_polygons,
  subpoints_origins = osm_highways_region,
  subpoints_destinations = shopping_grid,
  disaggregation_key = "interaction",
  disaggregation_threshold = disag_threshold,
  min_distance_meters = min_distance_meters
)

# Trip numbers
od_shopping_jittered = od_interaction_jittered %>% 
  rename(
    shopping = interaction,
    geo_code1 = O,
    geo_code2 = D
  ) %>% 
  mutate(
    cyclists = shopping * cycle_nat_mode_share,
    drivers = shopping * car_nat_mode_share,
    foot = shopping * foot_nat_mode_share,
    public_transport = shopping * pt_nat_mode_share,
    other = shopping * other_nat_mode_share,
    passengers = 0,
    all_modes = shopping
  )

od_shopping_jittered_updated = od_shopping_jittered %>% 
  rename(length_euclidean_unjittered = distance_euclidean) %>% 
  mutate(length_euclidean_jittered = units::drop_units(st_length(od_shopping_jittered))/1000) %>%
  filter(
    length_euclidean_jittered > (min_distance_meters/1000),
    length_euclidean_jittered < max_length_euclidean_km
  )
n_short_lines_removed = nrow(od_shopping_jittered) - nrow(od_shopping_jittered_updated)
message(n_short_lines_removed, " short or long desire lines removed")

saveRDS(od_shopping_jittered_updated, file.path(rds_folder, "od_shopping_jittered.Rds"))
