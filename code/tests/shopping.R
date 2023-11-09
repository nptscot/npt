library(tidyverse)
library(osmextract)
devtools::install_github("robinlovelace/ukboundaries")
library(ukboundaries)
devtools::install_github("robinlovelace/simodels")
library(simodels)
source("R/gravity_model.R")

disag_threshold = 100 # increasing this reduces the number of od pairs
# > summary(od_adjusted_jittered$interaction)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 49.65   49.91   49.96   49.94   49.98   50.00 
min_distance_meters = 500 # does this mean that any shops closer than 500m away are essentially ignored? 
# It would be better to route to these, then exclude them afterwards as too close for the trip to be worth cycling

# Add osm highways for scotland
osm_highways = readRDS("../inputdata/osm_highways_2023-08-09.Rds")

# Get shopping destinations from secure OS data
path_teams = Sys.getenv("NPT_TEAMS_PATH")
os_pois = readRDS(file.path(path_teams, "secure_data/OS/os_poi.Rds"))
os_pois = os_pois %>% 
  mutate(groupname = as.character(groupname))
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
# saveRDS(shop_polygons, "../inputdata/shop_polygons.Rds")
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

saveRDS(shopping_sf, "../inputdata/shopping_grid.Rds")


shopping_grid = readRDS("../inputdata/shopping_grid.Rds")

# Estimate number of shopping trips from each origin zone
# Calculate number of trips / number of cyclists
trip_purposes = read.csv("./data-raw/scottish-household-survey-2012-19.csv")
go_home = trip_purposes$Mean[trip_purposes$Purpose == "Go Home"]
trip_purposes = trip_purposes %>% 
  filter(Purpose != "Sample size (=100%)") %>% 
  mutate(adjusted_mean = Mean/(sum(Mean)-go_home)*sum(Mean)
         )
shop_percent = trip_purposes %>% 
  filter(Purpose =="Shopping") %>% 
  select(adjusted_mean)
shop_percent = shop_percent[[1]]/100

# need to improve on this figure:
# from NTS 2019 (England) average 953 trips/person/year divided by 365 = 2.61 trips/day
# previously used: 2019 mean distance travelled is 9.6km (from transport-and-travel-in-scotland-2019-local-authority-tables.xlsx)
zones = readRDS("inputdata/DataZones.Rds")
zones_shopping = zones %>%
  mutate(shopping_trips = ResPop2011 * 2.61 * shop_percent) # resident population (should use 18+ only) * trips per person (from NTS 2019 England) * percent of trips that are for shopping

# # Missing zone 
# (could find a more systematic way to do this)
# missing_zone =  zones %>% filter(DataZone == "S01010206")
# mapview::mapview(missing_zone) # The entire zone sits within a building site so it has no public road within it
zones_shopping = zones_shopping %>% 
  filter(DataZone != "S01010206")

# Spatial interaction model of journeys
# We could validate this SIM using the Scottish data on mean km travelled 
max_length_euclidean_km = 5
od_shopping = si_to_od(zones_shopping, shopping_grid, max_dist = max_length_euclidean_km * 1000)
od_interaction = od_shopping %>% 
  si_calculate(fun = gravity_model, 
               m = origin_shopping_trips,
               n = destination_size,
               d = distance_euclidean,
               beta = 0.5,
               constraint_production = origin_shopping_trips)
od_interaction = od_interaction %>% 
  filter(quantile(interaction, 0.9) < interaction)

saveRDS(od_interaction, "../inputdata/shopping_interaction.Rds")
od_interaction = readRDS("../inputdata/shopping_interaction.Rds")

# od_interaction = od_interaction %>% 
#   filter(!O == "S01010206")

# Need to correct the number of trips, in accordance with origin_shopping_trips
od_adjusted = od_interaction %>% 
  group_by(O) %>% 
  mutate(
    proportion = interaction / sum(interaction),
    shopping_all_modes = origin_shopping_trips * proportion
  ) %>% 
  ungroup()

# Jittering
shopping_polygons = sf::st_buffer(shopping_grid, dist = 0.0001)

# why does distance_euclidean drop so dramatically when we go from od_interaction to od_adjusted_jittered? 
od_adjusted_jittered = odjitter::jitter(
  od = od_adjusted,
  zones = zones_shopping,
  zones_d = shopping_polygons,
  subpoints_origins = osm_highways,
  subpoints_destinations = shopping_grid,
  disaggregation_key = "shopping_all_modes",
  disaggregation_threshold = disag_threshold,
  min_distance_meters = min_distance_meters,
  deduplicate_pairs = FALSE
)

saveRDS(od_adjusted_jittered, "../inputdata/shopping_interaction_jittered.Rds")

od_adjusted_jittered = readRDS("../inputdata/shopping_interaction_jittered.Rds")

# Trip numbers - find which % of these journeys are by bicycle

# Get cycle mode shares
cycle_mode_share = 0.012 
# it would be nice to get this by local authority 
# but table 16 in transport-and-travel-in-scotland-2019-local-authority-tables.xlsx
# is not accurate enough (no decimal places for the cycle % mode shares)

od_shopping_jittered = od_adjusted_jittered %>% 
  rename(
    geo_code1 = O,
    geo_code2 = D
  ) %>% 
  mutate(shopping_cycle = shopping_all_modes * cycle_mode_share)

od_shopping_jittered_updated = od_shopping_jittered %>% 
  rename(length_euclidean_unjittered = distance_euclidean) %>% 
  mutate(
    length_euclidean_unjittered = length_euclidean_unjittered/1000,
    length_euclidean_jittered = units::drop_units(st_length(od_shopping_jittered))/1000
    ) %>%
  filter(
    length_euclidean_jittered > (min_distance_meters/1000),
    length_euclidean_jittered < max_length_euclidean_km
  )
n_short_lines_removed = nrow(od_shopping_jittered) - nrow(od_shopping_jittered_updated)
message(n_short_lines_removed, " short or long desire lines removed")

saveRDS(od_shopping_jittered_updated, "../inputdata/od_shopping_jittered.Rds")
