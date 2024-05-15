library(tidyverse)
library(osmextract)
devtools::install_github("robinlovelace/ukboundaries")
library(ukboundaries)
devtools::install_github("robinlovelace/simodels")
library(simodels)
source("R/gravity_model.R")

disag_threshold = 100 # increasing this reduces the number of od pairs
min_distance_meters = 500 # does this mean that any destinations closer than 500m away are essentially ignored? 
# It would be better to route to these, then exclude them afterwards as too close for the trip to be worth cycling

# Add osm highways for scotland
osm_highways = readRDS("./inputdata/osm_highways_2023-08-09.Rds")

# Get leisure destinations from secure OS data
path_teams = Sys.getenv("NPT_TEAMS_PATH")
os_pois = readRDS(file.path(path_teams, "secure_data/OS/os_poi.Rds"))
os_leisure = os_pois |> 
  filter(groupname == "Sport and Entertainment") # 20524 points

# unique(os_leisure$categoryname)
# mapview::mapview(os_leisure)

os_leisure = os_leisure |> 
  st_transform(27700)

# create 500m grid covering whole of scotland
# Geographic data downloaded from https://hub.arcgis.com/datasets/ons::scottish-parliamentary-constituencies-december-2022-boundaries-sc-bgc-2/
scot_zones = st_read("./data-raw/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_-9179620948196964406.gpkg")
grid = st_make_grid(scot_zones, cellsize = 500, what = "centers")
# tm_shape(grid) + tm_dots() # to check (looks solid black)
grid_df = data.frame(grid)
grid_df = tibble::rowid_to_column(grid_df, "grid_id")

leisure = os_leisure |> 
  mutate(grid_id = st_nearest_feature(os_leisure, grid))

# calculate weighting of each grid point
leisure_grid = leisure |> 
  st_drop_geometry() |> 
  group_by(grid_id) |> 
  summarise(size = n())

# assign grid geometry
leisure_join = inner_join(grid_df, leisure_grid)
leisure_sf = st_as_sf(leisure_join)
leisure_sf = st_transform(leisure_sf, 4326)
# tm_shape(leisure_sf) + tm_dots("size") # check points look right

saveRDS(leisure_sf, "./inputdata/leisure_grid.Rds")


leisure_grid = readRDS("./inputdata/leisure_grid.Rds")

# Estimate number of leisure trips from each origin zone
# Calculate number of trips / number of cyclists
trip_purposes = read.csv("./data-raw/scottish-household-survey-2012-19.csv")
go_home = trip_purposes$Mean[trip_purposes$Purpose == "Go Home"]
trip_purposes = trip_purposes |> 
  filter(Purpose != "Sample size (=100%)") |> 
  mutate(adjusted_mean = Mean/(sum(Mean)-go_home)*sum(Mean)
  )
leisure_percent = trip_purposes |> 
  filter(Purpose =="Sport/Entertainment") |> 
  select(adjusted_mean)
leisure_percent = leisure_percent[[1]]/100

# need to improve on this figure:
# from NTS 2019 (England) average 953 trips/person/year divided by 365 = 2.61 trips/day
zones = readRDS("inputdata/DataZones.Rds")
zones_leisure = zones |>
  mutate(leisure_trips = ResPop2011 * 2.61 * leisure_percent) # resident population (should use 18+ only) * trips per person (from NTS 2019 England) * percent of trips that are for leisure

# # Missing zone 
# (could find a more systematic way to do this)
# missing_zone =  zones |> filter(DataZone == "S01010206")
# mapview::mapview(missing_zone) # The entire zone sits within a building site so it has no public road within it
zones_leisure = zones_leisure |> 
  filter(DataZone != "S01010206")

# Spatial interaction model of journeys
# We could validate this SIM using the Scottish data on mean km travelled 
max_length_euclidean_km = 5
od_leisure = si_to_od(zones_leisure, leisure_grid, max_dist = max_length_euclidean_km * 1000)
od_interaction = od_leisure |> 
  si_calculate(fun = gravity_model, 
               m = origin_leisure_trips,
               n = destination_size,
               d = distance_euclidean,
               beta = 0.5,
               constraint_production = origin_leisure_trips)
od_interaction = od_interaction |> 
  filter(quantile(interaction, 0.9) < interaction)

saveRDS(od_interaction, "./inputdata/leisure_interaction.Rds")
od_interaction = readRDS("./inputdata/leisure_interaction.Rds")

# Need to correct the number of trips, in accordance with origin_leisure_trips
od_adjusted = od_interaction |> 
  group_by(O) |> 
  mutate(
    proportion = interaction / sum(interaction),
    leisure_all_modes = origin_leisure_trips * proportion
  ) |> 
  ungroup()

# Jittering
leisure_polygons = sf::st_buffer(leisure_grid, dist = 0.0001)

# why does distance_euclidean drop so dramatically when we go from od_interaction to od_adjusted_jittered? 
od_adjusted_jittered = odjitter::jitter(
  od = od_adjusted,
  zones = zones_leisure,
  zones_d = leisure_polygons,
  subpoints_origins = osm_highways,
  subpoints_destinations = leisure_grid,
  disaggregation_key = "leisure_all_modes",
  disaggregation_threshold = disag_threshold,
  min_distance_meters = min_distance_meters,
  deduplicate_pairs = FALSE
)

saveRDS(od_adjusted_jittered, "./inputdata/leisure_interaction_jittered.Rds")

od_adjusted_jittered = readRDS("./inputdata/leisure_interaction_jittered.Rds")

# Trip numbers - find which % of these journeys are by bicycle

# Get cycle mode shares
cycle_mode_share = 0.012 
# it would be nice to get this by local authority 
# but table 16 in transport-and-travel-in-scotland-2019-local-authority-tables.xlsx
# is not accurate enough (no decimal places for the cycle % mode shares)

od_leisure_jittered = od_adjusted_jittered |> 
  rename(
    geo_code1 = O,
    geo_code2 = D
  ) |> 
  mutate(leisure_cycle = leisure_all_modes * cycle_mode_share)

od_leisure_jittered_updated = od_leisure_jittered |> 
  rename(length_euclidean_unjittered = distance_euclidean) |> 
  mutate(
    length_euclidean_unjittered = length_euclidean_unjittered/1000,
    length_euclidean_jittered = units::drop_units(st_length(od_leisure_jittered))/1000
  ) |>
  filter(
    length_euclidean_jittered > (min_distance_meters/1000),
    length_euclidean_jittered < max_length_euclidean_km
  )
n_short_lines_removed = nrow(od_leisure_jittered) - nrow(od_leisure_jittered_updated)
message(n_short_lines_removed, " short or long desire lines removed")

saveRDS(od_leisure_jittered_updated, "./inputdata/od_leisure_jittered.Rds")
