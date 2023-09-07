library(tidyverse)
library(osmextract)
devtools::install_github("robinlovelace/ukboundaries")
library(ukboundaries)
devtools::install_github("robinlovelace/simodels")
library(simodels)
source("R/gravity_model.R")

disag_threshold = 1000 # increasing this reduces the number of od pairs

min_distance_meters = 500 # does this mean that any shops closer than 500m away are essentially ignored? 
# It would be better to route to these, then exclude them afterwards as too close for the trip to be worth cycling
max_length_euclidean_km = 5

osm_highways = readRDS("./inputdata/osm_highways_2023-08-09.Rds")

scot_zones = st_read("./data-raw/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_-9179620948196964406.gpkg")
grid = st_make_grid(scot_zones, cellsize = 500, what = "centers")
grid_df = data.frame(grid)
grid_df = tibble::rowid_to_column(grid_df, "grid_id")

highways_bng = osm_highways %>% 
  st_transform(27700)
highways = highways_bng %>% 
  mutate(grid_id = st_nearest_feature(highways_bng, grid))

# calculate weighting of each grid point
highways_grid = highways %>% 
  st_drop_geometry() %>% 
  group_by(grid_id) %>% 
  summarise(size = n())

# assign grid geometry
highways_join = inner_join(grid_df, highways_grid)
highways_sf = st_as_sf(highways_join)
highways_sf = st_transform(highways_sf, 4326)
# tm_shape(highways_sf) + tm_dots("size") # check points look right

saveRDS(highways_sf, "./inputdata/highways_grid.Rds")
highways_grid = readRDS("./inputdata//highways_grid.Rds")


trip_purposes = read.csv("./data-raw/scottish-household-survey-2012-19.csv")
go_home = trip_purposes$Mean[trip_purposes$Purpose == "Go Home"]
trip_purposes = trip_purposes %>% 
  filter(Purpose != "Sample size (=100%)") %>% 
  mutate(adjusted_mean = Mean/(sum(Mean)-go_home)*sum(Mean)
  )
visiting_percent = trip_purposes %>% 
  filter(Purpose == "Visiting friends or relatives") %>% 
  select(adjusted_mean)
visiting_percent = visiting_percent[[1]]/100

zones = readRDS("inputdata/DataZones.Rds")
zones_visiting = zones %>%
  mutate(visiting_trips = ResPop2011 * 2.61 * visiting_percent) # resident population (should use 18+ only) * trips per person (from NTS 2019 England) * percent of trips that are for visiting

# Spatial interaction model of journeys
od_visiting = si_to_od(zones_visiting, zones_visiting, max_dist = max_length_euclidean_km * 1000)
od_interaction = od_visiting %>% 
  si_calculate(fun = gravity_model, 
               m = origin_visiting_trips,
               n = destination_ResPop2011,
               d = distance_euclidean,
               beta = 0.5,
               constraint_production = origin_visiting_trips)
od_interaction = od_interaction %>% 
  filter(quantile(interaction, 0.9) < interaction)

saveRDS(od_interaction, "./inputdata/visiting_interaction.Rds")
od_interaction = readRDS("./inputdata/visiting_interaction.Rds")

# od_interaction = od_interaction %>% 
#   mutate(interaction = interaction/50000)

# this isn't working. It's too slow. Maybe because interaction is too high?
# why does distance_euclidean drop so dramatically when we go from od_interaction to od_interaction_jittered? 
od_interaction_jittered = odjitter::jitter(
  od = od_interaction,
  zones = zones_visiting,
  subpoints = highways_grid,
  disaggregation_key = "interaction",
  disaggregation_threshold = disag_threshold,
  min_distance_meters = min_distance_meters,
  deduplicate_pairs = FALSE
)

saveRDS(od_interaction_jittered, "./inputdata/visiting_interaction_jittered.Rds")
od_interaction_jittered = readRDS("./inputdata/visiting_interaction_jittered.Rds")