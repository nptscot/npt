library(tidyverse)
library(osmextract)
devtools::install_github("robinlovelace/ukboundaries")
library(ukboundaries)
devtools::install_github("robinlovelace/simodels")
library(simodels)

library(tmap)
tmap_mode("view")
tmap_options(check.and.fix = TRUE)

source("R/gravity_model.R")

disag_threshold = 1000 # increasing this reduces the number of od pairs

min_distance_meters = 500 # does this mean that any shops closer than 500m away are essentially ignored? 
# It would be better to route to these, then exclude them afterwards as too close for the trip to be worth cycling
max_length_euclidean_km = 5


# Geographic zones --------------------------------------------------------

# Data Zones have 500-1000 residents
# https://spatialdata.gov.scot/geonetwork/srv/api/records/7d3e8709-98fa-4d71-867c-d5c8293823f2
# Intermediate Zones have 2500-6000 residents, there are 1279 in Scotland in 2011
# https://www.data.gov.uk/dataset/133d4983-c57d-4ded-bc59-390c962ea280/intermediate-zone-boundaries-2011#:~:text=Intermediate%20zones%20also%20represent%20a,covering%20the%20whole%20of%20Scotland.

# Population stats are also available from https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fpopulation-estimates-2011-datazone-linked-dataset&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fage=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fage%2Fall&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fsex=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fsex%2Fall

# DataZones (too small - makes for slow jittering)
zones = readRDS("../inputdata/DataZones_population.Rds")

# # Intermediate Zones
# izones = read_sf("./inputdata/SG_IntermediateZoneBdry_2011/SG_IntermediateZone_Bdry_2011.shp")
# izones = st_make_valid(izones)
# # st_is_valid(izones)
# izones = izones %>% 
#   st_transform(4326)
# # tm_shape(izones) + tm_polygons()

# Highways data -----------------------------------------------------------
# 
# osm_highways = readRDS("./inputdata/osm_highways_2023-08-09.Rds")
# 
# # This section only needs to run once
# scot_zones = st_read("./data-raw/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_-9179620948196964406.gpkg")
# grid = st_make_grid(scot_zones, cellsize = 300, what = "centers")
# grid_df = data.frame(grid)
# grid_df = tibble::rowid_to_column(grid_df, "grid_id")
# 
# highways_bng = osm_highways %>%
#   st_transform(27700)
# highways = highways_bng %>%
#   mutate(grid_id = st_nearest_feature(highways_bng, grid))
# 
# # calculate weighting of each grid point
# highways_grid = highways %>%
#   st_drop_geometry() %>%
#   group_by(grid_id) %>%
#   summarise(size = n())
# 
# # assign grid geometry
# highways_join = inner_join(grid_df, highways_grid)
# highways_sf = st_as_sf(highways_join)
# highways_sf = st_transform(highways_sf, 4326)
# 
# saveRDS(highways_sf, "./inputdata/highways_grid.Rds")
# 
# highways_grid = readRDS("./inputdata/highways_grid.Rds")
# 
# # check points look right
# tm_shape(zones) + tm_polygons() +
#   tm_shape(highways_grid) + tm_dots("size")
# 
# # check this is the same length as zones
# zones_in = st_contains(zones, highways_grid)
# summ = summary(zones_in)
# class(summ)
# summ = as.data.frame(summ)
# summ = summ %>% 
#   mutate(Freq = as.numeric(Freq))
# summary(summ$Freq)
# unique(summ$Freq)


# Trip purposes -----------------------------------------------------------

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

zones_visiting = zones %>%
  mutate(visiting_trips = ResPop2011 * 2.61 * visiting_percent) # resident population (should use 18+ only) * trips per person (from NTS 2019 England) * percent of trips that are for visiting


# Spatial interaction model -----------------------------------------------

# Spatial interaction model of journeys
od_visiting = si_to_od(zones_visiting, zones_visiting, max_dist = max_length_euclidean_km * 1000)
od_interaction = od_visiting %>% 
  si_calculate(fun = gravity_model, 
               m = origin_visiting_trips,
               n = destination_ResPop2011,
               d = distance_euclidean,
               beta = 0.5,
               constraint_production = origin_visiting_trips)
od_interaction_reduced = od_interaction %>%
  filter(quantile(interaction, 0.9) < interaction)

summary(sf::st_length(od_interaction))
summary(sf::st_length(od_interaction_reduced))

saveRDS(od_interaction_reduced, "./inputdata/visiting_interaction_zone.Rds")
od_interaction = readRDS("./inputdata/visiting_interaction_zone.Rds")

# od_interaction = od_interaction %>% 
#   mutate(interaction = interaction/50000)

# Jittering

oa_subpoints = readRDS("../inputdata/oas.Rds")

# # this isn't working. It's too slow. Maybe because interaction is too high?
# # why does distance_euclidean drop so dramatically when we go from od_interaction to od_interaction_jittered? 
od_interaction_jittered = odjitter::jitter(
  od = od_interaction,
  zones = zones_visiting,
  subpoints = oa_subpoints,
  disaggregation_key = "interaction",
  disaggregation_threshold = disag_threshold,
  min_distance_meters = min_distance_meters,
  deduplicate_pairs = FALSE
)
# 
# saveRDS(od_interaction_jittered, "./inputdata/visiting_interaction_jittered.Rds")
# od_interaction_jittered = readRDS("./inputdata/visiting_interaction_jittered.Rds")

od_interaction %>% 
  sample_n(1000) %>% 
  tm_shape() + tm_lines()

# Trip numbers - find which % of these journeys are by bicycle

# Get cycle mode shares
cycle_mode_share = 0.012 
# it would be nice to get this by local authority 
# but table 16 in transport-and-travel-in-scotland-2019-local-authority-tables.xlsx
# is not accurate enough (no decimal places for the cycle % mode shares)

od_visiting_cycletrips = od_interaction %>% 
  rename(
    visiting_all_modes = interaction,
    geo_code1 = O,
    geo_code2 = D
  ) %>% 
  mutate(visiting_cycle = visiting_all_modes * cycle_mode_share)

od_visiting_cycletrips_updated = od_visiting_cycletrips %>% 
  rename(length_euclidean_unjittered = distance_euclidean) %>% 
  mutate(
    length_euclidean_unjittered = length_euclidean_unjittered/1000,
    length_euclidean_jittered = units::drop_units(st_length(od_visiting_jittered))/1000
  ) %>%
  filter(
    length_euclidean_jittered > (min_distance_meters/1000),
    length_euclidean_jittered < max_length_euclidean_km
  )
n_short_lines_removed = nrow(od_visiting_jittered) - nrow(od_visiting_jittered_updated)
message(n_short_lines_removed, " short or long desire lines removed")

saveRDS(od_visiting_jittered_updated, "./inputdata/od_visiting_jittered.Rds")