library(tidyverse)
library(osmextract)
devtools::install_github("robinlovelace/ukboundaries")
library(ukboundaries)
devtools::install_github("robinlovelace/simodels")
library(simodels)
source("R/gravity_model.R")

disag_threshold = 100 # increasing this reduces the number of od pairs

min_distance_meters = 500 # does this mean that any shops closer than 500m away are essentially ignored? 
# It would be better to route to these, then exclude them afterwards as too close for the trip to be worth cycling

osm_highways = readRDS("../inputdata/osm_highways_2023-08-09.Rds")

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
zones_visiting = zones_visiting %>% 
  filter(DataZone != "S01010206")

# Edinburgh sample
scot_zones = sf::st_read("./data-raw/Scottish_Parliamentary_Constituencies_December_2022_Boundaries_SC_BGC_-9179620948196964406.gpkg")
edinburgh_zones = scot_zones %>% 
  mutate(city = substr(SPC22NM, 1, 9)) %>% 
  filter(city %in% "Edinburgh")
edinburgh_zones = sf::st_transform(edinburgh_zones, 4326)
library(tmap)
tmap::tm_shape(edinburgh_zones) + tm_polygons()

zones_sample = zones_visiting[edinburgh_zones,]
tmap::tm_shape(zones_sample) + tm_polygons()

# Spatial interaction model of journeys
max_length_euclidean_km = 5
od_visiting = si_to_od(zones_sample, zones_sample, max_dist = max_length_euclidean_km * 1000)
od_interaction = od_visiting %>% 
  si_calculate(fun = gravity_model, 
               m = origin_visiting_trips,
               n = destination_ResPop2011,
               d = distance_euclidean,
               beta = 0.5,
               constraint_production = origin_visiting_trips)
od_interaction = od_interaction %>% 
  filter(quantile(interaction, 0.9) < interaction)

saveRDS(od_interaction, "../inputdata/visiting_interaction.Rds")
od_interaction = readRDS("../inputdata/visiting_interaction.Rds")


# Need to correct the number of trips, in accordance with origin_shopping_trips
od_adjusted = od_interaction %>% 
  group_by(O) %>% 
  mutate(
    proportion = interaction / sum(interaction),
    shopping_all_modes = origin_shopping_trips * proportion
  ) %>% 
  ungroup()

# why does distance_euclidean drop so dramatically when we go from od_interaction to od_adjusted_jittered? 
od_adjusted_jittered = odjitter::jitter(
  od = od_interaction,
  zones = zones_visiting,
  subpoints = osm_highways,
  disaggregation_key = "interaction",
  disaggregation_threshold = disag_threshold,
  min_distance_meters = min_distance_meters,
  deduplicate_pairs = FALSE
)

saveRDS(od_adjusted_jittered, "../inputdata/visiting_interaction_jittered.Rds")
od_adjusted_jittered = readRDS("../inputdata/visiting_interaction_jittered.Rds")