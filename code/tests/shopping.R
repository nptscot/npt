library(tidyverse)

# Calculate number of trips / number of cyclists
trip_purposes = read.csv("./data-raw/scottish-household-survey-2012-19.csv")
shop_percent = trip_purposes %>% 
  filter(Purpose =="Shopping") %>% 
  select(Mean)

# Get shopping destinations from OSM
shops = osmextract::oe_get("shops")

shopping_grid = readRDS("./data-private/shopping_grid.Rds")
shopping_grid = shopping_grid[zones_region, ]

# Spatial interaction model of journeys
od_shopping = si_to_od(zones_other_region, shopping_grid, max_dist = max_length_euclidean_km * 1000)
od_interaction = od_shopping %>% 
  si_calculate(fun = gravity_model, 
               m = origin_shopping,
               n = destination_size,
               d = distance_euclidean,
               beta = 0.5,
               constraint_production = origin_shopping)
od_interaction = od_interaction %>% 
  filter(quantile(interaction, 0.9) < interaction)


# Jittering
shopping_polygons = sf::st_buffer(shopping_grid, dist = 0.0001)

od_interaction_jittered = odjitter::jitter(
  od = od_interaction,
  zones = zones_other_region,
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
