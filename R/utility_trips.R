#' Function build OD data from shopping
#' @param oas Output Area Bounds
#' @param os_pois OS poins of intrest
#' @param grid 500m grid over scotland
#' @param trip_purposes trip_purposes
#' @param intermediate_zones Scotland intermediate zones
#' @param parameters parameters
#' @param study_area study_area

make_od_shopping = function(oas, os_pois, grid, trip_purposes, intermediate_zones, parameters, study_area, odjitter_location = "odjitter") {
  os_retail = os_pois |>
    dplyr::filter(groupname == "Retail") # 26279 points
  os_retail = os_retail |>
    sf::st_transform(27700)

  shopping = os_retail |>
    dplyr::mutate(grid_id = sf::st_nearest_feature(os_retail, grid))

  # calculate weighting of each grid point
  shopping_grid = shopping |>
    sf::st_drop_geometry() |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(size = n())
  shopping_grid = sf::st_as_sf(shopping_grid, geometry = grid[shopping_grid$grid_id])
  shopping_grid = sf::st_transform(shopping_grid, 4326)

  # Estimate number of shopping trips from each origin zone
  # Calculate number of trips / number of cyclists
  shop_proportion_all_distances = trip_purposes |>
    dplyr::filter(Purpose == "Shopping") |>
    dplyr::transmute(proportion = adjusted_mean / 100) |>
    dplyr::pull(proportion)
  zones_shopping = zones |>
    dplyr::select(DataZone, ResPop2011)
  # from NTS 2019 (England) average 953 trips/person/year divided by 365 = 2.61 trips/day
  total_trips_per_day = 2.61
  distance_frequency = readr::read_csv("inputdata/distance_frequency_wide.csv")
  proportion_in_od = distance_frequency |>
    dplyr::filter(`NPT purpose` == "Shopping") |>
    dplyr::select(`1-2 km`, `2-5 km`, `5-10 km`, `10-15 km`, `15-20 km`) |>
    sum()
  shop_proportion = shop_proportion_all_distances * proportion_in_od
  zones_shopping = zones_shopping |>
    dplyr::mutate(shopping_trips = ResPop2011 * total_trips_per_day * shop_proportion) |>
    dplyr::select(-ResPop2011)
  zones_shopping = sf::st_transform(zones_shopping, 4326)
  zones_shopping = sf::st_make_valid(zones_shopping)


  # Spatial interaction model of journeys
  max_length_euclidean_km = 10 / 1.3
  od_shopping_initial = simodels::si_to_od(zones_shopping, shopping_grid, max_dist = max_length_euclidean_km * 1000)
  beta_shopping = distance_frequency |>
    dplyr::filter(`NPT purpose` == "Shopping") |>
    dplyr::pull(beta)
  od_interaction = od_shopping_initial |>
    simodels::si_calculate(
      fun = gravity_model,
      m = origin_shopping_trips,
      n = destination_size,
      d = distance_euclidean,
      beta = beta_shopping,
      constraint_production = origin_shopping_trips
    )

  od_interaction_filtered = od_interaction |>
    dplyr::filter(interaction >= 2.5)
  od_adjusted = od_interaction_filtered |>
    dplyr::group_by(O) |>
    dplyr::mutate(
      proportion = interaction / sum(interaction),
      shopping_all_modes = origin_shopping_trips * proportion
    ) |>
    dplyr::ungroup()

  # Jittering
  using_s2 = sf::sf_use_s2()
  sf::sf_use_s2(TRUE)
  shopping_polygons = sf::st_buffer(shopping_grid, dist = 250)
  sf::sf_use_s2(using_s2)
  # Workaround for #445
  sf::st_geometry(shopping_polygons) = "geom"

  shopping_grid_full = dplyr::bind_rows(
    shopping_grid,
    os_retail |> dplyr::transmute(grid_id = ref_no, size = 1)
    )
  od_adjusted_jittered = odjitter::jitter(
    od = od_adjusted,
    zones = zones_shopping,
    zones_d = shopping_polygons, # each polygon is a single grid point, so destinations are kept the same
    subpoints_origins = oas,
    subpoints_destinations = shopping_grid_full,
    disaggregation_key = "shopping_all_modes",
    disaggregation_threshold = parameters$disag_threshold,
    deduplicate_pairs = FALSE
  )

  # Get mode shares

  # These are the overall means from the SHS Travel Diaries in table 16 of
  # transport-and-travel-in-scotland-2019-local-authority-tables.xlsx
  # car = driver + passenger
  # public_transport = bus + rail
  # taxi = taxi + other
  mode_shares = data_frame(
    bicycle = 0.012,
    foot = 0.221,
    car = 0.652,
    public_transport = 0.093,
    taxi = 0.022
  )

  od_shopping_jittered = od_adjusted_jittered |>
    dplyr::rename(
      geo_code1 = O,
      geo_code2 = D
    ) |>
    dplyr::mutate(
      bicycle = shopping_all_modes * mode_shares$bicycle,
      foot = shopping_all_modes * mode_shares$foot,
      car = shopping_all_modes * mode_shares$car,
      public_transport = shopping_all_modes * mode_shares$public_transport,
      taxi = shopping_all_modes * mode_shares$taxi
    )

  od_shopping_subset = od_shopping_jittered |>
    dplyr::rename(length_euclidean_unjittered = distance_euclidean) |>
    dplyr::mutate(
      length_euclidean_unjittered = length_euclidean_unjittered / 1000,
      length_euclidean_jittered = units::drop_units(st_length(od_shopping_jittered)) / 1000
    ) |>
    dplyr::filter(
      length_euclidean_jittered > (parameters$min_distance_meters / 1000),
      length_euclidean_jittered < max_length_euclidean_km
    )
  n_short_lines_removed = nrow(od_shopping_jittered) - nrow(od_shopping_subset)
  message(n_short_lines_removed, " short or long desire lines removed")

  od_shopping_subset = od_shopping_subset |>
    dplyr::rename(
      origin_trips = origin_shopping_trips,
      all = shopping_all_modes
    ) |>
    mutate(purpose = "shopping")

  od_shopping_subset
}


#' Function build OD data from leisure
#' @param oas Output Area Bounds
#' @param os_pois OS poins of intrest
#' @param grid 500m grid over scotland
#' @param trip_purposes trip_purposes
#' @param intermediate_zones Scotland intermediate zones
#' @param parameters parameters
#' @param study_area study_area

make_od_leisure = function(oas, os_pois, grid, trip_purposes, intermediate_zones, parameters, study_area, odjitter_location = "odjitter") {
  os_leisure = os_pois |>
    dplyr::filter(groupname == "Sport and Entertainment") # 20524 points
  os_leisure = os_leisure |>
    sf::st_transform(27700)
  leisure = os_leisure |>
    dplyr::mutate(grid_id = sf::st_nearest_feature(os_leisure, grid))

  # calculate weighting of each grid point
  leisure_grid = leisure |>
    sf::st_drop_geometry() |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(size = n())

  # assign grid geometry
  grid_df = data.frame(grid)
  grid_df = tibble::rowid_to_column(grid_df, "grid_id")
  leisure_join = dplyr::inner_join(grid_df, leisure_grid)
  leisure_grid = sf::st_as_sf(leisure_join)
  leisure_grid = sf::st_transform(leisure_grid, 4326)

  # add in park points
  park_points = sf::st_read("inputdata/park_points.gpkg")
  if (parameters$geo_subset) {
    park_points = park_points[study_area, op = sf::st_within]
  }
  park_grid = park_points |>
    st_transform(27700)
  park_grid = park_grid |>
    dplyr::mutate(grid_id = sf::st_nearest_feature(park_grid, grid))
  park_grid = park_grid |>
    sf::st_drop_geometry() |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(size = n())
  # assign grid geometry
  park_join = dplyr::inner_join(grid_df, park_grid)
  park_grid = sf::st_as_sf(park_join)
  park_grid = sf::st_transform(park_grid, 4326)

  # combine the grids
  combined_grid = rbind(leisure_grid, park_grid)
  combined_grid = combined_grid |>
    sf::st_drop_geometry() |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(size = sum(size))
  # assign grid geometry
  combined_join = dplyr::inner_join(grid_df, combined_grid)
  combined_grid = sf::st_as_sf(combined_join)
  combined_grid = sf::st_transform(combined_grid, 4326)

  leisure_percent = trip_purposes |>
    dplyr::filter(Purpose == "Sport/Entertainment") |>
    dplyr::select(adjusted_mean)
  leisure_percent = leisure_percent[[1]] / 100

  zones_leisure = intermediate_zones |>
    dplyr::select(InterZone, ResPop2011)
  zones_leisure = zones_leisure |>
    dplyr::mutate(leisure_trips = ResPop2011 * 2.61 * leisure_percent) |>
    dplyr::select(-ResPop2011)
  zones_leisure = sf::st_transform(zones_leisure, 4326)
  zones_leisure = sf::st_make_valid(zones_leisure)

  # Spatial interaction model of journeys
  # We could validate this SIM using the Scottish data on mean km travelled
  max_length_euclidean_km = 5
  od_leisure_initial = simodels::si_to_od(zones_leisure, combined_grid, max_dist = max_length_euclidean_km * 1000)
  od_interaction = od_leisure_initial |>
    simodels::si_calculate(
      fun = gravity_model,
      m = origin_leisure_trips,
      n = destination_size,
      d = distance_euclidean,
      beta = 0.5,
      constraint_production = origin_leisure_trips
    )

  # Need to correct the number of trips, in accordance with origin_leisure_trips
  od_adjusted = od_interaction |>
    dplyr::group_by(O) |>
    dplyr::mutate(
      proportion = interaction / sum(interaction),
      leisure_all_modes = origin_leisure_trips * proportion
    ) |>
    dplyr::ungroup()

  # Jittering
  leisure_polygons = sf::st_buffer(combined_grid, dist = 0.0001)
  sf::st_geometry(leisure_polygons) = "geom"
  # why does distance_euclidean drop so dramatically when we go from od_interaction to od_adjusted_jittered?
  od_adjusted_jittered = odjitter::jitter(
    od = od_adjusted,
    zones = zones_leisure,
    zones_d = leisure_polygons, # each polygon is a single grid point, so destinations are kept the same
    subpoints_origins = oas,
    subpoints_destinations = combined_grid,
    disaggregation_key = "leisure_all_modes",
    disaggregation_threshold = parameters$disag_threshold,
    deduplicate_pairs = FALSE,
    odjitter_location = odjitter_location
  )

  # Get mode shares: TODO: move to parameters
  mode_shares = data_frame(
    bicycle = 0.012,
    foot = 0.221,
    car = 0.652,
    public_transport = 0.093,
    taxi = 0.022
  )

  od_leisure_jittered = od_adjusted_jittered |>
    dplyr::rename(
      geo_code1 = O,
      geo_code2 = D
    ) |>
    dplyr::mutate(
      bicycle = leisure_all_modes * mode_shares$bicycle,
      foot = leisure_all_modes * mode_shares$foot,
      car = leisure_all_modes * mode_shares$car,
      public_transport = leisure_all_modes * mode_shares$public_transport,
      taxi = leisure_all_modes * mode_shares$taxi
    )

  od_leisure_subset = od_leisure_jittered |>
    dplyr::rename(length_euclidean_unjittered = distance_euclidean) |>
    dplyr::mutate(
      length_euclidean_unjittered = length_euclidean_unjittered / 1000,
      length_euclidean_jittered = units::drop_units(st_length(od_leisure_jittered)) / 1000
    ) |>
    dplyr::filter(
      length_euclidean_jittered > (parameters$min_distance_meters / 1000),
      length_euclidean_jittered < max_length_euclidean_km
    )
  n_short_lines_removed = nrow(od_leisure_jittered) - nrow(od_leisure_subset)
  message(n_short_lines_removed, " short or long desire lines removed")

  od_leisure_subset = od_leisure_subset |>
    dplyr::rename(
      origin_trips = origin_leisure_trips,
      all = leisure_all_modes
    ) |>
    dplyr::mutate(purpose = "leisure")

  # Remove "output_col" column if it exists (TODO: fix upstream):
  if ("output_col" %in% colnames(od_leisure_subset)) {
    od_leisure_subset = od_leisure_subset |>
      dplyr::select(-output_col)
  }
  od_leisure_subset
}


#' Function build OD data for visting
#' @param oas Output Area Bounds
#' @param os_pois OS poins of intrest
#' @param grid 500m grid over scotland
#' @param trip_purposes trip_purposes
#' @param intermediate_zones Scotland intermediate zones
#' @param parameters parameters
#' @param study_area study_area

make_od_visiting = function(oas, os_pois, grid, trip_purposes, intermediate_zones, parameters, study_area, odjitter_location = "odjitter") {
  visiting_percent = trip_purposes |>
    dplyr::filter(Purpose == "Visiting friends or relatives") |>
    dplyr::select(adjusted_mean)
  visiting_percent = visiting_percent[[1]] / 100

  zones_visiting = intermediate_zones |>
    dplyr::select(InterZone, ResPop2011)
  zones_visiting = zones_visiting |>
    dplyr::mutate(visiting_trips = ResPop2011 * 2.61 * visiting_percent) |>
    dplyr::select(-ResPop2011)
  zones_visiting = sf::st_transform(zones_visiting, 4326)
  zones_visiting = sf::st_make_valid(zones_visiting)

  # Spatial interaction model of journeys
  max_length_euclidean_km = 5
  od_visiting_initial = simodels::si_to_od(zones_visiting, zones_visiting, max_dist = max_length_euclidean_km * 1000)
  od_interaction = od_visiting_initial |>
    simodels::si_calculate(
      fun = gravity_model,
      m = origin_visiting_trips,
      n = destination_visiting_trips,
      d = distance_euclidean,
      beta = 0.5,
      constraint_production = origin_visiting_trips
    )

  # Need to correct the number of trips, in accordance with origin_visiting_trips
  od_adjusted = od_interaction |>
    dplyr::group_by(O) |>
    dplyr::mutate(
      proportion = interaction / sum(interaction),
      visiting_all_modes = origin_visiting_trips * proportion
    ) |>
    dplyr::ungroup()

  # why does distance_euclidean drop so dramatically when we go from od_interaction to od_adjusted_jittered?
  od_adjusted_jittered = odjitter::jitter(
    od = od_adjusted,
    zones = zones_visiting,
    subpoints = oas,
    disaggregation_key = "visiting_all_modes",
    disaggregation_threshold = parameters$disag_threshold,
    deduplicate_pairs = FALSE,
    odjitter_location = odjitter_location
  )

  # Get cycle mode shares
  mode_shares = data_frame(
    bicycle = 0.012,
    foot = 0.221,
    car = 0.652,
    public_transport = 0.093,
    taxi = 0.022
  )

  od_visiting_jittered = od_adjusted_jittered |>
    dplyr::rename(
      geo_code1 = O,
      geo_code2 = D
    ) |>
    dplyr::mutate(
      bicycle = visiting_all_modes * mode_shares$bicycle,
      foot = visiting_all_modes * mode_shares$foot,
      car = visiting_all_modes * mode_shares$car,
      public_transport = visiting_all_modes * mode_shares$public_transport,
      taxi = visiting_all_modes * mode_shares$taxi
    )

  od_visiting_subset = od_visiting_jittered |>
    dplyr::rename(length_euclidean_unjittered = distance_euclidean) |>
    dplyr::mutate(
      length_euclidean_unjittered = length_euclidean_unjittered / 1000,
      length_euclidean_jittered = units::drop_units(st_length(od_visiting_jittered)) / 1000
    ) |>
    dplyr::filter(
      length_euclidean_jittered > (parameters$min_distance_meters / 1000),
      length_euclidean_jittered < max_length_euclidean_km
    )
  n_short_lines_removed = nrow(od_visiting_jittered) - nrow(od_visiting_subset)
  message(n_short_lines_removed, " short or long desire lines removed")

  od_visiting_subset = od_visiting_subset |>
    dplyr::rename(
      origin_trips = origin_visiting_trips,
      all = visiting_all_modes,
      destination_size = destination_visiting_trips
    ) |>
    dplyr::mutate(purpose = "visiting")
  od_visiting_subset
}
