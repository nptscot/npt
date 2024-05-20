#' Function build OD data from shopping
#' @param oas Output Area Bounds
#' @param os_pois OS poins of intrest
#' @param grid Grid of points to aggregate to
#' @param purpose The trip purpose (e.g. shopping, leisure, visiting)
#' @param trip_purposes trip_purposes
#' @param zones Administrative zones
#' @param parameters parameters
make_od = function(oas, os_pois, grid, purpose, trip_purposes, zones, parameters) {
  groupname = "Retail"
  if (purpose == "leisure") {
    groupname = "Sport and Entertainment"
  }
  os_pois = os_pois |>
    dplyr::filter(groupname == groupname)
  os_pois = os_pois |>
    sf::st_transform(sf::st_crs(grid))
  purpose_nts = "Shopping"

  if (purpose == "leisure") {
    purpose_nts = "Sport/Entertainment"
    # add in park points
    parks = make_parks(grid)
    os_pois = bind_rows(
      os_pois |> transmute(ref_no = as.character(ref_no)),
      parks |> transmute(ref_no = id)
    )
  }
  if (purpose == "visiting") {
    purpose_nts = "Visiting friends or relatives"
  }

  os_pois = os_pois |>
    dplyr::mutate(grid_id = sf::st_nearest_feature(os_pois, grid))

  # calculate weighting of each grid point
  p_grid = os_pois |>
    sf::st_drop_geometry() |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(size = n())
  p_grid = sf::st_as_sf(p_grid, geometry = grid[p_grid$grid_id])
  p_grid = sf::st_transform(p_grid, 4326)

  # Calculate number of trips / number of cyclists
  proportion_all_distances = trip_purposes |>
    dplyr::filter(tolower(Purpose) == tolower(purpose_nts)) |>
    dplyr::transmute(proportion = adjusted_mean / 100) |>
    dplyr::pull(proportion)
  zones_p = zones |>
    dplyr::select(DataZone, ResPop2011)
  # from NTS 2019 (England) average 953 trips/person/year divided by 365 = 2.61 trips/day
  total_trips_per_day = 2.61
  purpose2 = purpose
  if (purpose == "visiting") {
    purpose2 = "Social"
  }
  distance_frequency = readr::read_csv("inputdata/distance_frequency_wide.csv")
  proportion_in_od = distance_frequency |>
    dplyr::filter(tolower(`NPT purpose`) == tolower(purpose2)) |>
    dplyr::select(`1-2 km`, `2-5 km`, `5-10 km`, `10-15 km`, `15-20 km`) |>
    sum()
  proportion = proportion_all_distances * proportion_in_od
  zones_p = zones_p |>
    dplyr::mutate(p_trips = ResPop2011 * total_trips_per_day * proportion) |>
    dplyr::select(-ResPop2011)
  zones_p = sf::st_transform(zones_p, 4326)
  zones_p = sf::st_make_valid(zones_p)

  # Spatial interaction model of journeys
  max_length_euclidean_km = 20 / 1.3
  if (purpose == "visiting") {
    od_p_initial = simodels::si_to_od(zones_p, zones_p, max_dist = max_length_euclidean_km * 1000) |>
      dplyr::rename(destination_size = destination_p_trips)
  } else {
    od_p_initial = simodels::si_to_od(zones_p, p_grid, max_dist = max_length_euclidean_km * 1000)
  }
  beta_p = distance_frequency |>
    dplyr::filter(tolower(`NPT purpose`) == tolower(purpose2)) |>
    dplyr::pull(beta)

  od_interaction = od_p_initial |>
    simodels::si_calculate(
      fun = gravity_model,
      m = origin_p_trips,
      n = destination_size,
      d = distance_euclidean,
      beta = beta_p,
      constraint_production = origin_p_trips
    )

  interaction_summary = summary(od_interaction$interaction)
  od_interaction_filtered = od_interaction |>
    dplyr::filter(interaction >= 0.5)
  destinations_per_origin = od_interaction_filtered |>
    dplyr::group_by(O) |>
    dplyr::summarise(n = n()) |>
    dplyr::pull(n)
  if (min(destinations_per_origin) < 5) {
    message("Some origins have fewer than 5 destinations")
    # TODO: explore why some zones have fewer than 5 destinations after this:
    o_to_top_up = zones$DataZone[which(destinations_per_origin < 5)]
    od_interaction_top_up = od_interaction |>
      dplyr::filter(O %in% o_to_top_up) |>
      dplyr::group_by(O) |>
      slice_max(interaction, n = 5) |>
      dplyr::ungroup()
    od_filtered = od_interaction_filtered |>
      dplyr::filter(!(O %in% o_to_top_up))
    od_interaction_filtered = bind_rows(od_filtered, od_interaction_top_up)
  }

  od_adjusted = od_interaction_filtered |>
    dplyr::group_by(O) |>
    dplyr::mutate(
      proportion = interaction / sum(interaction),
      p_all_modes = origin_p_trips * proportion
    ) |>
    dplyr::ungroup()
  # sum(od_adjusted$p_all_modes) / sum(zones_p$p_trips) # close to 1

  # Jittering
  if (purpose == "visiting") {
    zones_centroids = zones_p |>
      sf::st_centroid()
    oas = c(sf::st_geometry(oas), sf::st_geometry(zones_centroids))
    od_adjusted_jittered = odjitter::jitter(
      od = od_adjusted,
      zones = zones_p,
      subpoints = oas,
      disaggregation_key = "p_all_modes",
      disaggregation_threshold = parameters$disag_threshold,
      deduplicate_pairs = FALSE
    )
  } else {
    using_s2 = sf::sf_use_s2()
    sf::sf_use_s2(TRUE)
    p_polygons = sf::st_buffer(p_grid, dist = 250)
    sf::sf_use_s2(using_s2)
    p_grid_full = dplyr::bind_rows(
      p_grid |> dplyr::mutate(grid_id = as.character(grid_id)),
      os_pois |> dplyr::transmute(grid_id = as.character(ref_no), size = 1)
    )
    od_adjusted_jittered = odjitter::jitter(
      od = od_adjusted,
      zones = zones_p,
      zones_d = p_polygons, # each polygon is a single grid point, so destinations are kept the same
      subpoints_origins = oas,
      subpoints_destinations = p_grid_full,
      disaggregation_key = "p_all_modes",
      disaggregation_threshold = parameters$disag_threshold,
      deduplicate_pairs = FALSE
    )
  }

  # Get mode shares

  # These are the overall means from the SHS Travel Diaries in table 16 of
  # transport-and-travel-in-scotland-2019-local-authority-tables.xlsx
  # car = driver + passenger
  # public_transport = bus + rail
  # taxi = taxi + other
  # TODO: update depending on the purpose
  mode_shares = data_frame(
    bicycle = 0.012,
    foot = 0.221,
    car = 0.652,
    public_transport = 0.093,
    taxi = 0.022
  )

  od_p_jittered = od_adjusted_jittered |>
    dplyr::rename(
      geo_code1 = O,
      geo_code2 = D
    ) |>
    dplyr::mutate(
      bicycle = p_all_modes * mode_shares$bicycle,
      foot = p_all_modes * mode_shares$foot,
      car = p_all_modes * mode_shares$car,
      public_transport = p_all_modes * mode_shares$public_transport,
      taxi = p_all_modes * mode_shares$taxi
    )

  od_p_subset = od_p_jittered |>
    dplyr::rename(length_euclidean_unjittered = distance_euclidean) |>
    dplyr::mutate(
      length_euclidean_unjittered = length_euclidean_unjittered / 1000,
      length_euclidean_jittered = units::drop_units(st_length(od_p_jittered)) / 1000
    ) |>
    dplyr::filter(
      length_euclidean_jittered > (parameters$min_distance_meters / 1000),
      length_euclidean_jittered < max_length_euclidean_km
    )
  n_short_lines_removed = nrow(od_p_jittered) - nrow(od_p_subset)
  message(n_short_lines_removed, " short or long desire lines removed")

  od_p_subset = od_p_subset |>
    dplyr::rename(
      origin_trips = origin_p_trips,
      all = p_all_modes
    ) |>
    mutate(purpose = purpose)

  od_p_subset
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
  park_grid = make_park_grid(grid)

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


make_parks = function(grid) {
  park_points = sf::st_read("inputdata/park_points.gpkg")
  study_area = sf::st_convex_hull(sf::st_union(grid))
  park_points = sf::st_transform(park_points, sf::st_crs(grid))
  park_points[study_area, ]
}

mode_shares = function() {
  data_frame(
    bicycle = 0.012,
    foot = 0.221,
    car = 0.652,
    public_transport = 0.093,
    taxi = 0.022
  )
}