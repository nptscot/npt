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
  # TODO: set min_per_o as a parameter?
  # Keep only max_per_o destinations per origin
  min_per_o = 10
  min_p = min(zones_p$p_trips)
  summary(od_interaction_filtered$n_destinations)
  od_interaction_filtered = purrr::map_dfr(
    unique(od_interaction$O),
    ~ {
      od_o = od_interaction |>
        dplyr::filter(O == .x)
      n_destinations = round(od_o$origin_p_trips[1] / min_p * min_per_o)
      od_o |>
        dplyr::slice_sample(n = n_destinations, weight_by = interaction)
    }
  )
  od_adjusted = od_interaction_filtered |>
    dplyr::group_by(O) |>
    dplyr::mutate(
      proportion = interaction / sum(interaction),
      p_all_modes = origin_p_trips * proportion
    ) |>
    dplyr::ungroup()
  summary(od_adjusted$p_all_modes)
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

make_parks = function(grid) {
  park_points = sf::st_read("inputdata/park_points.gpkg")
  study_area = sf::st_convex_hull(sf::st_union(grid))
  park_points = sf::st_transform(park_points, sf::st_crs(grid))
  park_points[study_area, ]
}
