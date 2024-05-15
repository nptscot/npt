get_uptake_scenarios = function(routes, purpose = "work") {
  routes = routes |> dplyr::group_by(route_number)
  if (purpose == "work") {
    routes = routes |>
      mutate(pcycle_go_dutch = pct::uptake_pct_godutch_2020(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))

    routes = routes |>
      mutate(pcycle_ebike = pct::uptake_pct_ebike_2020(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))

    routes = routes |>
      mutate(
        bicycle_go_dutch = max(c(pcycle_go_dutch * all, bicycle)),
        bicycle_ebike = max(c(pcycle_ebike * all, bicycle)),
        mode_ratio_go_dutch = (all - bicycle_go_dutch) / (all - bicycle),
        mode_ratio_go_dutch = case_when(is.infinite(mode_ratio_go_dutch) ~ 1, .default = mode_ratio_go_dutch),
        mode_ratio_go_dutch = case_when(is.nan(mode_ratio_go_dutch) ~ 0, .default = mode_ratio_go_dutch),
        mode_ratio_ebike = (all - bicycle_ebike) / (all - bicycle),
        mode_ratio_ebike = case_when(is.infinite(mode_ratio_ebike) ~ 1, .default = mode_ratio_ebike),
        mode_ratio_ebike = case_when(is.nan(mode_ratio_ebike) ~ 0, .default = mode_ratio_ebike),
        car_go_dutch = car * mode_ratio_go_dutch,
        public_transport_go_dutch = public_transport * mode_ratio_go_dutch,
        foot_go_dutch = foot * mode_ratio_go_dutch,
        taxi_go_dutch = taxi * mode_ratio_go_dutch,
        car_ebike = car * mode_ratio_ebike,
        public_transport_ebike = public_transport * mode_ratio_ebike,
        foot_ebike = foot * mode_ratio_ebike,
        taxi_ebike = taxi * mode_ratio_ebike,
      )
  } else if (purpose == "school") {
    routes = routes |>
      mutate(pcycle_go_dutch = pct::uptake_pct_godutch_school2(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))

    routes = routes |>
      mutate(pcycle_ebike = pct::uptake_pct_ebike_2020(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))


    routes = routes |>
      mutate(
        bicycle_go_dutch = max(c(pcycle_go_dutch * all, bicycle)),
        bicycle_ebike = max(c(pcycle_ebike * all, bicycle)),
        mode_ratio_go_dutch = (all - bicycle_go_dutch) / (all - bicycle),
        mode_ratio_go_dutch = case_when(is.infinite(mode_ratio_go_dutch) ~ 1, .default = mode_ratio_go_dutch),
        mode_ratio_go_dutch = case_when(is.nan(mode_ratio_go_dutch) ~ 0, .default = mode_ratio_go_dutch),
        mode_ratio_ebike = (all - bicycle_ebike) / (all - bicycle),
        mode_ratio_ebike = case_when(is.infinite(mode_ratio_ebike) ~ 1, .default = mode_ratio_ebike),
        mode_ratio_ebike = case_when(is.nan(mode_ratio_ebike) ~ 0, .default = mode_ratio_ebike),
        car_go_dutch = car * mode_ratio_go_dutch,
        public_transport_go_dutch = public_transport * mode_ratio_go_dutch,
        foot_go_dutch = foot * mode_ratio_go_dutch,
        other_go_dutch = other * mode_ratio_go_dutch,
        car_ebike = car * mode_ratio_ebike,
        public_transport_ebike = public_transport * mode_ratio_ebike,
        foot_ebike = foot * mode_ratio_ebike,
        other_ebike = other * mode_ratio_go_dutch
      )
  } else if (purpose == "utility") {
    routes = routes |>
      mutate(pcycle_go_dutch = pct::uptake_pct_godutch_2020(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))

    routes = routes |>
      mutate(pcycle_ebike = pct::uptake_pct_ebike_2020(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))

    routes = routes |>
      dplyr::mutate(
        bicycle_unchanged = bicycle,
        bicycle = case_when(
          purpose == "shopping" ~ bicycle * 0.5,
          TRUE ~ bicycle
        ),
        pcycle_go_dutch = case_when(
          purpose == "shopping" ~ pcycle_go_dutch * 0.5,
          TRUE ~ pcycle_go_dutch
        ),
        pcycle_ebike = case_when(
          purpose == "shopping" ~ pcycle_ebike * 0.5,
          TRUE ~ pcycle_ebike
        ),
        bicycle_go_dutch = max(c(pcycle_go_dutch * all, bicycle)),
        bicycle_ebike = max(c(pcycle_ebike * all, bicycle)),
        mode_ratio_baseline = (all - bicycle) / (all - bicycle_unchanged),
        mode_ratio_baseline = case_when(is.infinite(mode_ratio_baseline) ~ 1, .default = mode_ratio_baseline),
        mode_ratio_baseline = case_when(is.nan(mode_ratio_baseline) ~ 0, .default = mode_ratio_baseline),
        car = car * mode_ratio_baseline,
        public_transport = public_transport * mode_ratio_baseline,
        foot = foot * mode_ratio_baseline,
        taxi = taxi * mode_ratio_baseline,
        mode_ratio_go_dutch = (all - bicycle_go_dutch) / (all - bicycle),
        mode_ratio_go_dutch = case_when(is.infinite(mode_ratio_go_dutch) ~ 1, .default = mode_ratio_go_dutch),
        mode_ratio_go_dutch = case_when(is.nan(mode_ratio_go_dutch) ~ 0, .default = mode_ratio_go_dutch),
        mode_ratio_ebike = (all - bicycle_ebike) / (all - bicycle),
        mode_ratio_ebike = case_when(is.infinite(mode_ratio_ebike) ~ 1, .default = mode_ratio_ebike),
        mode_ratio_ebike = case_when(is.nan(mode_ratio_ebike) ~ 0, .default = mode_ratio_ebike),
        car_go_dutch = car * mode_ratio_go_dutch,
        public_transport_go_dutch = public_transport * mode_ratio_go_dutch,
        foot_go_dutch = foot * mode_ratio_go_dutch,
        taxi_go_dutch = taxi * mode_ratio_go_dutch,
        car_ebike = car * mode_ratio_ebike,
        public_transport_ebike = public_transport * mode_ratio_ebike,
        foot_ebike = foot * mode_ratio_ebike,
        taxi_ebike = taxi * mode_ratio_ebike,
      ) |>
      select(-bicycle_unchanged)
  } else {
    stop("Purpose ", purpose, " not yet supported")
  }
  routes
}
