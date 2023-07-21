get_scenario_go_dutch = function(routes, purpose = "work") {
  routes = routes %>% dplyr::group_by(route_id)
  if(purpose == "work") {
    routes = routes %>%
      mutate(pcycle_go_dutch = pct::uptake_pct_godutch_2020(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))
    routes = routes %>%
      mutate(
        pcycle_go_dutch = pcycle_go_dutch + (bicycle / all),
        pcycle_go_dutch = case_when(# Prevent the percentage of trips made by bike going above 100%:
          pcycle_go_dutch > 1 ~ 1,
          TRUE ~ pcycle_go_dutch),
        bicycle_go_dutch = pcycle_go_dutch * all,
        bicycle_increase = bicycle_go_dutch - bicycle,
        car_driver_go_dutch = car_driver - (bicycle_increase * car_driver / all),
        #car_passenger_go_dutch = car_passenger - (bicycle_increase * car_passenger / all),
        public_transport = train + bus,
        public_transport_go_dutch = public_transport - (bicycle_increase * public_transport / all),
        foot_go_dutch = foot - (bicycle_increase * foot / all),
        #other_go_dutch = other - (bicycle_increase * other / all)
      )
  } else if(purpose == "school") {
    routes = routes %>%
      mutate(pcycle_go_dutch = pct::uptake_pct_godutch_school2(
        # Prevent incorrect uptake values #491
        case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
        route_hilliness
      ))
    routes = routes %>%
      mutate(
        pcycle_go_dutch = pcycle_go_dutch + (bicycle / all),
        pcycle_go_dutch = case_when(# Prevent the percentage of trips made by bike going above 100%:
          pcycle_go_dutch > 1 ~ 1,
          TRUE ~ pcycle_go_dutch),
        bicycle_go_dutch = pcycle_go_dutch * all,
        bicycle_increase = bicycle_go_dutch - bicycle,
        car_driver_go_dutch = 1 - (bicycle_increase * 1 / all),
        #car_passenger_go_dutch = 1 - (bicycle_increase * 1 / all),
        public_transport = 1 + 1,
        public_transport_go_dutch = 1 - (bicycle_increase * 1 / all),
        foot_go_dutch = 1 - (bicycle_increase * 1 / all),
        #other_go_dutch = 1 - (bicycle_increase * 1 / all)
      )
  } else {
    stop("Purpose ", purpose, " not yet supported")
  }
  routes
}

bind_sf = function(x) {
  if (length(x) == 0) stop("Empty list")
  geom_name = attr(x[[1]], "sf_column")
  x = data.table::rbindlist(x, use.names = FALSE)
  # x = collapse::unlist2d(x, idcols = FALSE, recursive = FALSE)
  x[[geom_name]] = st_sfc(x[[geom_name]], recompute_bbox = TRUE)
  x = st_as_sf(x)
}
