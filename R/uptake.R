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
      mutate(pcycle_ebike = pct::uptake_pct_ebike_2020(
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
        pcycle_ebike = pcycle_ebike + (bicycle / all),
        pcycle_ebike = case_when(# Prevent the percentage of trips made by bike going above 100%:
          pcycle_ebike > 1 ~ 1,
          TRUE ~ pcycle_ebike),
        bicycle_go_dutch = pcycle_go_dutch * all,
        bicycle_ebike = pcycle_ebike * all,
        bicycle_increase_go_dutch = bicycle_go_dutch - bicycle,
        bicycle_increase_ebike = bicycle_ebike - bicycle,
        car_driver_go_dutch = car_driver - (bicycle_increase_go_dutch * car_driver / all),
        car_driver_ebike = car_driver - (bicycle_increase_ebike * car_driver / all),
        #car_passenger_go_dutch = car_passenger - (bicycle_increase * car_passenger / all),
        public_transport = train + bus,
        public_transport_go_dutch = public_transport - (bicycle_increase_go_dutch * public_transport / all),
        foot_go_dutch = foot - (bicycle_increase_go_dutch * foot / all),
        public_transport_ebike = public_transport - (bicycle_increase_ebike * public_transport / all),
        foot_ebike = foot - (bicycle_increase_ebike * foot / all),
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
      mutate(pcycle_ebike = pct::uptake_pct_ebike_2020(
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
        pcycle_ebike = pcycle_ebike + (bicycle / all),
        pcycle_ebike = case_when(# Prevent the percentage of trips made by bike going above 100%:
          pcycle_ebike > 1 ~ 1,
          TRUE ~ pcycle_go_dutch),
        bicycle_go_dutch = pcycle_go_dutch * all,
        bicycle_increase_go_dutch = bicycle_go_dutch - bicycle,
        bicycle_ebike = pcycle_ebike * all,
        bicycle_increase_ebike = bicycle_ebike - bicycle,
        car_driver_go_dutch = 1 - (bicycle_increase_go_dutch * 1 / all),
        car_driver_ebike = 1 - (bicycle_increase_ebike * 1 / all),
        #car_passenger_go_dutch = 1 - (bicycle_increase * 1 / all),
        public_transport = 1 + 1, #TODO: Fix this
        public_transport_go_dutch = 1 - (bicycle_increase_go_dutch * 1 / all),
        foot_go_dutch = 1 - (bicycle_increase_go_dutch * 1 / all),
        public_transport_ebike = 1 - (bicycle_increase_ebike * 1 / all),
        foot_ebike = 1 - (bicycle_increase_ebike * 1 / all),
        #other_go_dutch = 1 - (bicycle_increase * 1 / all)
      )
  } else {
    stop("Purpose ", purpose, " not yet supported")
  }
  routes
}
