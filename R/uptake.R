get_scenario_go_dutch = function(routes, purpose = "work") {
  routes = routes %>% dplyr::group_by(route_id)
  
  if(purpose == "work") {
    routes = routes %>%
      mutate(
        pcycle_go_dutch = pct::uptake_pct_godutch_2020(
        # Prevent incorrect uptake values #491
          case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
          route_hilliness),
        # Prevent the percentage of trips made by bike going above 100%:
        pcycle_go_dutch = case_when(
          pcycle_go_dutch > 1 ~ 1,
          TRUE ~ pcycle_go_dutch)
      )
    
    routes = routes %>%
      mutate(
        pcycle_ebike = pct::uptake_pct_ebike_2020(
          # Prevent incorrect uptake values #491
          case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
          route_hilliness),
        # Prevent the percentage of trips made by bike going above 100%:
        pcycle_ebike = case_when(
          pcycle_ebike > 1 ~ 1,
          TRUE ~ pcycle_ebike)
      )
    
    routes = routes %>%
      mutate(
        bicycle_go_dutch = max(c(pcycle_go_dutch * all, bicycle)),
        bicycle_ebike = max(c(pcycle_ebike * all, bicycle)),
        
        mode_ratio_go_dutch = (all - bicycle_go_dutch)/(all - bicycle),
        mode_ratio_go_dutch = case_when(is.infinite(mode_ratio_go_dutch) ~ 1, .default = mode_ratio_go_dutch),
        mode_ratio_ebike = (all - bicycle_ebike)/(all - bicycle),
        mode_ratio_ebike = case_when(is.infinite(mode_ratio_ebike) ~ 1, .default = mode_ratio_ebike),
        
        car_go_dutch = car * mode_ratio_go_dutch,
        public_transport_go_dutch = public_transport * mode_ratio_go_dutch,
        walk_go_dutch = walk * mode_ratio_go_dutch,
        taxi_go_dutch = taxi * mode_ratio_go_dutch,
        
        car_ebike = car * mode_ratio_ebike,
        public_transport_ebike = public_transport * mode_ratio_ebike,
        walk_ebike = walk * mode_ratio_ebike,
        taxi_ebike = taxi * mode_ratio_ebike
        
      )
  } else if(purpose == "school") {
    
    routes = routes %>%
      mutate(
        pcycle_go_dutch = pct::uptake_pct_godutch_school2(
          # Prevent incorrect uptake values #491
          case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
          route_hilliness),
        # Prevent the percentage of trips made by bike going above 100%:
        pcycle_go_dutch = case_when(
          pcycle_go_dutch > 1 ~ 1,
          TRUE ~ pcycle_go_dutch)
      )
    
    routes = routes %>%
      mutate(
        pcycle_ebike = pct::uptake_pct_ebike_2020(
          # Prevent incorrect uptake values #491
          case_when(length_route > 30000 ~ 30000, TRUE ~ length_route),
          route_hilliness),
        # Prevent the percentage of trips made by bike going above 100%:
        pcycle_ebike = case_when(
          pcycle_ebike > 1 ~ 1,
          TRUE ~ pcycle_ebike)
      )
    
    routes = routes %>%
      mutate(
        bicycle_go_dutch = max(c(pcycle_go_dutch * all, bicycle)),
        bicycle_ebike = max(c(pcycle_ebike * all, bicycle)),
        
        mode_ratio_go_dutch = (all - bicycle_go_dutch)/(all - bicycle),
        mode_ratio_go_dutch = case_when(is.infinite(mode_ratio_go_dutch) ~ 1, .default = mode_ratio_go_dutch),
        mode_ratio_ebike = (all - bicycle_ebike)/(all - bicycle),
        mode_ratio_ebike = case_when(is.infinite(mode_ratio_ebike) ~ 1, .default = mode_ratio_ebike),
        
        car_go_dutch = car * mode_ratio_go_dutch,
        public_transport_go_dutch = public_transport * mode_ratio_go_dutch,
        walk_go_dutch = walk * mode_ratio_go_dutch,
        taxi_go_dutch = taxi * mode_ratio_go_dutch,
        other_go_dutch = taxi * mode_ratio_go_dutch,
        
        car_ebike = car * mode_ratio_ebike,
        public_transport_ebike = public_transport * mode_ratio_ebike,
        walk_ebike = walk * mode_ratio_ebike,
        taxi_ebike = taxi * mode_ratio_ebike,
        other_ebike = ebike * mode_ratio_go_dutch,
      )
  } else {
    stop("Purpose ", purpose, " not yet supported")
  }
  routes
}