get_scenario_godutch = function(routes, purpose = "work") {
  routes = routes %>% group_by(route_number)
  routes = routes %>%
    mutate(pcycle_godutch = pct::uptake_pct_godutch_2020(
      # Prevent incorrect uptake values #491
      case_when(length > 30000 ~ 30000, TRUE ~ length),
      route_hilliness
    ))
  
  routes = routes %>%
    mutate(
      pcycle_godutch = pcycle_godutch + (cyclists / all_modes),
      pcycle_godutch = case_when(# Prevent the percentage of trips made by bike going above 100%:
        pcycle_godutch > 1 ~ 1,
        TRUE ~ pcycle_godutch),
      cyclists_godutch = pcycle_godutch * all_modes,
      bicycle_increase = cyclists_godutch - cyclists,
      drivers_godutch = drivers - (bicycle_increase * drivers / all_modes),
      passengers_godutch = passengers - (bicycle_increase * passengers / all_modes),
      public_transport_godutch = public_transport - (bicycle_increase * public_transport / all_modes),
      foot_godutch = foot - (bicycle_increase * foot / all_modes),
      other_godutch = other - (bicycle_increase * other / all_modes)
    )
  routes
}
