# Routing -----------------------------------------------------------------

max_to_route = 1000 # change to do all routes
update_routes = TRUE

get_routes = function(od, plan_types, purpose = "work", folder = ".", route_list = NULL, batch = TRUE) {
  no_batch = nrow(od) < 200 || nrow(od) > 300000
  if (no_batch) {
    batch = FALSE
  }
  for(plan in plan_types) {
    message("Getting the ", plan, " routes for ", purpose, " journeys")
    file_name = paste0("routes_max_dist_", purpose, "_", plan, "_baseline", ".Rds") 
    # file_name = paste0("routes_max_dist_", purpose, "_", plan, ".Rds") # todo: uncomment this and delete line above
    savename_f = file.path(rds_folder, file_name)
    message("Getting the ", plan, " routes")
    if(file.exists(savename_f)) {
      message("Routes already exist: reading from file")
      routes_filtered = readRDS(savename_f)
    } else {
      if(batch) {
        routes_raw = cyclestreets::batch(
          desire_lines = od,
          maxDistance = max_length_euclidean_km * 1000,
          username = "robinlovelace",
          wait = TRUE,
          strategies = plan
        )
      } else {
        routes_raw = stplanr::route(
          l = od,
          route_fun = cyclestreets::journey,
          plan = plan,
          # comment-out this line to use default instance:
          base_url = "http://5b44de2e26338760-api.cyclestreets.net",
          pat = Sys.getenv("CYCLESTREETS_BATCH")
        ) 
      }
      routes_raw = routes_raw %>% 
        group_by(route_number) %>% 
        mutate(route_hilliness = weighted.mean(gradient_smooth, distances)) %>%
        mutate(length_route = sum(distances)) %>% 
        ungroup()
      message("Filtering out the long ones")
      if(plan == "fastest") {
        routes_filtered = routes_raw %>% filter(length_route < max_fast_route_length)
        fast_ids = unique(paste0(routes_filtered$geo_code1, routes_filtered$geo_code2))
      } else {
        routes_filtered = routes_raw %>% filter(paste0(geo_code1, geo_code2) %in% fast_ids)
      }
      # Assign route_number if routes came from batch routing:
      if(is.null(routes_filtered$route_number)) {
        routes_filtered$route_number = routes_filtered$id
        routes_filtered$id = NULL
      }
      message("Saving ", savename_f)
      saveRDS(routes_filtered, savename_f)
    }
    route_list[[paste(plan)]] = routes_filtered
  }
  route_list
}


od_shopping_jittered_updated = od_shopping_jittered_updated %>% 
  slice(seq(max_to_route))
message(nrow(od_shopping_jittered_updated), " shopping routes will be calculated")

routes_shopping = list() # create the route_list object which grows as more routes added
if(update_routes | !file.exists("../outputs/routes_shopping.Rds")) {
  routes_shopping = get_routes(od_shopping_jittered_updated, plan_types, purpose = "shopping", folder = rds_folder, route_list = routes_shopping)
  for(i in 1:length(routes_shopping)) {
    routes_shopping[[i]] = get_scenario_near(routes_shopping[[i]], purpose = "shopping")
    routes_shopping[[i]] = get_scenario_climate(routes_shopping[[i]], include_passengers = FALSE, purpose = "shopping")
    # routes_shopping[[i]] = get_scenario_30pc(routes_shopping[[i]], include_passengers = FALSE, purpose = "shopping", pt_uplift_factor = 0.2, dr_factor = 0.1)
    routes_shopping[[i]] = get_scenario_godutch(routes_shopping[[i]], purpose = "shopping")
    routes_shopping[[i]] = get_scenario_ebike(routes_shopping[[i]], purpose = "shopping")
  }
  saveRDS(routes_shopping, "../outputs/routes_shopping.Rds")
} else {
  routes_shopping = readRDS("../outputs/routes_shopping.Rds")
}

