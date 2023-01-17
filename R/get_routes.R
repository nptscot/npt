get_routes = function(od, plan_types, purpose = "work", folder = ".", route_list = NULL, batch = TRUE) {
  no_batch = nrow(od) < 250 || nrow(od) > 14000
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
          strategies = plan
        )
      } else {
        routes_raw = stplanr::route(
          l = od,
          route_fun = cyclestreets::journey,
          plan = plan
          # comment-out this line to use default instance:
          # base_url = "http://5b44de2e26338760-api.cyclestreets.net",
          # pat = Sys.getenv("CYCLESTREETS_BATCH")
        ) 
      }
      # message("Filtering out the long ones")
      # if(plan == "fastest") {
      #   routes_filtered = routes_raw %>% filter(length < max_fast_route_length)
      #   fast_ids = unique(paste0(routes_filtered$geo_code1, routes_filtered$geo_code2))
      # } else {
      #   routes_filtered = routes_raw %>% filter(paste0(geo_code1, geo_code2) %in% fast_ids)
      # }
      routes_filtered = routes_raw %>% 
        rename(length_route = length) %>% 
        group_by(route_number) %>% 
        mutate(route_hilliness = mean(gradient_smooth)) %>% 
        ungroup()
      message("Saving ", savename_f)
      saveRDS(routes_filtered, savename_f)
    }
    route_list[[paste(plan)]] = routes_filtered
  }
  route_list
}