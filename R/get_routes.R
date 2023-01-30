get_routes = function(od, plans, purpose = "work", folder = ".", batch = TRUE, id = "001") {
  no_batch = nrow(od) < 250 || nrow(od) > 14000
  if (no_batch) {
    batch = FALSE
  }
  route_list = sapply(plans, function(x) NULL)
  for(plan in plans) {
    message("Getting the ", plan, " routes for ", purpose, " journeys")
    file_name = paste0("routes_max_dist_", purpose, "_", plan, "_id", id, ".Rds") 
    savename_f = file.path(folder, file_name)
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

batch_routes = function(od, plans, purpose = "work", folder = ".", batch = TRUE, nrow_batch = 100) {
  browser()
  n_groups = nrow(od) / nrow_batch
  groups = rep(seq(n_groups))
}
