get_routes = function(od, plans, purpose = "work", folder = ".", batch = TRUE, id = "001") {
  no_batch = nrow(od) < 250 
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
          plan = plan,
          warnNA = FALSE
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

batch_routes = function(od, plans, purpose = "work", folder = ".", batch = TRUE, nrow_batch = 1000) {
  
  #Split up into list
  od$splittingID <- ceiling(seq_len(nrow(od))/nrow_batch)
  od <- dplyr::group_by(od, splittingID)
  od <- dplyr::group_split(od)
  
  max_pad <- nchar(as.character(length(od)))
  
  results <- list()
  for(i in seq_len(length(od))){
    message(Sys.time()," doing batch ",i ," of ", length(od))
    results[[i]] <- get_routes(od = od[[i]], 
               plans = plans, 
               purpose = purpose, 
               folder = folder, 
               batch = batch,
               id = stringr::str_pad(i, max_pad, pad = "0"))
  }
  return(results)
}
