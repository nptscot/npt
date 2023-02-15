get_routes = function(od, plans, purpose = "work", folder = ".", batch = TRUE, batch_save = TRUE, nrow_batch = 100) {
  if (nrow(od) < 250) {
    batch = FALSE
  }
  route_list = sapply(plans, function(x) NULL)
  for(plan in plans) {
    message("Getting the ", plan, " routes for ", purpose, " journeys")
    file_name = paste0("routes_max_dist_", purpose, "_", plan, ".Rds") 
    savename_f = file.path(folder, file_name)
    message("Getting the ", plan, " routes")
    if(file.exists(savename_f)) {
      message("Routes already exist: reading from file: ", savename_f)
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
        if(batch_save) {
          routes_raw = batch_routes(
            od,
            fun = stplanr::route,
            route_fun = cyclestreets::journey,
            purpose = purpose,
            plan = plan,
            warnNA = FALSE, 
            nrow_batch = nrow_batch
            # comment-out this line to use default instance:
            # base_url = "http://5b44de2e26338760-api.cyclestreets.net",
            # pat = Sys.getenv("CYCLESTREETS_BATCH")
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

#' Save routes in batches before returning the result
#' 
#' An alternative to routing functions
#' 
#' @examples 
# tar_load(od_commute_subset)
# od = od_commute_subset
# fun = stplanr::route
# nrow_batch = 10
# plan = "fastest"
# purpose = "work"
batch_routes = function(od, fun, nrow_batch = 100, plan = "fastest", purpose, ..., temp_folder = tempdir()) {

  nrow_od = nrow(od)  
  #Split up into list
  od$splittingID <- ceiling(seq_len(nrow(od)) / nrow_batch)
  od <- dplyr::group_by(od, splittingID)
  od <- dplyr::group_split(od)
  
  max_pad <- nchar(as.character(length(od)))
  
  results <- list()
  for(i in seq_len(length(od))){
    od_to_route = od[[i]]
    id = stringr::str_pad(i, max_pad, pad = "0")
    f = paste0("batch_", plan, "_", purpose, "_", id, "_with_", nrow(od_to_route), "_routes_of_", nrow_od, "_rows.Rds")
    f = file.path(temp_folder, f)
    message(Sys.time()," doing batch ", id, " of ", length(od))
    message("Number of rows in batch: ", nrow(od_to_route))
    
    # results[[i]] <- fun(od_to_route, ...)
    if(file.exists(f)) {
      results[[i]] = readRDS(f)
    } else {
      results[[i]] <- fun(
        l = od_to_route,
        route_fun = cyclestreets::journey,
        plan = plan,
        warnNA = FALSE
        # comment-out this line to use default instance:
        # base_url = "http://5b44de2e26338760-api.cyclestreets.net",
        # pat = Sys.getenv("CYCLESTREETS_BATCH")
      )
      message("Saving ", f, " to ", temp_folder)
    }
  }
  message("Combining results")
  result = sf::st_as_sf(data.table::rbindlist(results))
  bbox = sfheaders::sf_bbox()
  return(result)
}
