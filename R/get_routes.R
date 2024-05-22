get_routes = function(od, plans, purpose = "work", folder = ".", batch = TRUE, batch_save = FALSE, nrow_batch = 100, date = NULL, segments = TRUE) {
  if (nrow(od) < 50) {
    batch = FALSE
  }
  route_list = sapply(plans, function(x) NULL)
  for (plan in plans) {
    message("Getting the ", plan, " routes for ", purpose, " journeys")
    file_name = paste0("routes_max_dist_", purpose, "_", plan, ".Rds")
    savename_f = file.path(folder, file_name)
    if (file.exists(savename_f)) {
      message("Routes already exist: reading from file: ", savename_f)
      routes_filtered = readRDS(savename_f)
    } else {
      if (batch && !batch_save) {
        # One-off saving of pre-computed routes:
        id = NULL
        if (as.character(Sys.time()) < "2024-05-22 08:05:12.486921" && plan == "quietest") {
          id = 9876
        }
        routes_raw = cyclestreets::batch(
          id = id,
          desire_lines = od,
          wait = TRUE,
          maxDistance = 30000,
          username = "robinlovelace",
          strategies = plan,
          cols_to_keep = c("id", "distances", "quietness", "gradient_smooth"),
          segments = segments,
          delete_job = FALSE
        )
      } else {
        if (batch_save) {
          if (!dir.exists("tmp")) {
            dir.create("tmp")
          }
          tmp_path = file.path("tmp", date)
          if (!dir.exists(tmp_path)) {
            dir.create(tmp_path)
          }
          routes_raw = batch_routes(
            od,
            fun = stplanr::route,
            route_fun = cyclestreets::journey,
            purpose = purpose,
            plan = plan,
            nrow_batch = nrow_batch,
            temp_folder = tmp_path,
            # comment-out this line to use default instance:
            base_url = paste0(
              "http://",
              Sys.getenv("CYCLESTREETS_BATCH"),
              "-api.cyclestreets.net"
            ),
            pat = Sys.getenv("CYCLESTREETS_BATCH")
          )
        } else {
          routes_raw = stplanr::route(
            l = od,
            route_fun = cyclestreets::journey,
            plan = plan,
            # comment-out this line to use default instance:
            base_url = paste0(
              "http://",
              Sys.getenv("CYCLESTREETS_BATCH"),
              "-api.cyclestreets.net"
            ),
            pat = Sys.getenv("CYCLESTREETS_BATCH")
          )
        }
      }

      if (is.character(segments) && !is(routes_raw, "sf")) {
        r_filtered = routes_raw$routes |>
          group_by(route_number) |>
          mutate(route_hilliness = mean(gradient_smooth)) |>
          mutate(length_route = sum(distances)) |>
          ungroup()

        routes_filtered = list(routes = r_filtered, segments = routes_raw$segments)
      } else {
        routes_filtered = routes_raw |>
          group_by(route_number) |>
          mutate(route_hilliness = mean(gradient_smooth)) |>
          mutate(length_route = sum(distances)) |>
          ungroup()
      }
      rm(routes_raw)
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
  # Split up into list
  od$splittingID = ceiling(seq_len(nrow(od)) / nrow_batch)
  od = dplyr::group_by(od, splittingID)
  od = dplyr::group_split(od)

  max_pad = nchar(as.character(length(od)))

  results = list()
  for (i in seq_len(length(od))) {
    od_to_route = od[[i]]
    id = stringr::str_pad(i, max_pad, pad = "0")
    f = paste0("batch_", plan, "_", purpose, "_", id, "_with_", nrow(od_to_route), "_routes_of_", nrow_od, "_rows.Rds")
    f = file.path(temp_folder, f)
    message(Sys.time(), " doing batch ", id, " of ", length(od))
    message("Number of rows in batch: ", nrow(od_to_route))
    message("Looking in the file: ", f)

    # results[[i]] <- fun(od_to_route, ...)
    if (file.exists(f)) {
      message("File exists")
      results[[i]] = readRDS(f)
    } else {
      message("File does not exist")
      # Retry failing batches
      # Source: https://stackoverflow.com/questions/31999808/retry-for-loop-r-loop-if-error
      # results = as.list(1:100) # for testing
      while (TRUE) {
        message("Starting routing")

        id_batch = NULL

        # Recover routes:
        # if(plan == "balanced") {
        #   id_batch = as.numeric(id) + 3809 - 10
        #   message("id ", id_batch, " for ", id)
        # } else {
        #   id_batch = NULL
        # }

        results[[i]] = try(

          # standard routing:
          # fun(
          #   l = od_to_route,
          #   route_fun = cyclestreets::journey,
          #   plan = plan,
          #   warnNA = FALSE
          #   # comment-out this line to use default instance:
          #   # base_url = "http://5b44de2e26338760-api.cyclestreets.net",
          #   # pat = Sys.getenv("CYCLESTREETS_BATCH")
          # )
          # batch routing:
          cyclestreets::batch(
            desire_lines = od_to_route,
            id = id_batch,
            maxDistance = 30000,
            username = "robinlovelace",
            strategies = plan,
            wait = TRUE
          )
        )
        if (!is(results[[i]], "try-error")) break
      }
      message("Saving ", f, " to ", temp_folder)
      saveRDS(results[[i]], f)
      f = paste0("od", "_", purpose, "_", id, "_with_", nrow(od_to_route), "_of_", nrow_od, "_rows.Rds")
      f = file.path(temp_folder, f)
      message("Saving ", f, " to ", temp_folder)
      saveRDS(od_to_route, f)
    }
  }
  message("Combining results")
  saveRDS(results, file.path(temp_folder, "results_list.Rds"))
  result = bind_sf(results)
  return(result)
}

bind_sf = function(x) {
  if (length(x) == 0) stop("Empty list")
  geom_name = attr(x[[1]], "sf_column")
  x = data.table::rbindlist(x, use.names = FALSE)
  x[[geom_name]] = sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
  x = sf::st_as_sf(x)
  x
}
