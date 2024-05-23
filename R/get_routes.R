get_routes = function(od, plans, purpose = "work", folder = ".", batch = TRUE, nrow_batch = 100, date = NULL, segments = TRUE, c2k = c("id", "distances", "quietness", "gradient_smooth")) {
  if (nrow(od) < 50) {
    batch = FALSE
  }
  route_list = sapply(plans, function(x) NULL)
  for (plan in plans) {
    message("Getting the ", plan, " routes for ", purpose, " journeys")
    file_name = glue::glue("routes_{purpose}_{plan}.csv.gz")
    savename_f = file.path(folder, file_name)
    if (file.exists(savename_f)) {
      message("Routes already exist: reading from file: ", savename_f)
      routes_raw = cyclestreets:::batch_read(savename_f, cols_to_keep = c2k, segments = segments)
      routes_id_table = table(routes_raw$routes$route_number)
      routes_id_names = sort(as.numeric(names(routes_id_table)))
      od$id = as.character(seq(nrow(od)))
      od = sf::st_drop_geometry(od)
      n_routes_removed = nrow(od) - length(routes_id_names)
      message(n_routes_removed, " routes removed")
      routes_raw$routes = dplyr::left_join(routes_raw$routes, od, by = dplyr::join_by(route_number == id))
    } else {
      # One-off saving of pre-computed routes:
      id = NULL
        if (as.character(Sys.time()) < "2024-05-23 07:15:12.486921" && plan == "balanced") {
          id = 9905
        }
        routes_raw = cyclestreets::batch(
          desire_lines = od,
          id = id,
          directory = folder,
          wait = TRUE,
          maxDistance = 30000,
          username = "robinlovelace",
          strategies = plan,
          cols_to_keep = c2k,
          segments = segments,
          delete_job = FALSE,
          filename = file_name |> gsub(".csv.gz", "", x = _)
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

  route_list[[paste(plan)]] = routes_filtered

  route_list
}

bind_sf = function(x) {
  if (length(x) == 0) stop("Empty list")
  geom_name = attr(x[[1]], "sf_column")
  x = data.table::rbindlist(x, use.names = FALSE)
  x[[geom_name]] = sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
  x = sf::st_as_sf(x)
  x
}
