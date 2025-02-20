#' Function To simplify rnets
#' @param combined_network combined_network
#' @param parameters parameters

simplify_network = function(rnet_y, parameters, region_boundary) {
  # Read spatial data directly from URLs into sf objects
  # Build file path and URL based on parameters$region
  region_snake_case = snakecase::to_snake_case(parameters$region[[1]])
  base_name = paste0("OS_Scotland_Network_", region_snake_case, ".geojson")
  rnet_x_f = file.path("inputdata", base_name)

  if (!dir.exists("inputdata")) {
    dir.create("inputdata")
  }

  if (!file.exists(rnet_x_f)) {
    download_url = paste0("https://github.com/nptscot/inputdata/releases/download/os_rtps/", base_name)
    download.file(url = download_url, destfile = rnet_x_f, mode = "wb")
  }

  rnet_x = sf::read_sf(rnet_x_f) |>
    sf::st_transform(crs = "EPSG:27700")

  rnet_xp = sf::st_transform(rnet_x, "EPSG:27700")
  rnet_yp = sf::st_transform(rnet_y, "EPSG:27700")

  # TODO: do we need to do the step mentioned in the comment below?:
  # Subsetting 'rnet_xp' to include only those features that are within the buffer created around 'rnet_yp'.

  params = list(
                list(
                  source = rnet_yp,
                  target = rnet_xp,
                  attribute = "all_fastest_bicycle_go_dutch",
                  new_name = "all_fastest_bicycle_go_dutch",
                  agg_fun = sum,
                  weights = c("target_weighted")
                )
              )

  results_list = purrr::map(params, function(p) {
    anime_join(
      source_data = p$source,
      target_data = p$target,
      attribute = p$attribute,
      new_name = p$new_name,
      agg_fun = p$agg_fun,
      weights = p$weights,
      angle_tolerance = 35,
      distance_tolerance = 15
    )
  })

  rnet_merged_all = reduce(results_list, function(x, y) {
    left_join(x, y, by = "id")
  }, .init = rnet_xp)

  # Remove unnecessary columns from the merged spatial object
  rnet_merged_all = rnet_merged_all[, !(names(rnet_merged_all) %in% c("identifier", "length_x"))]

  # Remove Z and M dimensions (if they exist) and set geometry precision
  has_Z_or_M = any(sf::st_dimension(rnet_merged_all) %in% c("XYZ", "XYM", "XYZM"))

  # If Z or M dimensions exist, remove them and print a message
  if (has_Z_or_M) {
    rnet_merged_all = sf::st_zm(rnet_merged_all, what = "ZM")
    cat("Z or M dimensions have been removed from rnet_merged_all.\n")
  }

  # Set the precision of geometries in 'rnet_merged_all' to 1e3 (0.001)
  rnet_merged_all$geometry = sf::st_set_precision(rnet_merged_all$geometry, 1e3)

  # Round all numeric columns in 'rnet_merged_all' to 0 decimal places
  rnet_merged_all = rnet_merged_all |>
    mutate(across(where(is.numeric), ~ round(.x, 0)))

  # Prepare a list of columns to check for NA, excluding 'geometry'
  rnet_yp_list = as.list(names(rnet_yp))
  columns_to_check = unlist(rnet_yp_list[rnet_yp_list != "geometry"])

  # Filter out rows in 'rnet_merged_all' where all specified are NA
  rnet_merged_all = rnet_merged_all |>
    dplyr::filter_at(columns_to_check, any_vars(!is.na(.)))

  # Converting the projected geometry into a GEOS geometry. GEOS is a library used for spatial operations.
  rnet_merged_all_geos = geos::as_geos_geometry(rnet_merged_all)

  # Creating a buffer around the GEOS geometry. This expands the geometry by a specified distance (in meters).
  rnet_merged_all_geos_buffer = geos::geos_buffer(rnet_merged_all_geos, distance = 30, params = geos::geos_buffer_params(quad_segs = 4))

  # Converting the buffered GEOS geometry back to an sf object.
  rnet_merged_all_projected_buffer = sf::st_as_sf(rnet_merged_all_geos_buffer)

  # Subsetting another dataset 'rnet_y' based on the spatial relation with 'rnet_merged_all_buffer'.
  # It selects features from 'rnet_y' that are within the boundaries of 'rnet_merged_all_buffer'.
  # rnet_y_subset = sf::st_intersection(rnet_yp, rnet_merged_all_projected_buffer)
  # browser()
  rnet_yp_points = sf::st_point_on_surface(rnet_yp)
  rnet_yp_points_subset = rnet_yp_points[rnet_merged_all_projected_buffer, ]
  rnet_y_subset = rnet_yp[rnet_yp_points_subset, ]

  # Filter 'rnet_y' to exclude geometries within 'within_join'
  rnet_y_rest = rnet_yp[!rnet_yp$geometry %in% rnet_y_subset$geometry, ]

  # Transform the CRS of the 'rnet_merged_all' object to WGS 84 (EPSG:4326)
  rnet_merged_all = sf::st_transform(rnet_merged_all, "EPSG:4326")
  rnet_y_rest = sf::st_transform(rnet_y_rest, "EPSG:4326")

  # Combine 'rnet_y_rest' and 'rnet_merged_all' into a single dataset
  simplified_network = dplyr::bind_rows(rnet_y_rest, rnet_merged_all)

  # Remove specified columns and replace NA values with 0 in the remaining columns
  items_to_remove = c("geometry", "length_x_original", "length_x_cropped")

  cols_to_convert = names(simplified_network)[!names(simplified_network) %in% items_to_remove]
  for (col in cols_to_convert) {
    simplified_network[[col]][is.na(simplified_network[[col]])] = 0
  }

  simplified_network
}
