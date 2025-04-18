#' Function To simplify rnets
#' @param combined_network combined_network
#' @param parameters parameters

simplify_network = function(rnet_y, parameters, region_boundary) {

  region_snake_case = snakecase::to_snake_case(parameters$region[[1]])
  base_name = paste0("OS_Scotland_Network_", region_snake_case, ".geojson")
  rnet_x_f = file.path("inputdata", base_name)

  if (!dir.exists("inputdata")) {
    dir.create("inputdata")
  }

  if (!file.exists(rnet_x_f)) {
    stop("Download the latest inputdata")
  }

  rnet_x = sf::read_sf(rnet_x_f) |>
    sf::st_transform(crs = "EPSG:27700")

  rnet_xp = sf::st_transform(rnet_x, "EPSG:27700")
  rnet_yp = sf::st_transform(rnet_y, "EPSG:27700")


  name_list = names(rnet_yp)
  funs = list()

  for (name in name_list) {
    if (name == "geometry") {
      next 
    } else if (name %in% c("gradient", "quietness")) {
      funs[[name]] = mean
    } else {
      funs[[name]] = sum
    }
  }

  dist = 25
  angle = 35
  rnet_merged_all = stplanr::rnet_merge(rnet_xp, rnet_yp, dist = dist, funs = funs, max_angle_diff = angle, segment_length = 20)

  rnet_merged_all = rnet_merged_all[, !(names(rnet_merged_all) %in% c("identifier", "length_x"))]

  has_Z_or_M = any(sf::st_dimension(rnet_merged_all) %in% c("XYZ", "XYM", "XYZM"))

  if (has_Z_or_M) {
    rnet_merged_all = sf::st_zm(rnet_merged_all, what = "ZM")
    cat("Z or M dimensions have been removed from rnet_merged_all.\n")
  }

  rnet_merged_all$geometry = sf::st_set_precision(rnet_merged_all$geometry, 1e3)

  rnet_merged_all = rnet_merged_all |>
    mutate(across(where(is.numeric), ~ round(.x, 0)))

  rnet_yp_list = as.list(names(rnet_yp))
  columns_to_check = unlist(rnet_yp_list[rnet_yp_list != "geometry"])

  rnet_merged_all = rnet_merged_all |>
    dplyr::filter_at(columns_to_check, any_vars(!is.na(.)))

  rnet_merged_all_geos = geos::as_geos_geometry(rnet_merged_all)

  rnet_merged_all_geos_buffer = geos::geos_buffer(rnet_merged_all_geos, distance = 30, params = geos::geos_buffer_params(quad_segs = 4))

  rnet_merged_all_projected_buffer = sf::st_as_sf(rnet_merged_all_geos_buffer)

  rnet_yp_points = sf::st_point_on_surface(rnet_yp)
  rnet_yp_points_subset = rnet_yp_points[rnet_merged_all_projected_buffer, ]
  rnet_y_subset = rnet_yp[rnet_yp_points_subset, ]

  rnet_y_rest = rnet_yp[!rnet_yp$geometry %in% rnet_y_subset$geometry, ]

  rnet_merged_all = sf::st_transform(rnet_merged_all, "EPSG:4326")
  rnet_y_rest = sf::st_transform(rnet_y_rest, "EPSG:4326")

  simplified_network = dplyr::bind_rows(rnet_y_rest, rnet_merged_all)

  items_to_remove = c("geometry", "length_x_original", "length_x_cropped")

  cols_to_convert = names(simplified_network)[!names(simplified_network) %in% items_to_remove]
  for (col in cols_to_convert) {
    simplified_network[[col]][is.na(simplified_network[[col]])] = 0
  }

  simplified_network
}
