#' Function To simplify rnets
#' @param combined_network combined_network
#' @param parameters parameters

simplify_network = function(rnet_y, parameters){
  # Read spatial data directly from URLs into sf objects
  # TODO: use small dataset if open data build is TRUE
  if (parameters$open_data_build) {
    rnet_x = geojsonsf::geojson_sf("https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_x_ed.geojson")
    rnet_x_buffers = sf::st_buffer(rnet_x, dist = 20, endCapStyle = "FLAT")
    single_rnet_x_buffer = sf::st_union(rnet_x_buffers)
    rnet_x_buffer = sf::st_sf(geometry = single_rnet_x_buffer)
    rnet_x_buffer = sf::st_make_valid(rnet_x_buffer)
  } else {
    # URL for the original route network
    url_rnet_x = "https://github.com/nptscot/networkmerge/releases/download/v0.1/OS_Scotland_Network.geojson"
    f_rnet_x = basename(url_rnet_x)
    if (!file.exists(f_rnet_x)) {
      download.file(url_rnet_x, f_rnet_x, method = "libcurl")
    }
    if (file.exists(f_rnet_x) && file.size(f_rnet_x) > 0) {
      rnet_x = geojsonsf::geojson_sf(f_rnet_x)
    } else {
      stop("File download failed or file is empty for rnet_x")
    }
    
  }
  
  # Transform the spatial data to a different coordinate reference system (EPSG:27700)
  # TODO: uncomment:
  rnet_xp = sf::st_transform(rnet_x, "EPSG:27700")
  rnet_yp = sf::st_transform(rnet_y, "EPSG:27700")

  # Removing lines that not exist in combined_network_tile
  # Creating a buffer around the GEOS geometry 'rnet_yp'. This expands the geometry by a specified distance (15 meters in this case).
  rnet_yp_buffer = geos::geos_buffer(rnet_yp, distance = 15, params = geos::geos_buffer_params(quad_segs = 4))

  # Converting the buffer created above into an 'sf' (simple features) object. This is necessary because the buffer created by 'geos_buffer'
  rnet_yp_buffer = sf::st_as_sf(rnet_yp_buffer)

  # Subsetting 'rnet_xp' to include only those features that are within the buffer created around 'rnet_yp'.
  rnet_xp = rnet_xp[rnet_yp_buffer, , op = sf::st_within]

  # Extract column names from the rnet_yp
  name_list = names(rnet_yp)
  
  # Initialize an empty list
  funs = list()
  
  # Loop through each name and assign it a function based on specific conditions
  for (name in name_list) {
    if (name == "geometry") {
      next  # Skip the current iteration
    } else if (name %in% c("Gradient", "Quietness")) {
      funs[[name]] = mean
    } else {
      funs[[name]] = sum
    }
  }
  
  # Merge the spatial objects rnet_xp and rnet_yp based on specified parameters
  dist = 20
  angle = 15
  rnet_merged_all = stplanr::rnet_merge(rnet_xp, rnet_yp, dist = dist, segment_length = 10, funs = funs, max_angle_diff = angle)  #
  
  # Remove unnecessary columns from the merged spatial object
  rnet_merged_all = rnet_merged_all[ , !(names(rnet_merged_all) %in% c('identifier','length_x'))]
  
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
  rnet_merged_all = rnet_merged_all %>%
    mutate(across(where(is.numeric), ~ round(.x, 0)))
  
  # Prepare a list of columns to check for NA, excluding 'geometry'
  rnet_yp_list = as.list(names(rnet_yp))
  columns_to_check = unlist(rnet_yp_list[rnet_yp_list != "geometry"])
  
  # Filter out rows in 'rnet_merged_all' where all specified are NA
  rnet_merged_all = rnet_merged_all %>%
    dplyr::filter_at(columns_to_check, any_vars(!is.na(.)))
      
  # Selecting only the geometry column from the 'rnet_merged_all' dataset.
  rnet_merged_all_only_geometry = rnet_merged_all %>% dplyr::select(geometry)
  
  # Merging all geometries into a single geometry using st_union from the sf package.
  rnet_merged_all_union = sf::st_union(rnet_merged_all_only_geometry)
  
  # Transforming the merged geometry to a specific coordinate reference system (CRS), EPSG:27700.
  rnet_merged_all_projected = sf::st_transform(rnet_merged_all_union, "EPSG:27700")
  
  # Converting the projected geometry into a GEOS geometry. GEOS is a library used for spatial operations.
  rnet_merged_all_geos = geos::as_geos_geometry(rnet_merged_all_projected)
  
  # Creating a buffer around the GEOS geometry. This expands the geometry by a specified distance (in meters).
  rnet_merged_all_geos_buffer = geos::geos_buffer(rnet_merged_all_geos, distance = 30, params = geos::geos_buffer_params(quad_segs = 4))
  
  # Converting the buffered GEOS geometry back to an sf object.
  rnet_merged_all_projected_buffer = sf::st_as_sf(rnet_merged_all_geos_buffer)
  
  # Transform the coordinate reference system of 'rnet_merged_all' to WGS 84 (EPSG:4326).
  rnet_merged_all_buffer = sf::st_transform(rnet_merged_all_projected_buffer, "EPSG:4326")
  
  # Subsetting another dataset 'rnet_y' based on the spatial relation with 'rnet_merged_all_buffer'.
  # It selects features from 'rnet_y' that are within the boundaries of 'rnet_merged_all_buffer'.
  rnet_y_subset = rnet_y[rnet_merged_all_buffer, , op = sf::st_within]
  
  # Filter 'rnet_y' to exclude geometries within 'within_join'
  rnet_y_rest = rnet_y[!rnet_y$geometry %in% rnet_y_subset$geometry, ]
  
  # Transform the CRS of the 'rnet_merged_all' object to WGS 84 (EPSG:4326)
  rnet_merged_all = sf::st_transform(rnet_merged_all, "EPSG:4326")
  
  # Combine 'rnet_y_rest' and 'rnet_merged_all' into a single dataset
  simplified_network = dplyr::bind_rows(rnet_y_rest, rnet_merged_all)
  
  # Remove specified columns and replace NA values with 0 in the remaining columns
  items_to_remove = c('geometry', 'length_x_original', 'length_x_cropped')
  
  cols_to_convert = names(simplified_network)[!names(simplified_network) %in% items_to_remove]
  for (col in cols_to_convert) {
    simplified_network[[col]][is.na(simplified_network[[col]])] = 0
  }
  
  simplified_network
}