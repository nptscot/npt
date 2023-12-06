# Load necessary libraries
library(sf)
library(stplanr)
library(dplyr)
library(geos)

# Define the simplify_network function
simplify_network <- function(parameters, combined_network) {
  # Read spatial data
  if (parameters$open_data_build) {
    rnet_x = sf::read_sf("https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_x_ed.geojson")
    rnet_x_buffers = sf::st_buffer(rnet_x, dist = 20, endCapStyle = "FLAT")
    single_rnet_x_buffer = sf::st_union(rnet_x_buffers)
    rnet_x_buffer = sf::st_sf(geometry = single_rnet_x_buffer)
    rnet_x_buffer = sf::st_make_valid(rnet_x_buffer)
  } else {
    url_rnet_x = "https://github.com/nptscot/networkmerge/releases/download/v0.1/OS_Scotland_Network.geojson"
    f_rnet_x = basename(url_rnet_x)
    if (!file.exists(f_rnet_x)) download.file(url_rnet_x, f_rnet_x, method = "libcurl")
    if (file.exists(f_rnet_x) && file.size(f_rnet_x) > 0) {
      rnet_x = sf::read_sf(f_rnet_x)
    } else {
      stop("File download failed or file is empty for rnet_x")
    }
  }

  # Assign rnet_y from combined_network
  rnet_y = combined_network

  # Transform the spatial data to a different coordinate reference system (EPSG:27700)
  rnet_xp = sf::st_transform(rnet_x, "EPSG:27700")
  rnet_yp = sf::st_transform(rnet_y, "EPSG:27700")

  # Extract column names from rnet_yp
  name_list = names(rnet_yp)

  # Initialize an empty list for functions
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

  # Merge the spatial objects rnet_xp and rnet_yp
  dist = 20
  angle = 10
  rnet_merged_all = stplanr::rnet_merge(rnet_xp, rnet_yp, dist = dist, segment_length = 10, funs = funs, max_angle_diff = angle)

  # Remove specific columns from the merged spatial object
  rnet_merged_all = rnet_merged_all[ , !(names(rnet_merged_all) %in% c('identifier','length_x'))]

  # Remove Z and M dimensions (if they exist) and set geometry precision
  if (any(sf::st_dimension(rnet_merged_all) %in% c("XYZ", "XYM", "XYZM"))) {
    rnet_merged_all = sf::st_zm(rnet_merged_all, what = "ZM")
    cat("Z or M dimensions have been removed from rnet_merged_all.\n")
  }

  rnet_merged_all$geometry = sf::st_set_precision(rnet_merged_all$geometry, 1e3)
  rnet_merged_all = rnet_merged_all %>%
    mutate(across(where(is.numeric), ~ round(.x, 0)))      

  # Prepare a list of columns to check for NA, excluding 'geometry'
  rnet_yp_list = as.list(names(rnet_yp))
  columns_to_check = unlist(rnet_yp_list[rnet_yp_list != "geometry"])

  # Filter out rows in 'rnet_merged_all' where all specified columns are NA
  rnet_merged_all = rnet_merged_all %>%
    filter(rowSums(is.na(select(., all_of(columns_to_check)))) != length(columns_to_check))

  # Select only the geometry column
  rnet_merged_all_only_geometry = rnet_merged_all %>% select(geometry)

  # Merge all geometries into a single geometry
  rnet_merged_all_union = sf::st_union(rnet_merged_all_only_geometry)

  # Transform to a specific CRS
  rnet_merged_all_projected = sf::st_transform(rnet_merged_all_union, "EPSG:27700")

  # Convert to a GEOS geometry
  rnet_merged_all_geos = geos::as_geos_geometry(rnet_merged_all_projected)

  # Creating a buffer around the GEOS geometry
  rnet_merged_all_geos_buffer = geos::geos_buffer(rnet_merged_all_geos, distance = 30)

  # Convert the buffered GEOS geometry back to an sf object
  rnet_merged_all_projected_buffer = sf::st_as_sf(rnet_merged_all_geos_buffer)

  # Confirm buffered geometry CRS as EPSG:27700
  rnet_merged_all_buffer = sf::st_transform(rnet_merged_all_projected_buffer, "EPSG:4326")

  # Subsetting 'rnet_y' based on the spatial relation with 'rnet_merged_all_buffer'
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

  # Return the final simplified network
  return(simplified_network)
}

