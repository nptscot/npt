#' Subset a networkmin_
#' 
#' The input dataset must contain a column named `value` which
#' determines the most important segments of the network. 
#' 
#' @param x A network object of class `sf`
#' @param argument2 A second argument
#' @examples
#' res = subset_network(example_network)
#' plot(res)
#' if(FALSE) {
#' network_tile = sf::read_sf("./data-raw/network_tile.geojson")
#' names(network_tile)
#' network_tile$value = network_tile$dutch_slc
#' network_tile_subset = subset_network(network_tile)
#' plot(network_tile$geometry, col = "grey")
#' plot(network_tile_subset$geometry, col = "red", add = TRUE)
#' }
#' @export
cohesive_network = function(network_tile, combined_grid_buffer, base_value = "all_fastest_bicycle_go_dutch", crs = "EPSG:27700", min_percentile = 0.90 , arterial = FALSE, dist =10) {

    # Transform the coordinates of the spatial object
    network_tile_transformed = sf::st_transform(network_tile, crs)

    network_tile_transformed = network_tile_transformed |>
      dplyr::mutate(
        value = dplyr::case_when(
          roadClassification == 'A Road' ~ .data[[base_value]] * 1.5,
          roadClassification == 'B Road' ~ .data[[base_value]] * 1.2,
          TRUE ~ .data[[base_value]]
        )
      )

    network_tile_transformed = network_tile_transformed[!is.na(network_tile_transformed$value), ]
    network_tile_transformed$averageWidth[is.na(network_tile_transformed$averageWidth)] = 0.1

    grid_sf = combined_grid_buffer

    grid_sf$grid_id = seq_len(nrow(grid_sf))

    split_network = sf::st_intersection(network_tile_transformed, grid_sf)

    select_by_density = function(density) {
      if (density < 10) {
        top_n = 6
      } else if (density >= 10 & density < 20) {
        top_n = 10
      } else {
        top_n = (density %/% 10) + 12
      }
      
      # Check if the value is NA (rest of the cases) and set top_n to 1
      if (is.na(density)) {
        top_n = 1
      }
      return(top_n)
    }

    distance_threshold = units::set_units(dist, "m") # Adjust the distance as needed

    split_network = split_network |>
      dplyr::group_by(grid_id) |>
      dplyr::mutate(top_n = select_by_density(unique(density)))


    split_network_max = split_network |>
      dplyr::group_by(grid_id) |>
      dplyr::group_modify(~ {
        # Step 1: Filter for the current grid_id
        split_network_filtered = .x
        
        # Step 2: Calculate Centroids for These Elements
        centroids = sf::st_centroid(split_network_filtered)
        
        # Step 3: Sort the Centroids Based on a Value Attribute
        sorted_centroids = centroids |>
          dplyr::arrange(desc(value))
        
        # Step 4: Identify the Centroid with the Highest Value
        highest_value_centroid = sorted_centroids[1, ]
        
        # Step 5: Calculate Distances from Each Centroid to the Highest Value Centroid
        distances_to_highest = sf::st_distance(centroids, highest_value_centroid)

        # Step 6: Select Line Strings Based on top_n and Distance Criteria
        top_n = unique(split_network_filtered$top_n)

        selected_lines = split_network_filtered |>
          dplyr::mutate(distance_to_highest = sf::st_distance(centroids, highest_value_centroid)[,1]) |>
          dplyr::filter(distance_to_highest >= distance_threshold) |>
          dplyr::slice_head(n = top_n)        
        return(selected_lines)
      }) |> 
      dplyr::bind_rows()

    # Calculate centroids of these max segments
    split_network_max = sf::st_as_sf(split_network_max)
    centroids_df = sf::st_centroid(split_network_max)

    if (arterial) {
      # Filter the dataset for arterial roads
      min_percentile_value = stats::quantile(network_tile_transformed$value, probs = min_percentile)
      network_tile_linestring_percentile = dplyr::filter(network_tile_transformed, value > min_percentile_value)
      network_tile_linestring_percentile = dplyr::filter(network_tile_linestring_percentile, arterial == 'Yes')

    } else {
      # Calculate the minimum percentile value from the 'value' column for non-arterial roads
      min_percentile_value = stats::quantile(network_tile_transformed$value, probs = min_percentile)
      # Filter the dataset to include only rows where 'value' is greater than the minimum percentile value
      network_tile_linestring_percentile = dplyr::filter(network_tile_transformed, value > min_percentile_value)
    }

    # Prepare network
    print("Preparing network")
    net_percentile = prepare_network(network_tile_linestring_percentile, crs)

    # Calculate paths for each centroid
    all_paths_sf_percentile = purrr::map_dfr(
        seq_len(nrow(centroids_df)),
        function(n) {
            calculate_paths_from_point(net_percentile, centroids_df[n, ], centroids_df)
        }
    )

  # TODO: more effecient way to do this
  all_paths_sf_percentile_duplicated_geoms = duplicated(all_paths_sf_percentile$geometry)
  all_paths_sf_percentile = all_paths_sf_percentile[!all_paths_sf_percentile_duplicated_geoms, ]

  largest_component = calculate_largest_component(all_paths_sf_percentile)
  largest_component_sf = sf::st_as_sf(largest_component, 'edges')
    return(largest_component_sf)
}

prepare_network = function(network, transform_crs = 27700) {
    network = network |>
        sf::st_cast("LINESTRING") |>
        sfnetworks::as_sfnetwork(directed = FALSE) |>
        sf::st_transform(transform_crs) |>
        sfnetworks::activate("edges") |>
        dplyr::mutate(arterialness = calculate_arterialness_score(highway, bicycle, network),
                      # Higher arterialness_score leads to lower weight
                      weight = max(arterialness) + 1 - arterialness)

    return(network)
}


# Function to calculate paths from a point
calculate_paths_from_point = function(network, point, centroids) {
  # Ensure point_key is a single element
  # Convert point to a string representation that uniquely identifies it
  path_cache = list()
  point_key = paste(sort(as.character(point)), collapse = "_")

  # Check if paths for this point are already cached
  if (point_key %in% names(path_cache)) {
    return(path_cache[[point_key]])
  }

  # Calculate paths if not in cache
  paths_from_point = sfnetworks::st_network_paths(network, from = point, to = centroids, weights = "weight")
  edges_in_paths = paths_from_point |> 
    dplyr::pull(edge_paths) |> 
    base::unlist() |> 
    base::unique()
    
  result = network |> dplyr::slice(unique(edges_in_paths)) |> sf::st_as_sf()

  # Store the result in cache
  path_cache[[point_key]] = result

  # Return the result
  return(result)
}


calculate_arterialness_score = function(highway, bicycle, network_tile) {
    # Base score based on road classification
    # browser()
    base_score = dplyr::case_when(
        highway == "primary" ~ 50,
        highway == "secondary" ~ 10,
        highway == "tertiary" ~ 5,
        TRUE ~ 1  
    )

    # # Calculate max_width only if it's not already calculated

    # max_width = max(network_tile$averageWidth, na.rm = TRUE)
    # print(max_width)

    # # Normalize the width score
    # normalized_width_score = ifelse(is.numeric(averageWidth) & averageWidth > 0, 
    #                                 averageWidth / max_width, 
    #                                 0) # Handling NA and non-positive values

    # # Scaling factor
    # scaling_factor = 5
    # width_score = normalized_width_score * scaling_factor

    # Bicycle preference scoring
    # bicycle_score = dplyr::case_when(
    #     bicycle %in% c("yes", "designated", "permissive", "mtb") ~ 3, # High preference score
    #     bicycle %in% c(NA, "unknown") ~ 2, # Low preference score, treating NA as 'dismount'
    #     bicycle %in% c("customers", "dismount", "private") ~ 1, # Low preference score, treating NA as 'dismount'
    #     TRUE ~ 0  # Default score if none of the conditions are met
    # )
    
    # # You might adjust the scaling factor or add additional logic based on your requirements
    # scaling_factor = 5
    # adjusted_bicycle_score = bicycle_score * scaling_factor

    # Return the total arterialness score
    # return(base_score + adjusted_bicycle_score)
    return(base_score)
}

create_buffers_and_grid = function(city_name, num_buffers, grid_sizes) {

  zones = zonebuilder::zb_zone(city_name)

  centriod_of_city = zones$centroid[1]
  centriod_of_city = sf::st_transform(centriod_of_city, crs = 27700)

  create_square_buffer = function(point, distance) {
    # Extract coordinates of the point
    coords = st_coordinates(point)
    
    # Calculate coordinates of the square's corners
    x = coords[,1]
    y = coords[,2]

    square_coords = matrix(c(x - distance, y + distance,  # top left
                              x + distance, y + distance,  # top right
                              x + distance, y - distance,  # bottom right
                              x - distance, y - distance,  # bottom left
                              x - distance, y + distance), # to close the square
                            ncol = 2, byrow = TRUE)
    
    # Create a polygon and return
    st_polygon(list(square_coords))
  }

  generate_grid = function(buffer_sf, grid_size) {
    # Check if buffer_sf is an sf object
    if (!inherits(buffer_sf, "sf")) {
      stop("The buffer must be an sf object.")
    }

    # Extract the geometry
    geom = st_geometry(buffer_sf)
    
    # Get the bounding box of the geometry
    bbox = st_bbox(geom)

    # Create a sequence of points for the grid
    x_points = seq(from = bbox$xmin, to = bbox$xmax, by = grid_size)
    y_points = seq(from = bbox$ymin, to = bbox$ymax, by = grid_size)

    # Create grid polygons
    grid_polys = vector("list", length = (length(x_points) - 1) * (length(y_points) - 1))
    index = 1
    for (i in 1:(length(x_points) - 1)) {
      for (j in 1:(length(y_points) - 1)) {
        grid_polys[[index]] = st_polygon(list(rbind(
          c(x_points[i], y_points[j]),
          c(x_points[i + 1], y_points[j]),
          c(x_points[i + 1], y_points[j + 1]),
          c(x_points[i], y_points[j + 1]),
          c(x_points[i], y_points[j])
        )))
        index = index + 1
      }
    }

    # Combine into a single simple feature collection
    grid_sfc = st_sfc(grid_polys, crs = st_crs(buffer_sf))
    
    return(grid_sfc)
  }

  buffer_list = list()
  buffer_list_dif = list()
  grid_buffer_list = list()
  grid_buffer_list_sf = list()
  
  for (i in 1:num_buffers) {
      buffer = create_square_buffer(centriod_of_city, distance = i * 1000)
      buffer_list[[i]] = buffer
  }

  buffer_list_dif[[1]] = buffer_list[[1]]
  for (i in num_buffers:2) {      
    buffer_list_dif[[i]] = st_difference(buffer_list[[i]], buffer_list[[i - 1]])
  }

  for (i in 1:num_buffers) {
    buffer_sf = st_sf(geometry = st_sfc(buffer_list[[i]]))
    grid = generate_grid(buffer_sf, grid_sizes[i])
    grid_buffer_list[[i]] = st_intersection(grid, buffer_list_dif[[i]])
    
  }

  combined_grid_buffer = rbind(grid_buffer_list)
  return(combined_grid_buffer)
}


convert_to_numeric = function(values) {
  # Initialize an empty vector to store numeric values
  numeric_values = numeric(length(values))
  
  # Loop through each value in the input vector
  for (i in seq_along(values)) {
    value = values[i]
    
    # Check for NA, NULL, or length zero for each element
    if (is.null(value) || length(value) == 0) {
      numeric_values[i] = NA
    } else {
      # Extract numbers and convert to numeric
      numbers = as.numeric(unlist(regmatches(value, gregexpr("[[:digit:]]+", value))))
      if (length(numbers) > 0) {
        numeric_values[i] = numbers[1]  # Store the first number
      } else {
        numeric_values[i] = NA  # Store NA if no numbers are found
      }
    }
  }
  
  return(numeric_values)
}

calculate_largest_component = function(network_tile) {
  # Convert sf object (network_tile) to sfnetwork
  network_tile_sfn = sfnetworks::as_sfnetwork(network_tile)

  # Convert sfnetwork to igraph
  network_tile_igraph = tidygraph::as_tbl_graph(network_tile_sfn)

  # Find the components
  components = igraph::components(network_tile_igraph)

  # Identify the largest component
  largest_component = which.max(components$csize)
  largest_component_nodes = which(components$membership == largest_component)

  # Extract the largest component subgraph
  largest_component_subgraph = igraph::subgraph(network_tile_igraph, largest_component_nodes)

  # Convert the largest component subgraph back to sfnetwork
  largest_component_sfn = sfnetworks::as_sfnetwork(largest_component_subgraph)

  # Return the sfnetwork of the largest component
  return(largest_component_sfn)
}
