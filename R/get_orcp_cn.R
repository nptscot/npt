library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(igraph)

orcp_network = function(area, NPT_zones, length_threshold = 10000, percentile_value = 0.6) {

    osm = osmactive::get_travel_network("Scotland", boundary = area, boundary_type = "clipsrc")
    cycle_net = osmactive::get_cycling_network(osm)
    drive_net = osmactive::get_driving_network_major(osm)
    cycle_net = osmactive::distance_to_road(cycle_net, drive_net)
    cycle_net = osmactive::classify_cycle_infrastructure(cycle_net)
    # filter cycle_net based on column bicycle is yes dismount adn designated
    cycle_net = cycle_net |>
      dplyr::filter(bicycle %in% c("yes", "dismount", "designated")) |>
      dplyr::filter(cycle_segregation == "Separated cycle track") |>
      dplyr::mutate(length = as.numeric(sf::st_length(geometry))) |>
      dplyr::filter(length > 1) |>
      sf::st_transform(crs = 27700)

    cycle_net = st_cast(cycle_net, "LINESTRING")  
    cycle_net = cycle_net |> select(geometry)
    cycle_net$length = st_length(cycle_net)
    cycle_net_components = find_component(cycle_net, threshold = 1)

    # Get the list of all unique components
    unique_components = unique(cycle_net_components$component)

    # Initialize an empty list to store the cleaned components
    cycle_net_components_clean = list()

    # Loop over each unique component
    for (comp in unique_components) {
      # Filter for the current component and select the geometry column
      component_data = cycle_net_components |>
                        filter(component == comp) |>
                        select(geometry)

      # Calculate the total length after performing a spatial union of the geometries
      total_length <- sum(st_length(st_union(component_data)))

      tryCatch({
        # Check if total length is more than 1000 meters
        if (total_length > units::set_units(200, "meters")) {
          # Clean the data by removing dangles if length condition is met
          cleaned_data <- remove_dangles(component_data)
          # Store only the geometry data in the list
          cycle_net_components_clean[[length(cycle_net_components_clean) + 1]] <- cleaned_data
        }
      }, error = function(e) {
        # If an error occurs, print a custom error message
        print(sprintf("Failed to process component %d due to error: %s", comp, e$message))
      })

    }

    cycle_net_clean <- do.call(rbind, cycle_net_components_clean)
    snapped_lines = sf::st_snap(cycle_net_clean, cycle_net_clean, tolerance = 20)
    snapped_lines_compoenet = find_component(snapped_lines, threshold = 1)
    snapped_lines_compoenet_buffer = sf::st_buffer(snapped_lines_compoenet, dist = 30)
    cycle_net_clean_components = st_join(cycle_net_clean, snapped_lines_compoenet_buffer, join = st_intersects)


    cycle_net_clean_length = cycle_net_clean_components |>
        dplyr::group_by(component) |>
        dplyr::summarize(
                          total_length = sum(as.numeric(sf::st_length(geometry)), na.rm = TRUE))  

    min_percentile_value = stats::quantile(cycle_net_clean_length$total_length, probs = 0.8, na.rm = TRUE)
    cycle_net_f = cycle_net_clean_length |> dplyr::filter(total_length > min_percentile_value)

    filtered_OS_zones = cycle_net_f |> 
                        sf::st_transform(27700) |> 
                        sf::st_zm()

    cycle_net_NPT = stplanr::rnet_merge(filtered_OS_zones, NPT_zones, , max_angle_diff = 10, dist = 1, segment_length = 5, funs = list(all_fastest_bicycle_go_dutch = mean))       

    # cycle_net_NPT = sf::st_join(filtered_OS_zones, NPT_zones, join = st_intersects)

    summarized_data = cycle_net_NPT |>
        dplyr::group_by(component) |>
        dplyr::summarize(mean_all_fastest_bicycle_go_dutch = mean(all_fastest_bicycle_go_dutch, na.rm = TRUE),
                         total_length = sum(as.numeric(sf::st_length(geometry)), na.rm = TRUE))

    min_percentile_value = stats::quantile(summarized_data$mean_all_fastest_bicycle_go_dutch, probs = 0.6, na.rm = TRUE)
    
    summarized_data = summarized_data |> dplyr::filter(mean_all_fastest_bicycle_go_dutch > min_percentile_value)

    return(summarized_data)
}

create_line <- function(pt1, pt2) {
  st_sfc(st_linestring(rbind(st_coordinates(pt1), st_coordinates(pt2))), crs = st_crs(pt1))
}

find_component= function(rnet, threshold = 50) {

  sf::st_crs(rnet) = 27700

  # Calculate the distance matrix between features
  dist_matrix = sf::st_distance(rnet)

  # Convert the threshold to units of meters
  threshold = units::set_units(threshold, "m")

  # Create a connectivity matrix where connections are based on the threshold distance
  
  connectivity_matrix = Matrix::Matrix(dist_matrix < threshold, sparse = TRUE)

  # Create an undirected graph from the adjacency matrix
  graph = igraph::graph_from_adjacency_matrix(connectivity_matrix, mode = "undirected", diag = FALSE)

  # Find the connected components in the graph
  components = igraph::components(graph)

  # Assign component membership to the road network
  rnet$component = components$membership

  # Return the updated road network with component membership
  return(rnet)
}

find_endpoints = function(rnet) {
  # Remove duplicate lines in rnet
  merged_lines = sf::st_union(rnet)
  
  # Cast to LINESTRING
  merged_lines = sf::st_cast(merged_lines, "LINESTRING")

  # Create an empty list to store points
  junctions = list()
  
  # Extract the first and last coordinates of each line
  all_points = do.call(rbind,
    lapply(sf::st_geometry(merged_lines), function(line) {
      coords = sf::st_coordinates(line)
      rbind(coords[1, , drop = FALSE], coords[nrow(coords), , drop = FALSE])
    })
  )
  
  if (is.matrix(all_points)) {
    all_points = as.data.frame(all_points)
  }
  
  all_points = all_points[, 1:2]
  names(all_points) = c("X", "Y")

  # Count occurrences of each point
  points_counted = all_points |>
    dplyr::group_by(X, Y) |>
    dplyr::summarise(Count = dplyr::n(), .groups = 'drop')
  
  # Filter for unique points (occurring only once)
  unique_points = points_counted |>
    dplyr::filter(Count == 1)
  
  # Convert to sf object
  points_sf = sf::st_as_sf(unique_points, coords = c("X", "Y"), crs = sf::st_crs(rnet))

  # use point_sf crete conve hull
  hull_sf = st_convex_hull(st_combine(points_sf))
  points_not_within_hull <- points_sf[!st_within(points_sf, hull_sf, sparse = FALSE), ]

  return(points_sf)
}


find_nearest_points = function(points_sf, rnet, segment_length = 10, dist = 100) {
    rnet = stplanr::line_cast(rnet)
    rnet = stplanr::line_segment(rnet, segment_length = segment_length)   
    rnet_points_df = as.data.frame(sf::st_coordinates(rnet))
    names(rnet_points_df) = c("X", "Y", "L1")
    rnet_points_sf = st_as_sf(rnet_points_df, coords = c("X", "Y"), crs = st_crs(rnet))
    points_sf_buffer = st_buffer(points_sf, dist = dist)
    rnet_points_sf_within_buffer = sf::st_intersection(rnet_points_sf, points_sf_buffer)
    # Find the nearest point in os_points_sf_within_buffer for each point in points_sf
    nearest_points = nngeo::st_nn(points_sf, rnet_points_sf_within_buffer, k = 1, returnDist = TRUE)

    # Extract the nearest points and distances
    nearest_indices = unlist(nearest_points$nn)
    nearest_distances = unlist(nearest_points$dist)

    # Ensure nearest_indices are within the range of points_sf
    if (all(nearest_indices <= nrow(rnet_points_sf_within_buffer))) {
    # Add the index of points_sf to the nearest_os_points
    nearest_os_points = rnet_points_sf_within_buffer[nearest_indices, ]
    nearest_os_points$index_points_sf = 1:length(nearest_indices)
    } else {
    stop("The nearest_indices are out of bounds. Please check your data.")
    }
    return(nearest_os_points)
}

calculate_paths_from_point_dist = function(network, point, target_point, path_type = "shortest") {
    # Convert points to sfc
    point_geom = sf::st_as_sfc(point, crs = st_crs(network))
    target_geom = sf::st_as_sfc(target_point, crs = st_crs(network))
    
    # Find nearest network nodes
    start_node = sf::st_nearest_feature(network, point_geom)
    end_node = sf::st_nearest_feature(network, target_geom)

    # Ensure network is in the right format and calculate weights
    network = network |>
        sf::st_cast("LINESTRING") |>
        sfnetworks::as_sfnetwork(directed = FALSE) |>
        sfnetworks::activate("edges") |>
        dplyr::mutate(weight = sf::st_length(geometry))
    
    # Calculate paths using weights
    paths = sfnetworks::st_network_paths(
        network, 
        from = start_node, 
        to = end_node, 
        weights = "weight",
        type = path_type
    )
    
    # Extract the edge paths
    edges_in_paths = paths |>
        dplyr::pull(edge_paths) |>
        unlist() |>
        unique()
    
    # Result as an sf object
    result = network |>
        dplyr::slice(edges_in_paths) |>
        sf::st_as_sf()
    
    return(result)
}

compute_shortest_paths = function(points_sf, osm_points, rnet, segment_length = 10) {
  hull_sf = st_convex_hull(st_combine(osm_points))
  buffer_distance = 500 
  buffered_hull_sf = st_buffer(hull_sf, dist = buffer_distance)

  rnet = rnet[sf::st_union(buffered_hull_sf), , op = sf::st_intersects]  

  # Process the OSM data
  rnet = stplanr::line_cast(rnet)
  rnet_split = stplanr::line_segment(rnet, segment_length = segment_length)
  
  # Build the network
  network = rnet_split |>
    sf::st_cast("LINESTRING") |>
    sf::st_transform(27700) |>
    sfnetworks::as_sfnetwork(directed = FALSE) |>
    sfnetworks::activate("edges") |>
    dplyr::mutate(weight = sf::st_length(geometry))
  
  # Convert sfnetwork to igraph for component analysis
  network_igraph = as.igraph(network, directed = FALSE)
  components = igraph::components(network_igraph)
  
  # Assign component IDs to nodes and edges
  network = network |>
    activate("nodes") |>
    mutate(component_id = as.numeric(components$membership))
  
  network = network |>
    activate("edges") |>
    mutate(component_id = as.numeric(components$membership[head_of(network_igraph, E(network_igraph))]))
  
  # Initialize an empty list to store paths
  all_paths = list()
  
  # Loop through all pairs of points
  for (i in 1:nrow(points_sf)) {
    start_point = points_sf[i, ]
    end_point = osm_points |> filter(index_points_sf == i)
    
    start_node = sf::st_nearest_feature(start_point, network |> activate("nodes") |> st_as_sf())
    end_node = sf::st_nearest_feature(end_point, network |> activate("nodes") |> st_as_sf())
    
    # Compute the shortest path
    paths_from_point = sfnetworks::st_network_paths(
      network,
      from = start_node,
      to = end_node,
      weights = "weight",
      type = "shortest"
    )
    
    # Extract edges from the path for visualization
    edges_in_path = network |>
      activate("edges") |>
      filter(row_number() %in% unlist(paths_from_point$edge_paths[[1]])) |>
      sf::st_as_sf()
    
    # Store the path information in the list
    all_paths[[i]] = list(
      start_point = start_point,
      end_point = end_point,
      path_edges = edges_in_path,
      start_node_geom = network |> activate("nodes") |> slice(start_node) |> st_as_sf(),
      end_node_geom = network |> activate("nodes") |> slice(end_node) |> st_as_sf()
    )
  }
  
  return(all_paths)
}

fix_net_connectivity <- function(file_name, input_dir, output_dir, gisBase, gisDbase, location, mapset) {
  library(rgrass)

  # Initialize GRASS GIS environment
  initGRASS(gisBase = gisBase, 
            gisDbase = gisDbase, 
            location = location, 
            mapset = mapset, 
            override = TRUE)
  
  execGRASS("g.gisenv", flags = "s")
  
  # Construct input and output file paths
  input_file <- glue::glue("{input_dir}/{file_name}")
  output_file <- glue::glue("{output_dir}/{file_name}_fixed.geojson")
  
  # Import network data from GeoJSON
  execGRASS("v.in.ogr", flags = c("overwrite", "o"), 
            parameters = list(input = input_file, 
                              output = "network"))
  
  # Clean the network
  execGRASS("v.clean", flags = c("overwrite"),
            parameters = list(input = "network", 
                              output = "fixed_network", 
                              tool = "break", 
                              threshold = 10))
  
  # Export the cleaned network to GeoJSON
  execGRASS("v.out.ogr", flags = c("overwrite"), 
            parameters = list(input = "fixed_network", 
                              output = output_file, 
                              format = "GeoJSON"))
  
  cat("Network", output_file, " is cleaned and export completed successfully.\n")
}

library(sf)
library(igraph)
library(geosphere)
library(igraph)

sf_to_igraph = function(network_sf) {
  # network_sf = cycle_net_components_1
  # Assuming network_sf is LINESTRING
  points = unique(do.call(rbind, lapply(network_sf$geometry, function(line) {
    rbind(st_coordinates(line)[1, ], st_coordinates(line)[nrow(st_coordinates(line)), ])
  })))
  points = unique(points)
  edges = do.call(rbind, lapply(network_sf$geometry, function(line) {
    start = which(points[,1] == st_coordinates(line)[1,1] & points[,2] == st_coordinates(line)[1,2])
    end = which(points[,1] == st_coordinates(line)[nrow(st_coordinates(line)),1] & points[,2] == st_coordinates(line)[nrow(st_coordinates(line)),2])
    c(start, end)
  }))
  graph = graph_from_edgelist(as.matrix(edges), directed = FALSE)
  return(graph)
}

remove_dangles = function(network, percentile = 0.012) {

    network$length = sf::st_length(network)
    network_g = sf_to_igraph(network)
    vertex_degrees = degree(network_g)

    dangle_vertices = which(vertex_degrees == 1)

    points = unique(do.call(rbind, lapply(st_geometry(network), function(line) {
    rbind(st_coordinates(line)[1, ], st_coordinates(line)[nrow(st_coordinates(line)), ])
    })))
    # Extract indices of LINESTRINGs to check
    line_indices = sapply(st_geometry(network), function(line) {
    coords = st_coordinates(line)
    c(which(points[,1] == coords[1,1] & points[,2] == coords[1,2]),
        which(points[,1] == coords[nrow(coords),1] & points[,2] == coords[nrow(coords),2]))
    })

    # sum of network$length
    sum_value = sum(network$length)
    
    # Median of network$length
    threshold_length = percentile* sum_value
    short_dangles = network[(line_indices[1,] %in% dangle_vertices | line_indices[2,] %in% dangle_vertices) & network$length < units::set_units(threshold_length, "meters"), ]

    network_clean = network[!((line_indices[1,] %in% dangle_vertices | line_indices[2,] %in% dangle_vertices) & network$length <  units::set_units(threshold_length, "meters")), ]
}