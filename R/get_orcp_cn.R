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

  snapped_lines = sf::st_snap(cycle_net, cycle_net, tolerance = 15)
  group_ids = sapply(sf::st_geometry(snapped_lines), function(geometry, index, lines) {
    possible_near = sf::st_intersects(geometry, lines, sparse = FALSE)
    connected = which(possible_near)
    unioned = sf::st_union(sf::st_geometry(lines[connected, ]))
    return(unioned)
  }, index = lines_index, lines = snapped_lines)

    # Create a new sf object with merged lines
    merged_lines = sf::st_sf(geometry = do.call(sf::st_sfc, group_ids))
    merged_lines = merged_lines[!duplicated(sf::st_as_text(merged_lines$geometry)), ]    

    network = merged_lines
    network_multilines = network[sf::st_geometry_type(network) == "MULTILINESTRING", ]
    network_lines = sf::st_cast(network_multilines, "LINESTRING")

    network = network_lines
    if (!inherits(network, "sfnetwork")) {
        network_sfn = sfnetworks::as_sfnetwork(network, directed = FALSE)
    } else {
        network_sfn = network
    }

    network_igraph = tidygraph::as_tbl_graph(network_sfn)   

    components = igraph::components(network_igraph)
    component_ids = order(components$csize, decreasing = TRUE)  # top 10 components by size

    top_components_sfn = list()  # Initialize list to store sfnetwork components

    # Extract each of the top 10 components and convert them
    for (component_id in component_ids) {
        component_nodes = which(components$membership == component_id)
        component_subgraph = igraph::subgraph(network_igraph, component_nodes)
        if (length(component_nodes) > 0) {  # Check if there are nodes in the component
            component_sfn = sfnetworks::as_sfnetwork(component_subgraph, directed = FALSE)
            top_components_sfn[[component_id]] = component_sfn
        } else {
            top_components_sfn[[component_id]] = NULL
        }
    }

    valid_components = sapply(top_components_sfn, function(x) !is.null(x) && inherits(x, "sfnetwork"))
    all_edges = NULL

    # Loop through each component, activate edges, convert to sf, and combine
    for (i in seq_along(top_components_sfn)) {
        if (!is.null(top_components_sfn[[i]]) && inherits(top_components_sfn[[i]], "sfnetwork")) {
            # Activate edges and convert to sf
            edges_sf = top_components_sfn[[i]] |>
                sfnetworks::activate("edges") |>
                sf::st_as_sf() |>
                dplyr::mutate(component = as.factor(i))  # Add component ID
            
            # Combine into one sf object
            if (is.null(all_edges)) {
                all_edges = edges_sf
            } else {
                all_edges = rbind(all_edges, edges_sf)
            }
        }
    }

    sf::st_crs(all_edges) = 27700

    filtered_OS_zones = all_edges |> 
                        sf::st_transform(27700) |> 
                        sf::st_zm()

    cycle_net_NPT = stplanr::rnet_merge(filtered_OS_zones, NPT_zones, , max_angle_diff = 10, dist = 1, segment_length = 5, funs = list(all_fastest_bicycle_go_dutch = mean))          
    # cycle_net_NPT = sf::st_join(filtered_OS_zones, NPT_zones, join = st_intersects)

    summarized_data = cycle_net_NPT |>
        dplyr::group_by(component) |>
        dplyr::summarize(total_all_fastest_bicycle_go_dutch = sum(all_fastest_bicycle_go_dutch, na.rm = TRUE),
                         total_length = sum(as.numeric(sf::st_length(geometry)), na.rm = TRUE))

    min_percentile_value = stats::quantile(summarized_data$total_all_fastest_bicycle_go_dutch, probs = percentile_value, na.rm = TRUE)
    
    summarized_data = summarized_data |> dplyr::filter(total_all_fastest_bicycle_go_dutch > min_percentile_value)

    summarized_data = summarized_data |> dplyr::filter(total_length > length_threshold)

    cycle_net_NPT_filtered = cycle_net_NPT |>
    filter(st_intersects(geometry, summarized_data$geometry, sparse = FALSE) |> apply(1, any))

    return(cycle_net_NPT_filtered)
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

