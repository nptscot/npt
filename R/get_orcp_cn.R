orcp_network = function(area, NPT_zones, length_threshold = 10000, percentile_value = 0.6) {
  tryCatch({
    options(timeout=30000)
    osm = osmactive::get_travel_network("Scotland", boundary = area, boundary_type = "clipsrc", force_download = TRUE)
    cycle_net = osmactive::get_cycling_network(osm)
    drive_net = osmactive::get_driving_network_major(osm)
    cycle_net = osmactive::distance_to_road(cycle_net, drive_net)
    cycle_net = osmactive::classify_cycle_infrastructure(cycle_net)
    # filter cycle_net based on column bicycle is yes dismount and designated
    cycle_net = cycle_net |>
      dplyr::filter(bicycle %in% c("yes", "dismount", "designated")) |>
      dplyr::filter(cycle_segregation %in% c("Off Road Cycleway")) |>
      dplyr::mutate(length = as.numeric(sf::st_length(geometry))) |>
      dplyr::filter(length > 1) |>
      sf::st_transform(crs = 27700)

    cycle_net = sf::st_cast(cycle_net, "LINESTRING")  
    cycle_net = cycle_net |> dplyr::select(geometry)
    cycle_net$length = sf::st_length(cycle_net)
    cycle_net_components = find_component(cycle_net, threshold = 1)

    # Get the list of all unique components
    unique_components = unique(cycle_net_components$component)
    # Initialize an empty list to store the cleaned components
    cycle_net_components_clean = list()

    # Loop over each unique component
    for (comp in unique_components) {
      # Filter for the current component and select the geometry column
      component_data = cycle_net_components |>
                        dplyr::filter(component == comp) |>
                        dplyr::select(geometry)

      # Calculate the total length after performing a spatial union of the geometries
      total_length = sum(sf::st_length(sf::st_union(component_data)))

      tryCatch({
        # Check if total length is more than 1000 meters
        if (total_length > units::set_units(200, "meters")) {
          # Clean the data by removing dangles if length condition is met
          cleaned_data = remove_dangles(component_data)
          # Store only the geometry data in the list
          cycle_net_components_clean[[length(cycle_net_components_clean) + 1]] = cleaned_data
        }
      }, error = function(e) {
        # If an error occurs, print a custom error message
        print(sprintf("Failed to process component %d due to error: %s", comp, e$message))
      })
    }

    cycle_net_clean = do.call(rbind, cycle_net_components_clean)
    snapped_lines = sf::st_snap(cycle_net_clean, cycle_net_clean, tolerance = 20)
    snapped_lines_compoenet = find_component(snapped_lines, threshold = 1)
    snapped_lines_compoenet_buffer = sf::st_buffer(snapped_lines_compoenet, dist = 30)
    cycle_net_clean_components = sf::st_join(cycle_net_clean, snapped_lines_compoenet_buffer, join = sf::st_intersects)

    cycle_net_clean_length = cycle_net_clean_components |>
        dplyr::group_by(component) |>
        dplyr::summarize(
                          total_length = sum(as.numeric(sf::st_length(geometry)), na.rm = TRUE)
                        )  

    min_percentile_value = stats::quantile(cycle_net_clean_length$total_length, probs = 0.8, na.rm = TRUE)
    cycle_net_f = cycle_net_clean_length |> 
                  dplyr::filter(total_length > min_percentile_value)

    filtered_OS_zones = cycle_net_f |> 
                        sf::st_transform(27700) |> 
                        sf::st_zm()

    NPT_zones = sf::st_cast(NPT_zones, "LINESTRING")
    filtered_OS_zones = sf::st_cast(filtered_OS_zones, "LINESTRING")
    NPT_zones$id = 1:nrow(NPT_zones)
    filtered_OS_zones$id = 1:nrow(filtered_OS_zones)
                            
    params = list(
        list(
          source = NPT_zones,
          target = filtered_OS_zones,
          attribute = "all_fastest_bicycle_go_dutch",
          new_name = "all_fastest_bicycle_go_dutch",
          agg_fun = sum,
          weights = c("target_weighted")
        )
      )

    results_list = purrr::map(params, function(p) {
        corenet::anime_join(
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

    cycle_net_NPT = purrr::reduce(results_list, function(x, y) {
      dplyr::left_join(x, y, by = "id")
    }, .init = filtered_OS_zones)

    cycle_net_NPT = stplanr::rnet_merge(filtered_OS_zones, NPT_zones, max_angle_diff = 10, dist = 1, segment_length = 5, funs = list(all_fastest_bicycle_go_dutch = mean))       

    # cycle_net_NPT = sf::st_join(filtered_OS_zones, NPT_zones, join = st_intersects)

    summarized_data = cycle_net_NPT |>
        dplyr::group_by(component) |>
        dplyr::summarize(all_fastest_bicycle_go_dutch = mean(all_fastest_bicycle_go_dutch, na.rm = TRUE),
                        total_length = sum(as.numeric(sf::st_length(geometry)), na.rm = TRUE))

    min_percentile_value = stats::quantile(summarized_data$all_fastest_bicycle_go_dutch, probs = 0.5, na.rm = TRUE)
    
    summarized_data = summarized_data |> dplyr::filter(all_fastest_bicycle_go_dutch >= min_percentile_value)

    return(summarized_data)
  }, error = function(e) {
    message("Error in processing: ", e$message)
    return(NULL)
  })
}

create_line = function(pt1, pt2) {
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
  points_not_within_hull = points_sf[!st_within(points_sf, hull_sf, sparse = FALSE), ]

  return(points_sf)
}


find_nearest_points = function(points_sf, rnet, segment_length = 10, dist = 100) {
    if (is.null(points_sf) || nrow(points_sf) == 0) {
        return(NULL)
    }

    rnet = stplanr::line_cast(rnet)
    rnet = stplanr::line_segment(rnet, segment_length = segment_length)   
    rnet_points_df = as.data.frame(sf::st_coordinates(rnet))
    names(rnet_points_df) = c("X", "Y", "L1")
    rnet_points_sf = st_as_sf(rnet_points_df, coords = c("X", "Y"), crs = st_crs(rnet))
    points_sf_buffer = st_buffer(points_sf, dist = dist)
    rnet_points_sf_within_buffer = sf::st_intersection(rnet_points_sf, points_sf_buffer)

    if (nrow(rnet_points_sf_within_buffer) == 0) {
        message("No points in the road network are within the buffer distance.")
        nearest_os_points = NULL
    } else {
        # Find the nearest point in rnet_points_sf_within_buffer for each point in points_sf
        nearest_points = nngeo::st_nn(points_sf, rnet_points_sf_within_buffer, k = 1, returnDist = TRUE)

        # Extract the nearest points and distances
        nearest_indices = unlist(nearest_points$nn)
        nearest_distances = unlist(nearest_points$dist)

        # Ensure nearest_indices are within the range of rnet_points_sf_within_buffer
        if (all(nearest_indices <= nrow(rnet_points_sf_within_buffer))) {
            # Add the index of points_sf to the nearest_os_points
            nearest_os_points = rnet_points_sf_within_buffer[nearest_indices, ]
            nearest_os_points$index_points_sf = 1:length(nearest_indices)
        } else {
            nearest_os_points = NULL
        }
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

fix_net_connectivity = function(file_name, input_dir, output_dir, gisBase, gisDbase, location, mapset) {
  library(rgrass)

  # Initialize GRASS GIS environment
  initGRASS(gisBase = gisBase, 
            gisDbase = gisDbase, 
            location = location, 
            mapset = mapset, 
            override = TRUE)
  
  execGRASS("g.gisenv", flags = "s")
  
  # Construct input and output file paths
  input_file = glue::glue("{input_dir}/{file_name}")
  output_file = glue::glue("{output_dir}/{file_name}_fixed.geojson")
  
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

find_orcp_path = function(orcp_city_boundary, cohesive_network_city_boundary, OSM_city, open_roads_scotland_city_boundary, combined_net_city_boundary) {
    # orcp_city_boundary = find_component(orcp_city_boundary, threshold = 1)
    components = unique(orcp_city_boundary$component)
    # mapview::mapview(orcp_city_boundary) + mapview::mapview(cohesive_network_city_boundary, color = "red")
    if (nrow(orcp_city_boundary) > 0) {
        # Initialize the list to store paths for each component
        all_paths_dict = list()

        # Loop through each component
        for (component_id in components) {
            message("Processing component: ", component_id)
            gdf = dplyr::filter(orcp_city_boundary, component == component_id)
            end_points_sf = list()
            os_points = list()
            osm_points = list()
            end_points_sf = find_endpoints(gdf)
            os_points = find_nearest_points(end_points_sf, cohesive_network_city_boundary, dist = 3000, segment_length = 5)
            osm_points = find_nearest_points(os_points, OSM_city, dist = 3000, segment_length = 5)

            if (length(end_points_sf) != 0 && length(os_points) != 0 && length(osm_points) != 0) {
                hull_sf = sf::st_convex_hull(sf::st_union(osm_points))
                buffer_distance = 3000  # Adjust the buffer distance as needed
                buffered_hull_sf = sf::st_buffer(hull_sf, dist = buffer_distance)
                
                OSM = OSM_city[sf::st_union(buffered_hull_sf), , op = sf::st_intersects]
                OS = open_roads_scotland_city_boundary[sf::st_union(buffered_hull_sf), , op = sf::st_intersects]

                paths = compute_shortest_paths(end_points_sf, osm_points, OSM)

                filtered_paths = list()  # This list will store only the paths that meet the criterion
                number_of_paths = 0

                for (i in 1:length(paths)) {
                    number_of_paths = number_of_paths + nrow(paths[[i]]$path_edges)
                    path = paths[[i]]$path_edges
                    
                    if (is.null(path) || nrow(path) == 0) {
                        next
                    }
                    
                    path_buffered = sf::st_buffer(path, dist = 3)
                    combined_net_city_boundary_path = combined_net_city_boundary[sf::st_union(path_buffered), , op = sf::st_intersects]
                    
                    path_npt = stplanr::rnet_merge(path, combined_net_city_boundary_path, max_angle_diff = 10, dist = 1, segment_length = 5, funs = list(all_fastest_bicycle_go_dutch = mean))
                
                    path_mean = mean(path_npt$all_fastest_bicycle_go_dutch, na.rm = TRUE)
                    
                    if (!is.nan(path_mean) && !is.na(path_mean) && path_mean >= 50) {
                        filtered_paths[[length(filtered_paths) + 1]] = sf::st_union(path_npt)
                    }
                    
                    all_paths_dict[[component_id]] = filtered_paths
                }

                if (number_of_paths == 0) {
                    components = components[components != component_id]
                }
            } else {
                message("No routes found for component: ", component_id)
                components = components[components != component_id]
            }
        }

        if (length(components) > 0) {        
          all_orcp_path_sf = list()

          for (component_id in components) {
              y_range = length(all_paths_dict[[component_id]])

              # Skip the rest of the loop if y_range is 0
              if (y_range == 0) {
                  next
              }

              for (y in 1:y_range) {
                  # Extract the sf object if it's not NULL
                  if (!is.null(all_paths_dict[[component_id]][[y]])) {
                      # Assuming 'all_orcp_path_sf' is initialized earlier as a list
                      all_orcp_path_sf[[length(all_orcp_path_sf) + 1]] = all_paths_dict[[component_id]][[y]]
                  }
              }
          }

          combined_orcp_path_sf = do.call(rbind, all_orcp_path_sf)
          combined_orcp_path_sf = lapply(combined_orcp_path_sf[, 1], st_sfc, crs = 27700)
          geometries = lapply(combined_orcp_path_sf, function(sfc) { sfc[[1]] })
          combined_sfc = st_sfc(geometries, crs = 27700) 
          combined_orcp_path_sf = st_sf(geometry = combined_sfc)

          combined_orcp_path_sf_buffered = st_buffer(combined_orcp_path_sf, dist = 2)
          intersects = st_intersects(combined_orcp_path_sf_buffered, combined_net_city_boundary, sparse = FALSE)

          combined_orcp_path_sf$all_fastest_bicycle_go_dutch = combined_net_city_boundary$all_fastest_bicycle_go_dutch[apply(intersects, 1, function(x) which(x)[1])]

          summarized_data = combined_orcp_path_sf |> 
          dplyr::group_by(geometry) |> dplyr::summarize(total_all_fastest_bicycle_go_dutch = sum(all_fastest_bicycle_go_dutch, na.rm = TRUE),
                            total_length = sum(as.numeric(sf::st_length(geometry)), na.rm = TRUE))

          min_percentile_value = stats::quantile(summarized_data$total_all_fastest_bicycle_go_dutch, probs = 0.6, na.rm = TRUE)
          
          summarized_data = summarized_data |> dplyr::filter(total_all_fastest_bicycle_go_dutch > min_percentile_value)

          summarized_data = summarized_data |> dplyr::filter(total_length > 10)

          combined_orcp_path_sf_filtered = combined_orcp_path_sf |>
            filter(st_intersects(geometry, summarized_data$geometry, sparse = FALSE) |> apply(1, any))

          missing_columns = setdiff(names(combined_orcp_path_sf), names(orcp_city_boundary))

          if (length(missing_columns) > 0) {
            for (col in missing_columns) {
              orcp_city_boundary[[col]] = NA
            }
          }

          # Now check for missing columns in orcp_city_boundary_component compared to combined_orcp_path_sf
          missing_columns = setdiff(names(orcp_city_boundary), names(combined_orcp_path_sf))

          if (length(missing_columns) > 0) {
            for (col in missing_columns) {
              combined_orcp_path_sf[[col]] = NA
            }
          }


          # Combine all_paths_sf with orcp_city_boundary_component
          # fliter orcp_city_boundary_component based on updated components list
          orcp_city_boundary_withpath = orcp_city_boundary |>
            filter(component %in% components)
          orcp_city_boundary = rbind(orcp_city_boundary_withpath, combined_orcp_path_sf)
        } else {
          message("No component is available for processing.")
          orcp_city_boundary = orcp_city_boundary
        }  
    } else {
        cat("orcp_city_boundary is empty, skipping processing.\n")
    }
    
    return(orcp_city_boundary)
}


line_merge = function(cohesive_network_city_boundary, os_combined_net_city_boundary, combined_net_city_boundary, group_column = "name_1") {

  buffer = sf::st_buffer(cohesive_network_city_boundary, dist = 1)
  os_buffer = os_combined_net_city_boundary[sf::st_union(buffer), , op = sf::st_within]

  params = list(
                list(
                  source = combined_net_city_boundary,
                  target = os_buffer,
                  attribute = "all_fastest_bicycle_go_dutch",
                  new_name = "all_fastest_bicycle_go_dutch",
                  agg_fun = sum,
                  weights = c("target_weighted")
                )
              )

  results_list = purrr::map(params, function(p) {
    corenet::anime_join(
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


  os_buffer_NPT = reduce(results_list, function(x, y) {
    left_join(x, y, by = "id")
  }, .init = os_buffer)  


  if (!"all_fastest_bicycle_go_dutch" %in% names(os_buffer_NPT)) {
    os_buffer_NPT$all_fastest_bicycle_go_dutch = os_buffer_NPT$all_fastest_bicycle_go_dutch.x
  }
  
  os_buffer_NPT = sf::st_cast(os_buffer_NPT, "LINESTRING")

  os_buffer_NPT_group = os_buffer_NPT |>
    group_by(!!sym(group_column)) |>
    summarize(
      road_function = first(road_function),
      all_fastest_bicycle_go_dutch = round(mean(all_fastest_bicycle_go_dutch, na.rm = TRUE)),  # rounding the mean
      geometry = st_line_merge(st_combine(st_union(geometry)))
    )

  geometry_types = sf::st_geometry_type(os_buffer_NPT_group)
  os_buffer_NPT_group_linestring = os_buffer_NPT_group[geometry_types == "LINESTRING", ]

  # Handle other geometries separately, e.g., POINT or POLYGON
  os_buffer_NPT_group_others = os_buffer_NPT_group[geometry_types != "LINESTRING", ]
  os_buffer_NPT_group_multilinestring = os_buffer_NPT_group[geometry_types == "MULTILINESTRING", ]
  os_buffer_NPT_group_multilinestring = sf::st_cast(os_buffer_NPT_group_multilinestring, "LINESTRING")

  # Combine with original LINESTRING geometries
  os_buffer_NPT_group_combined = rbind(os_buffer_NPT_group_linestring, os_buffer_NPT_group_multilinestring)

  return(os_buffer_NPT_group_combined)
}


convert_to_linestrings = function(geometries) {
  # Check the geometry types
  geometry_types = sf::st_geometry_type(geometries)
  
  # If the unique geometry types are not just LINESTRING, perform the conversion
  if (!all(unique(geometry_types) == "LINESTRING")) {
    
    # Filter out LINESTRING geometries
    linestrings = geometries[geometry_types == "LINESTRING", ]
    
    # Filter out MULTILINESTRING geometries and cast them to LINESTRING
    multilinestrings = geometries[geometry_types == "MULTILINESTRING", ]
    multilinestrings = sf::st_cast(multilinestrings, "LINESTRING")
    
    # Combine the LINESTRING and converted MULTILINESTRING geometries
    combined = dplyr::bind_rows(linestrings, multilinestrings)
    
    # Return combined geometries
    return(combined)
    
  } else {
    # If all geometries are LINESTRING, return the original geometries
    return(geometries)
  }
}

