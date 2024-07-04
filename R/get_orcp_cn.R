orcp_network = function(area, NPT_zones, length_threshold = 1, percentile_value = 0.6) {

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
    dplyr::filter(length > length_threshold) |>
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

    funs = list()

    name_list = names(NPT_zones) 
    for (name in name_list) {
    if (name == "geometry") {
        next  # Correctly skip the current iteration if the name is "geometry"
    } else if (name %in% c("gradient", "quietness")) {
        funs[[name]] = mean  # Assign mean function for specified fields
    } else {
        funs[[name]] = sum  # Assign sum function for all other fields
    }
    }

    filtered_OS_zones = all_edges |> 
                        sf::st_transform(27700) |> 
                        sf::st_zm()
    cycle_net_NPT =  stplanr::rnet_merge(filtered_OS_zones, NPT_zones, dist = 10, funs = funs, max_angle_diff = 10)

    summarized_data = cycle_net_NPT |>
        dplyr::group_by(component) |>
        dplyr::summarize(total_all_fastest_bicycle_go_dutch = sum(all_fastest_bicycle_go_dutch, na.rm = TRUE))

    min_percentile_value = stats::quantile(summarized_data$total_all_fastest_bicycle_go_dutch, probs = percentile_value, na.rm = TRUE)
    
    summarized_data = summarized_data |> dplyr::filter(total_all_fastest_bicycle_go_dutch > min_percentile_value)

    cycle_net_NPT_filtered <- cycle_net_NPT %>%
    filter(st_intersects(geometry, summarized_data$geometry, sparse = FALSE) %>% apply(1, any))

    return(cycle_net_NPT_filtered)
}