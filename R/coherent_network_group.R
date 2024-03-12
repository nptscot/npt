coherent_network_group = function(CITY, ZONE, min_percentile, arterial = FALSE) {
  library(tidygraph)
  # Generate coherent network
  rnet_coherent = cohesive_network(network_tile = CITY, combined_grid_buffer = ZONE, arterial = arterial, min_percentile = min_percentile)
  
  # Select relevant columns
  rnet_coherent_selected = rnet_coherent |>
    dplyr::select(all_fastest_bicycle_go_dutch, weight)
  
  # Group and process the network
  grouped_net = rnet_coherent_selected |>
    sfnetworks::as_sfnetwork(directed = FALSE) |>
    tidygraph::morph(to_linegraph) |>
    dplyr::mutate(group = tidygraph::group_edge_betweenness(n_groups = 12)) |>
    tidygraph::unmorph() |>
    tidygraph::activate(edges) |>
    sf::st_as_sf() |>
    sf::st_transform("EPSG:4326") |>
    dplyr::group_by(group) |>
    dplyr::summarise(mean_potential = mean(weight, na.rm = TRUE)) |>
    dplyr::mutate(group = rank(-mean_potential))
  
  # Return the processed network
  grouped_net
}
