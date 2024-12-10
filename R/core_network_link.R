core_network_link = function(os_scotland, osm_scotland, output_folder, date_folder) {

  all_corenet_link = sf::st_sf(geometry = st_sfc()) 
  st_crs(all_corenet_link) = 27700
  
  lads = sf::read_sf("inputdata/boundaries/la_regions_2023.geojson")
  region_names = unique(lads$Region)[c(3, 4, 1, 6, 2, 5)] |>
    # Reverse to build smallest first:
    rev()

  cnet_path = file.path(output_folder, "combined_network_tile.geojson")
  combined_net = sf::read_sf(cnet_path) |>
    sf::st_transform(crs = "EPSG:27700")

  # foreach(region = region_names) %dopar% {
  for (region in region_names) {
    message("Generating coherent network links for: ", region)
    region_snake = snakecase::to_snake_case(region)

    region_boundary = dplyr::filter(lads, Region == region) |>
      sf::st_transform(crs = "EPSG:27700")

    combined_net_region_boundary = combined_net[sf::st_union(region_boundary), , op = sf::st_intersects]

    min_percentile_value = stats::quantile(combined_net_region_boundary$all_fastest_bicycle_go_dutch, probs = parameters$coherent_percentile[2], na.rm = TRUE)

    os_scotland_region_boundary = os_scotland[sf::st_union(region_boundary), , op = sf::st_intersects]

    os_combined_net_region_boundary = corenet::cohesive_network_prep(
      base_network = os_scotland_region_boundary,
      influence_network = combined_net_region_boundary,
      region_boundary,
      crs = "EPSG:27700",
      key_attribute = "road_function",
      attribute_values = c("A Road", "B Road")
    )

    os_combined_net_region_boundary = os_combined_net_region_boundary[os_combined_net_region_boundary$form_of_way != "Slip Road", ]

    cohesive_network_region_boundary = corenet::corenet(combined_net_region_boundary, os_combined_net_region_boundary, region_boundary,
      key_attribute = "all_fastest_bicycle_go_dutch",
      crs = "EPSG:27700", maxDistPts = 15000, minDistPts = 1, npt_threshold = min_percentile_value,
      road_scores = list("A Road" = 1, "B Road" = 1), n_removeDangles = 6, penalty_value = 100000, group_column = "name_1"
    )

    cohesive_network_region_boundary = line_merge(
                    cohesive_network_region_boundary,
                    os_combined_net_region_boundary,
                    combined_net_region_boundary, 
                    group_column = "name_1"
                    ) 

    cohesive_network_region_boundary = cohesive_network_region_boundary |> select(name_1, all_fastest_bicycle_go_dutch, geometry, road_function)

    cohesive_network_region_boundary = cohesive_network_region_boundary |>
      mutate(road_function = case_when(
        road_function == "A Road" ~ "Primary",
        road_function %in% c("B Road", "Minor Road") ~ "Secondary",
        road_function %in% c("Local Road", "Local Access Road", "Secondary Access Road") ~ "Local Access",
        TRUE ~ as.character(road_function)  # Keeps other values as they are
      ))

    all_corenet_link = rbind(all_corenet_link, cohesive_network_region_boundary)
    message("Coherent network link for: ", region, " generated successfully")
  }
  corenet::create_coherent_network_PMtiles(folder_path = output_folder, city_filename = glue::glue("/combined_{date_folder}"), cohesive_network = all_corenet_link |> sf::st_transform(4326))
}
