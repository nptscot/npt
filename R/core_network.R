core_network = function(os_scotland, osm_scotland, la_name, output_folder, date_folder) {

    message("Processing coherent network for LA: ", la_name)
    la_name_snake = snakecase::to_snake_case(la_name)

    cnet_path = file.path(output_folder, la_name_snake, "combined_network_tile.geojson")
    combined_net = sf::read_sf(cnet_path) |>
        sf::st_transform(crs = "EPSG:27700")

    folder_path = file.path(output_folder, la_name_snake, "") 

    tryCatch(
        {
        la_name_boundary = filter(lads, LAD23NM == la_name) |>
            sf::st_transform(crs = "EPSG:27700")

        combined_net_la_name_boundary = combined_net[sf::st_union(la_name_boundary), , op = sf::st_intersects]

        min_percentile_value = stats::quantile(combined_net_la_name_boundary$all_fastest_bicycle_go_dutch, probs = parameters$coherent_percentile[1], na.rm = TRUE)

        os_scotland_la_name_boundary = os_scotland[sf::st_union(la_name_boundary), , op = sf::st_intersects]

        os_combined_net_la_name_boundary = corenet::cohesive_network_prep(
            base_network = os_scotland_la_name_boundary,
            influence_network = combined_net_la_name_boundary,
            la_name_boundary,
            crs = "EPSG:27700",
            key_attribute = "road_function",
            attribute_values = c("A Road", "B Road", "Minor Road", "Local Road" , "Secondary Access Road" , "Local Access Road" )
        )

        cohesive_network_la_name_boundary = corenet::corenet(combined_net_la_name_boundary, os_combined_net_la_name_boundary, la_name_boundary,
            key_attribute = "all_fastest_bicycle_go_dutch",
            crs = "EPSG:27700", maxDistPts = 3000, minDistPts = 2, npt_threshold = min_percentile_value,
            road_scores = list("A Road" = 1, "B Road" = 1, "Minor Road" = 100, "Local Road" = 100, "Secondary Access Road" = 100 , "Local Access Road" = 100), n_removeDangles = 6, penalty_value = 1, group_column = "name_1"
        )

        message("Generating Off Road Cycle Path network for: ", la_name)
        
        orcp_la_name_boundary = orcp_network(area = la_name_boundary, NPT_zones = combined_net_la_name_boundary, percentile_value = 0.7) 

        if (!is.null(orcp_la_name_boundary) && nrow(orcp_la_name_boundary) > 0) {
            osm_la_name = osm_scotland[sf::st_union(la_name_boundary), , op = sf::st_intersects] |> sf::st_transform(27700)
            osm_la_name = osm_la_name[!is.na(osm_la_name$highway), ]

            orcp_la_name_boundary = find_orcp_path(orcp_la_name_boundary, cohesive_network_la_name_boundary, osm_la_name, os_scotland_la_name_boundary, combined_net_la_name_boundary)

            orcp_la_name_boundary = orcp_la_name_boundary |>
            group_by(component) |>
            summarize(
                all_fastest_bicycle_go_dutch = round(mean(all_fastest_bicycle_go_dutch, na.rm = TRUE)),
                geometry = st_line_merge(st_combine(st_union(geometry)))
            )

            p_90 = quantile(orcp_la_name_boundary$all_fastest_bicycle_go_dutch, 0.90, na.rm = TRUE)
            p_25 = quantile(orcp_la_name_boundary$all_fastest_bicycle_go_dutch, 0.25, na.rm = TRUE)

            # Update road function based on percentile thresholds
            orcp_la_name_boundary$road_function = case_when(
            orcp_la_name_boundary$all_fastest_bicycle_go_dutch > p_90 ~ "Primary",
            orcp_la_name_boundary$all_fastest_bicycle_go_dutch > p_25 & orcp_la_name_boundary$all_fastest_bicycle_go_dutch <= p_90 ~ "Secondary",
            orcp_la_name_boundary$all_fastest_bicycle_go_dutch <= p_25 ~ "Local Access",
            TRUE ~ "Local Access"  
            )

            orcp_la_name_boundary$name_1 = orcp_la_name_boundary$component

            # Identify common columns
            common_columns = intersect(names(cohesive_network_la_name_boundary), names(orcp_la_name_boundary))

            # Subset both data frames to common columns
            cohesive_network_filtered = cohesive_network_la_name_boundary[common_columns]

            cohesive_network_filtered = line_merge(cohesive_network_filtered, os_combined_net_la_name_boundary, combined_net_la_name_boundary, group_column = "name_1")

            orcp_la_name_boundary_filtered = orcp_la_name_boundary[common_columns]

            if (!is.null(cohesive_network_filtered) && nrow(cohesive_network_filtered) > 0) {
            orcp_la_name_boundary_filtered = convert_to_linestrings(orcp_la_name_boundary_filtered)

            grouped_network = rbind(cohesive_network_filtered, orcp_la_name_boundary_filtered)
            } else {
            grouped_network = orcp_la_name_boundary_filtered
            }

            # Remove duplicates in grouped_network
            grouped_network = grouped_network[!duplicated(grouped_network), ]
        } else {
            grouped_network = cohesive_network_la_name_boundary
        }

        grouped_network = grouped_network %>%
            mutate(road_function = case_when(
            road_function == "A Road" ~ "Primary",
            road_function %in% c("B Road", "Minor Road") ~ "Secondary",
            road_function %in% c("Local Road", "Local Access Road", "Secondary Access Road") ~ "Local Access",
            TRUE ~ as.character(road_function)  # Keeps other values as they are
            ))

        # Use la_name name in the filename
        corenet::create_coherent_network_PMtiles(folder_path = folder_path, city_filename = glue::glue("{la_name_snake}_{date_folder}"), cohesive_network = grouped_network|> sf::st_transform(4326))

        message("Coherent network for: ", la_name, " generated successfully")
        },
        error = function(e) {
        message(sprintf("An error occurred with %s: %s", la_name, e$message))    
        }
    )
}
