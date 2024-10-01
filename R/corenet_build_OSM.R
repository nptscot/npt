corenet_build_OSM = function(osm_scotland, region_names, cities_region_names) {

  message("Generate the city's coherent network for each region with growing")

  for (region in region_names) {
    # region = region_names[5]  "Edinburgh and Lothians"  
    message("Processing coherent network for region: ", region)
    region_snake = snakecase::to_snake_case(region)
    coherent_area = cities_region_names[[region]]

    cnet_path = file.path(output_folder, region_snake, "combined_network_tile.geojson")
    combined_net = sf::read_sf(cnet_path) |>
      sf::st_transform(crs = "EPSG:27700")

    folder_path = file.path(output_folder, region_snake, "coherent_networks_OSM/")

    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }

    for (city in coherent_area) {
      # city = coherent_area[3] "City of Edinburgh"
      city_filename = snakecase::to_snake_case(city)
      tryCatch(
        {
          message("Generating coherent network for: ", city)
          city_boundary = filter(lads, LAD23NM == city) |>
            sf::st_transform(crs = "EPSG:27700")

          combined_net_city_boundary = combined_net[sf::st_union(city_boundary), , op = sf::st_intersects]

          min_percentile_value = stats::quantile(combined_net_city_boundary$all_fastest_bicycle_go_dutch, probs = parameters$coherent_percentile[1], na.rm = TRUE)

          osm_scotland_city_boundary = osm_scotland[sf::st_union(city_boundary), , op = sf::st_intersects]

          osm_combined_net_city_boundary = corenet::cohesive_network_prep(
            base_network = osm_scotland_city_boundary,
            influence_network = combined_net_city_boundary,
            city_boundary,
            crs = "EPSG:27700",
            key_attribute = "highway",
            attribute_values = c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", "unclassified", "pedestrian", "footway", "cycleway",  "path")
          )

          osm_combined_net_city_boundary = osm_combined_net_city_boundary |> dplyr::rename(road_function = highway)

          cohesive_network_city_boundary = corenet::corenet(combined_net_city_boundary, osm_combined_net_city_boundary, city_boundary,
            key_attribute = "all_fastest_bicycle_go_dutch",
            crs = "EPSG:27700", maxDistPts = 3000, minDistPts = 2, npt_threshold = min_percentile_value,
            road_scores = list("primary" = 1, "primary_link" = 1, "secondary" = 1, "secondary_link" = 1, "pedestrian" = 1, "footway" = 1, "cycleway" = 1, "unclassified" = 10, "tertiary" = 10, "tertiary_link" = 10, "path" = 100), n_removeDangles = 6, penalty_value = 1, group_column = "name"
          )

          cohesive_network_city_boundary = line_merge(cohesive_network_city_boundary, osm_combined_net_city_boundary, combined_net_city_boundary, group_column = "name")

          # Use city name in the filename
          corenet::create_coherent_network_PMtiles(folder_path = folder_path, city_filename = glue::glue("{city_filename}_{date_folder}_4"), cohesive_network = cohesive_network_city_boundary|> sf::st_transform(4326))

          message("Coherent network for: ", city, " generated successfully")

          # Generate growing networks
          # Define common parameters
          network_params = list(
            key_attribute = "all_fastest_bicycle_go_dutch",
            crs = "EPSG:27700",
            maxDistPts = c(1500, 2500, 3000),
            minDistPts = 2,
            road_scores = list("primary" = 1, "primary_link" = 1, "secondary" = 1, "secondary_link" = 1, "pedestrian" = 1, "footway" = 1, "cycleway" = 1, "unclassified" = 100, "tertiary" = 100, "tertiary_link" = 100, "path" = 100),
            n_removeDangles = 6,
            penalty_value = 1,
            group_column = "name"
          )

          # Define the varying npt_threshold values
          max_value = round(stats::quantile(combined_net_city_boundary$all_fastest_bicycle_go_dutch, probs = 0.99, na.rm = TRUE))
          min_value = round(stats::quantile(combined_net_city_boundary$all_fastest_bicycle_go_dutch, probs = 0.95, na.rm = TRUE))

          
          if (min_value > 50) {
            step_size = (max_value - min_value) / 2
            step_size = -abs(step_size)
            thresholds = round(seq(max_value, min_value, by = step_size))

            # Generate the networks using varying npt_threshold
            CN_networks = Map(function(threshold, maxDistPt) { 
              message("Generating CN network for threshold: ", threshold, " and maxDistPt: ", maxDistPt)
              corenet::corenet(
                combined_net_city_boundary,
                osm_combined_net_city_boundary,
                city_boundary,
                key_attribute = network_params$key_attribute,
                crs = network_params$crs,
                maxDistPts = maxDistPt,
                minDistPts = network_params$minDistPts,
                npt_threshold = threshold,
                road_scores = network_params$road_scores,
                n_removeDangles = network_params$n_removeDangles,
                penalty_value = network_params$penalty_value,
                group_column = network_params$group_column
              )
            }, thresholds, network_params$maxDistPts)

            # Process each generated network
            for (i in seq_along(CN_networks)) {
              cn = CN_networks[[i]]

              cn = line_merge(
                              cn,
                              osm_combined_net_city_boundary,
                              combined_net_city_boundary,
                              group_column = "name"
                              )

              # grouped_network = corenet::coherent_network_group(cn, key_attribute = "all_fastest_bicycle_go_dutch")
              # grouped_network = grouped_network |> dplyr::rename(all_fastest_bicycle_go_dutch = mean_potential)

              # Use city name and threshold in the filename, using the correct threshold
              city_filename = glue::glue("{snakecase::to_snake_case(city)}_{date_folder}_{i}")
              corenet::create_coherent_network_PMtiles(folder_path = folder_path, city_filename = city_filename, cohesive_network = cn |> sf::st_transform(4326))
              message("Coherent network for: ", city, " with threshold ", thresholds[i] , " generated successfully")
            }
          } else {
            message("Min value is not greater than 50 Code execution skipped.")
          }
        },
        error = function(e) {
          message(sprintf("An error occurred with %s: %s", city, e$message))    
        }
      )
    }
  }

  message("Generate the links coherent network for the LAs")
  # foreach(region = region_names) %dopar% {
  for (region in region_names) {
    message("Processing coherent network links for region: ", region)
    region_snake = snakecase::to_snake_case(region)

    folder_path = file.path(output_folder, region_snake, "coherent_networks_OSM/")

    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }


    message("Generating coherent network links for: ", region)
    region_boundary = dplyr::filter(lads, Region == region) |>
      sf::st_transform(crs = "EPSG:27700")

    combined_net_region_boundary = combined_net[sf::st_union(region_boundary), , op = sf::st_intersects]

    min_percentile_value = stats::quantile(combined_net_region_boundary$all_fastest_bicycle_go_dutch, probs = parameters$coherent_percentile[2], na.rm = TRUE)

    osm_scotland_region_boundary = osm_scotland[sf::st_union(region_boundary), , op = sf::st_intersects]

    osm_combined_net_region_boundary = corenet::cohesive_network_prep(
      base_network = osm_scotland_region_boundary,
      influence_network = combined_net_region_boundary,
      region_boundary,
      crs = "EPSG:27700",
      key_attribute = "highway",
      attribute_values = c("primary", "primary_link", "secondary", "secondary_link")
    )

    osm_combined_net_region_boundary = osm_combined_net_region_boundary |> dplyr::rename(road_function = highway)

    cohesive_network_region_boundary = corenet::corenet(combined_net_region_boundary, osm_combined_net_region_boundary, region_boundary,
      key_attribute = "all_fastest_bicycle_go_dutch",
      crs = "EPSG:27700", maxDistPts = 10000, minDistPts = 2000, npt_threshold = min_percentile_value,
      road_scores = list("primary" = 1, "primary_link" = 1, "secondary" = 1, "secondary_link" = 1), n_removeDangles = 6, penalty_value = 1000, group_column = "name"
    )

    cohesive_network_region_boundary = line_merge(
                    cohesive_network_region_boundary,
                    osm_combined_net_region_boundary,
                    combined_net_region_boundary,
                    group_column = "name"
                    )

    corenet::create_coherent_network_PMtiles(folder_path = folder_path, city_filename = glue::glue("{region_snake}_{date_folder}"), cohesive_network = cohesive_network_region_boundary |> sf::st_transform(4326))

    message("Coherent network link for: ", region, " generated successfully")
    
  }


  message("Combine all cohesive networks (CN) into a single file for each growing network")

  no_lists = 1:4
  all_CN_geojson_groups = list()

  # Function to check if a file starts with any region name
  starts_with_region = function(filepath) {
      # Extract the filename from the path
      filename = basename(filepath)
      
      # Check if the filename starts with any of the region names, anchored at the start
      any(sapply(region_names_lowercase, function(region) grepl(paste0("^", region), filename, ignore.case = TRUE)))
  }

  # Loop through the subfolders and read GeoJSON files
  subfolders = list.dirs(output_folder, full.names = TRUE, recursive = FALSE)
  for (folder in subfolders) {
    coherent_networks_path = file.path(folder, "coherent_networks_OSM")
    geojson_files = list.files(coherent_networks_path, pattern = "\\.geojson$", full.names = TRUE)

    # Initialize a list to track files that have been matched to a number
    matched_files = list()

    # Loop through each predefined number and match files
    for (no in no_lists) {
      # Define a pattern that includes the current number from no_lists
      number_pattern = sprintf(".*_%s_%d_coherent_network\\.geojson$", date_folder, no)
      # Find files that match this pattern
      these_matched_files = grep(number_pattern, geojson_files, value = TRUE)
      
      # Process each matched file
      for (geojson_file in these_matched_files) {
        if (!exists(as.character(no), where = all_CN_geojson_groups)) {
          all_CN_geojson_groups[[as.character(no)]] = list()
        }
        # Read and transform the geojson data
        geojson_data = sf::st_read(geojson_file, quiet = TRUE) |>
          sf::st_transform(crs = 4326)
        all_CN_geojson_groups[[as.character(no)]][[length(all_CN_geojson_groups[[as.character(no)]]) + 1]] = list(data = geojson_data, file = geojson_file)
      }

      # Add these files to the overall matched files list
      matched_files = c(matched_files, these_matched_files)
    }
    
    if (no == "4") {
      # Define a pattern for files with the date but without a specific number
      LAs_link_geojson = sprintf(".*_%s_coherent_network\\.geojson$", date_folder)
      # Identify files that match the general pattern but are not in the number-specific list
      LAs_link_geojson_files = grep(LAs_link_geojson, geojson_files, value = TRUE)
      
      combined_sf = bind_rows(lapply(all_CN_geojson_groups[["4"]], function(x) x$data)) 
      buffered_sf = stplanr::geo_buffer(sf::st_union(combined_sf), crs = "EPSG:27700", dist = 20)

      buffered_sf = sf::st_transform(buffered_sf, crs = 27700)

      # Process general matched files
      for (geojson_file in LAs_link_geojson_files) {
        # Read and transform the geojson data
        geojson_data = sf::st_read(geojson_file, quiet = TRUE) |>
          sf::st_transform(crs = 27700)

        clipped_sf = sf::st_difference(geojson_data, sf::st_union(buffered_sf)) |>
          sf::st_transform(crs = 4326)

        all_CN_geojson_groups[[as.character(no)]][[length(all_CN_geojson_groups[[as.character(no)]]) + 1]] = list(data = clipped_sf, file = geojson_file)
      }
    }
  }

  # Print filenames for each group to check the sorting
  # for (number in names(all_CN_geojson_groups)) {
  #   cat("Group", number, "contains the following files:\n")
  #   lapply(all_CN_geojson_groups[[number]], function(x) cat(x$file, "\n"))
  # }
  # for (number in names(all_CN_geojson_groups)) {
  #   cat("Number of files for group", number, ":", length(all_CN_geojson_groups[[number]]), "\n")
  # }
  all_columns = c("geometry", "all_fastest_bicycle_go_dutch")

  # Iterate over each group to process and save the data
  for (number in names(all_CN_geojson_groups)) {
    # Combine all GeoJSON data into one sf object for the current number group
    combined_CN_geojson = do.call(rbind, lapply(all_CN_geojson_groups[[number]], function(x) {
      if (is.list(x) && "data" %in% names(x) && inherits(x$data, "sf")) {
        
        # Round the column 'all_fastest_bicycle_go_dutch' if it exists
        if ("all_fastest_bicycle_go_dutch" %in% names(x$data)) {
          x$data$all_fastest_bicycle_go_dutch = round(x$data$all_fastest_bicycle_go_dutch)
        }
        
        # Step 3: Add missing columns with NA values
        missing_cols = setdiff(all_columns, names(x$data))
        if (length(missing_cols) > 0) {
          x$data[missing_cols] = NA
        }

        # Reorder columns to match the full set of columns
        x$data = x$data[, all_columns]

        return(x$data)
      } else {
        return(NULL)
      }
    }))
    # Define the file path for the combined GeoJSON
    combined_CN_file = glue::glue("{output_folder}/combined_CN_{number}_{date_folder}_OSM.geojson")
    
    # Write the combined GeoJSON to a file
    sf::st_write(combined_CN_geojson, combined_CN_file, delete_dsn = TRUE)
    cat("Combined cohesive networks GeoJSON file for group", number, "has been saved to:", combined_CN_file, "\n")

    # Define the path for the PMtiles
    combined_CN_pmtiles = glue::glue("{output_folder}/combined_CN_{number}_{date_folder}_OSM.pmtiles")
    
    # Construct the Tippecanoe command for the current group
    command_tippecanoe = paste0(
      'tippecanoe -o ', combined_CN_pmtiles,
      ' --name="', 'Scottish_Coherent_Networks_', number, '"',
      ' --layer=coherent_networks',
      ' --attribution="University of Leeds"',
      ' --minimum-zoom=6',
      ' --maximum-zoom=13',
      ' --maximum-tile-bytes=5000000',
      ' --simplification=10',
      ' --buffer=5',
      ' -rg',
      ' --force ',
      combined_CN_file
    )

    # Execute the command and capture output
    system_output = system(command_tippecanoe, intern = TRUE)
    cat("Tippecanoe output for group", number, ":\n", system_output, "\n")
  }
}