corenet_build_OS = function(os_scotland, osm_scotland) {

  message("Generate the links coherent network for the LAs")

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

    # create folder if not exist 
    folder_path_links = glue::glue(output_folder, "/0_cohesive_network_link/")

    if (!dir.exists(folder_path_links)) {
      dir.create(folder_path_links, recursive = TRUE)
    } 

    corenet::create_coherent_network_PMtiles(folder_path = folder_path_links, city_filename = glue::glue("{region_snake}_{date_folder}"), cohesive_network = cohesive_network_region_boundary |> sf::st_transform(4326))

    message("Coherent network link for: ", region, " generated successfully")
  
  }

  message("Combine all cohesive networks (CN) into a single file for each growing network")

  # Loop through the subfolders and read GeoJSON files
  subfolders = list.dirs(output_folder, full.names = TRUE, recursive = FALSE)
  subfolders = subfolders[!grepl("0_cohesive_network_link", subfolders)]

  all_la_coherent_list = list()

  for (folder in subfolders) {
    coherent_networks_path = file.path(folder, "coherent_networks_OS")
    if (dir.exists(coherent_networks_path)) {
      geojson_files = list.files(coherent_networks_path, pattern = "\\.geojson$", full.names = TRUE)
      for (geojson_file in geojson_files) {
        geojson_data = sf::st_read(geojson_file, quiet = TRUE) |>
          sf::st_transform(crs = 4326)
        # Append the data to the list
        all_la_coherent_list[[length(all_la_coherent_list) + 1]] = geojson_data
      }
    }
  }

  # Combine all data frames into one
  links_geojson_files = list.files(folder_path_links, pattern = "\\.geojson$", full.names = TRUE)

  all_region_links_coherent_list = lapply(links_geojson_files, function(geojson_file) {
    sf::st_read(geojson_file, quiet = TRUE)
  })

  all_region_links_coherent = do.call(rbind, all_region_links_coherent_list) |>
    sf::st_transform(crs = 27700)

  buffered_sf = stplanr::geo_buffer(sf::st_union(all_la_coherent), crs = "EPSG:27700", dist = 20) |>
    sf::st_transform(crs = 27700)

  clipped_sf = sf::st_difference(all_region_links_coherent, sf::st_union(buffered_sf)) |>
          sf::st_transform(crs = 4326)

  all_la_coherent = sf::st_transform(all_la_coherent, crs = 4326)
  
  # find common columns
  common_columns = intersect(names(all_la_coherent), names(clipped_sf))

  # Select only the common columns in both data frames and bind them
  final_la_coherent = rbind(
    all_la_coherent[, common_columns, drop = FALSE],
    clipped_sf[, common_columns, drop = FALSE]
  )

  # Define the file path for the combined GeoJSON
  combined_CN_geojson_file = glue::glue("{output_folder}/combined_CN_{date_folder}_OS.geojson")
  combined_CN_gpkg_file = glue::glue("{output_folder}/combined_CN_{date_folder}_OS.gpkg")
  # Write the combined data to GeoJSON and GeoPackage files
  sf::st_write(final_la_coherent, combined_CN_geojson_file, delete_dsn = TRUE)
  sf::st_write(final_la_coherent, combined_CN_gpkg_file, delete_dsn = TRUE)

  # Print messages indicating where the files have been saved
  cat("Combined cohesive networks GeoJSON file for group has been saved to:", combined_CN_geojson_file, "\n")
  cat("Combined cohesive networks GeoPackage file for group has been saved to:", combined_CN_gpkg_file, "\n")

  # Define the path for the PMtiles
  combined_CN_pmtiles = glue::glue("{output_folder}/combined_CN_{date_folder}_OS.pmtiles")
  
  # Construct the Tippecanoe command for the current group
  command_tippecanoe = paste0(
    'tippecanoe -o ', combined_CN_pmtiles,
    ' --name="', 'Scottish_Coherent_Networks', 
    ' --layer=coherent_networks',
    ' --attribution="University of Leeds"',
    ' --minimum-zoom=6',
    ' --maximum-zoom=13',
    ' --maximum-tile-bytes=5000000',
    ' --simplification=10',
    ' --buffer=5',
    ' -rg',
    ' --force ',
    combined_CN_geojson_file
  )

  # Execute the command and capture output
  system_output = system(command_tippecanoe, intern = TRUE)
  cat("Tippecanoe output for group :\n", system_output, "\n")
  
}