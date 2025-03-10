# Aim: combine regional outputs, create and upload outputs

library(tidyverse)
library(targets)
library(tidygraph)
library(osmextract)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(osmactive)
tar_source()

parameters = jsonlite::read_json("parameters.json", simplifyVector = T)
lads = sf::read_sf("inputdata/boundaries/la_regions_scotland_bfe_simplified_2023.geojson")
date_folder = parameters$date_routing
output_folder = file.path("outputdata", date_folder)

# # Start with Glasgow:
region_names = unique(lads$Region)[c(3, 4, 1, 6, 2, 5, 7)] |>
  # Reverse to build smallest first:
  rev()
# To build just 1 region for testing:
# region_names = region_names[5] # Edinburgh

cities_region_names = lapply(
  region_names,
  function(region) {
    cities_in_region = lads |>
      filter(Region == region) |>
      pull(LAD23NM) |>
      unique()
  }
)
names(cities_region_names) = region_names
region_names_lowercase = snakecase::to_snake_case(region_names)

# Build route networks:
region = region_names[1]
for (region in region_names[1:7]) {
  message("Processing region: ", region)
  parameters$region = region
  jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)
  targets::tar_make()
}

default_wd = "/workspaces/npt/"

if (getwd() != default_wd && dir.exists(default_wd) ) {
  setwd(default_wd)
  message("Changed working directory from ", getwd(), " to ", default_wd)
}

# CbD classification of networks ---------------------------------------------
GENERATE_CDB = TRUE

if (GENERATE_CDB) {
  # See https://github.com/nptscot/osmactive/blob/main/code/classify-roads.R and traffic-volumes.R
  f_traffic = "scottraffic/final_estimates_Scotland_higherror_discarded_2025-03.gpkg"
  if (!file.exists(f_traffic)) {
    system("gh repo clone nptscot/scottraffic")
    file.remove(f_traffic)
    setwd("scottraffic")
    system("gh release list")
    system("gh release download v7")
    setwd("..")
  }
  traffic_volumes_scotland = sf::read_sf(f_traffic) |> 
    sf::st_transform(4326) 

  # Generate cycle_net: forcing update:
  # osm_national = get_travel_network("Scotland", force_download = TRUE)
  options(timeout=30000)
  osm_national = osmactive::get_travel_network("Scotland")
  if (nrow(osm_national) < 100000) {
    stop("The current OSM data for Scotland might be incomplete. Please re-downloading with force_download = TRUE.")
  }
  # saveRDS(osm_national, "inputdata/osm_national_2024_05_23")

  # Generate road segment midpoints
  osm_centroids = osm_national |> 
    sf::st_point_on_surface() |> 
    select(osm_id)

  # Run for each region
  # Set the number of cores to use
  num_cores = min(parallel::detectCores() - 1, 10)
  registerDoParallel(num_cores)
  region = region_names[1]
  cbd_filename = glue::glue(output_folder, "/cbd_layer_{date_folder}.geojson")

  for (region in region_names) {
  # foreach(region = region_names) %dopar% {
    message("Processing region: ", region)
    region_geom = lads |> 
      filter(Region == region)
    district_names = region_geom$LAD23NM
    
    # Run for each district within each Scottish region
    district = district_names[1]
    for (district in district_names) {
      message("Processing district: ", district)
      district_geom = region_geom |> 
        filter(LAD23NM == district)
      district_centroids = osm_centroids[district_geom, ]
      osm_district = osm_national |>
        filter(osm_id %in% district_centroids$osm_id)
      nrow(osm_district) / nrow(osm_national)
      cycle_net = osmactive::get_cycling_network(osm_district)
      drive_net = osmactive::get_driving_network_major(osm_district)
      cycle_net = osmactive::distance_to_road(cycle_net, drive_net)
      cycle_net = osmactive::classify_cycle_infrastructure(cycle_net, include_mixed_traffic = TRUE)
      drive_net = osmactive::clean_speeds(drive_net)
      # summary(drive_net$maxspeed_clean) # TODO: move to osmactive?
      cycle_net = osmactive::clean_speeds(cycle_net)      
      drive_net = osmactive::estimate_traffic(drive_net)
      cycle_net = osmactive::estimate_traffic(cycle_net) |> 
        rename(assumed_traffic_cyclenet = assumed_volume)
      
      # See https://github.com/acteng/network-join-demos
      # Do we really need this?
      cycle_net_joined_polygons = stplanr::rnet_join(
        rnet_x = cycle_net,
        rnet_y = drive_net |>
          transmute(
            maxspeed_drivenet = maxspeed_clean,
            highway_drivenet = highway,
            assumed_traffic_drivenet = assumed_volume
          ) |>
          sf::st_cast(to = "LINESTRING"),
        dist = 20,
        segment_length = 10
      )
      
      # group by + summarise stage
      cycleways_with_road_data_df = cycle_net_joined_polygons |>
        sf::st_drop_geometry() |>
        group_by(osm_id) |>
        summarise(
          maxspeed_drivenet = osmactive:::most_common_value(maxspeed_drivenet),
          highway_drivenet = osmactive:::most_common_value(highway_drivenet),
          assumed_traffic_drivenet = osmactive:::most_common_value(assumed_traffic_drivenet)
        ) |>
        mutate(
          maxspeed_drivenet = as.numeric(maxspeed_drivenet),
          assumed_traffic_drivenet = as.numeric(assumed_traffic_drivenet)
        )
      
      # join back onto cycle_net
      cycle_net_joined = left_join(
        cycle_net,
        cycleways_with_road_data_df
      )
      
      cycle_net_joined = cycle_net_joined |>
        mutate(
          final_speed = case_when(
            !is.na(maxspeed_clean) ~ maxspeed_clean,
            TRUE ~ maxspeed_drivenet
          ),
          assumed_traffic = case_when(
            !is.na(assumed_traffic_cyclenet) ~ assumed_traffic_cyclenet,
            TRUE ~ assumed_traffic_drivenet
          )
        )
      
      traffic_volumes_region = traffic_volumes_scotland[district_geom, ] # this is now a simple district outline
      cycle_net_traffic_polygons = stplanr::rnet_join(
        max_angle_diff = 30,
        rnet_x = cycle_net_joined,
        rnet_y = traffic_volumes_region |>
          transmute(
            name_1, road_classification, pred_flows
          ) |>
          sf::st_cast(to = "LINESTRING"),
        dist = 15,
        segment_length = 10
      )
      
      # group by + summarise stage
      cycleways_with_traffic_df = cycle_net_traffic_polygons |>
        st_drop_geometry() |>
        group_by(osm_id) |>
        summarise(
          pred_flows = median(pred_flows),
          road_classification = osmactive:::most_common_value(road_classification),
          name_1 = osmactive:::most_common_value(name_1)
        )
      
      # join back onto cycle_net
      cycle_net_traffic = left_join(cycle_net_joined, cycleways_with_traffic_df)
      
      # Use original traffic estimates in some cases
      # e.g. where residential/service roads have been misclassified as A/B/C roads
      cycle_net_traffic = cycle_net_traffic |>
        mutate(
          final_traffic = case_when(
            cycle_segregation == "Off Road Cycleway" ~ NA,
            # TODO: Check if shared footways or tracks should be NA
            # Default: no, because it's useful to know the traffic level on parallel road
            # cycle_segregation == "Segregated Track (Wide)" ~ NA,
            # cycle_segregation == "Segregated Track (Narrow)" ~ NA,
            # cycle_segregation == "Shared Footway" ~ NA,
            highway %in% c("residential", "service") & road_classification %in% c("A Road", "B Road", "Classified Unnumbered") & pred_flows >= 4000 ~ assumed_traffic,
            !is.na(pred_flows) ~ pred_flows,
            TRUE ~ assumed_traffic
          )
        )

      cycle_net_traffic = level_of_service(cycle_net_traffic)
            
      cbd_layer = cycle_net_traffic |>
        transmute(
          osm_id,
          highway,
          `Speed limit` = final_speed,
          `Infrastructure type` = cycle_segregation,
          `Level of Service`,
          `Traffic volume category` = case_when(
           final_traffic >= 0 & final_traffic < 1999.5 ~ "0 to 1999",
            final_traffic >= 1999.5 & final_traffic < 3999.5 ~ "2000 to 3999",
            final_traffic >= 3999.5 ~ "4000+",
            TRUE ~ NA_character_
          )
        )
      # save file for individual district
      district_name = district_geom$LAD23NM |> 
        snakecase::to_snake_case()
      cbd_filename = paste0(output_folder, "/cbd_layer_", district_name, ".geojson")
      # Delete the file if it already exists (delete_dsn issues):
      if (file.exists(cbd_filename)) {
        file.remove(cbd_filename)
      }
      sf::write_sf(cbd_layer, cbd_filename, delete_dsn = FALSE)
    }
  }

  # Combine all CBD files into a single file
  cbd_files = list.files(output_folder, pattern = "cbd_layer_.*\\.geojson$", full.names = TRUE)
  cbd_files = cbd_files[!grepl("cbd_layer_\\d{4}-\\d{2}-\\d{2}\\.geojson$", cbd_files)]
  # check the length of cbd_files should equal to length of lads$LAD23NM
  if (length(cbd_files) != length(lads$LAD23NM)) {
    stop("Number of CBD files does not match number of districts.")
  }
  # Create an empty cbd_layers and cbd_layer
  cbd_layers = sf::st_sf(geometry = st_sfc())
  cbd_layer_f = sf::st_sf(geometry = st_sfc())
  cbd_layers = lapply(cbd_files, sf::read_sf)
  cbd_layer_f = do.call(rbind, cbd_layers)
  cbd_filename = paste0(output_folder, "/cbd_layer_", date_folder, ".geojson")
  # Update traffic volumes for off road cycleways
  cbd_layer_f = cbd_layer_f |>
    mutate(
      `Traffic volume category` = case_when(
        `Infrastructure type` == "Off Road Cycleway" ~ NA_character_,
        # highway %in% c("footway", "pedestrian", "steps") ~ NA_character_,
        TRUE ~ `Traffic volume category`
      )
    )
  sf::write_sf(cbd_layer_f, cbd_filename, delete_dsn = FALSE)
  fs::file_size(cbd_filename)

  # PMTiles:
  pmtiles_msg = paste(
    glue::glue("tippecanoe -o {output_folder}/cbd_layer_{date_folder}.pmtiles"),
    "--name=cbd_layer",
    "--layer=cbd_layer",
    "--attribution=UniversityofLeeds",
    "--minimum-zoom=6",
    "--maximum-zoom=13",
    "--drop-smallest-as-needed",
    "--maximum-tile-bytes=2000000",
    "--simplification=10",
    "--buffer=5",
    glue::glue("--force  {cbd_filename}"),
    collapse = " "
  )
  system(pmtiles_msg)

  print(glue::glue("Generating PMTiles at {output_folder}/cbd_layer_{date_folder}.pmtiles"))
}

# Generate coherent network -------------------------------------------------
# Read the open roads data outside the loop for only once

if (parameters$generate_CN_start) {
  os_file_path = "inputdata/open_roads_scotland.gpkg"
  if (!file.exists(os_file_path)) {
    setwd("inputdata")
    system("gh release download OS_network --skip-existing")
    setwd("..")
  }
  os_scotland = sf::read_sf(os_file_path)
  sf::st_geometry(os_scotland) = "geometry"

  osm_file_path = "inputdata/connectivity_fixed_osm.gpkg"
  if (!file.exists(osm_file_path)) {
    setwd("inputdata")
    system("gh release download OSM_fixed --skip-existing")
    setwd("..")
  }
  osm_scotland = sf::read_sf(osm_file_path)
  sf::st_geometry(osm_scotland) = "geometry"

  message("Running corenet_build function")
  if (parameters$coherent_sources == "OS") {
      corenet_build_OS(os_scotland, osm_scotland, region_names,cities_region_names)
  } else if (parameters$coherent_sources == "OSM") {
      corenet_build_OSM(osm_scotland, region_names,cities_region_names)
  } else {
      stop("Invalid value for parameters$coherent_sources. Expected 'OS' or 'OSM'.")
  }
} else {
  message("parameters$generate_CN_start is FALSE, skipping corenet_build.")
}

# # Test cn for one LA: ------------------
# output_folder_region = file.path(output_folder, region_names_lowercase[1])
# list.files(output_folder_region)
# output_folder_region_cn = file.path(output_folder_region, "coherent_networks_OS")
# list.files(output_folder_region_cn)
# #  [8] "city_of_edinburgh_2024-09-30_4_coherent_network.pmtiles"   
# # cn_name = glue::glue("{snakecase::to_snake_case(cities_region_names[[1]][1])}_{date_folder}_4_coherent_network.pmtiles")
# cn_name =  glue::glue("city_of_edinburgh_{date_folder}_4_coherent_network.pmtiles")
# la_cn_path = file.path(output_folder_region_cn, cn_name)
# file.exists(la_cn_path)
# cn_test = sf::read_sf(la_cn_path)
# names(cn_test)
# table(cn_test$road_function)
# mapview::mapview(cn_test, zcol = "road_function")

# Combine regional outputs 

GENERATE_PMTILES = TRUE

if (GENERATE_PMTILES) {
  # Combine regional route networks:
  subfolders = list.dirs(output_folder, full.names = TRUE, recursive = FALSE)

  combined_network_list = lapply(subfolders, function(folder) {
    message(glue::glue("Processing folder: {folder}"))
    combined_network_file = paste0(folder, "/combined_network_tile.geojson")
    if (file.exists(combined_network_file)) {
      network = sf::read_sf(combined_network_file)
    } # nolint
  })
  # TODO: try with stplanr:::bind_sf(combined_network_list)
  combined_network = dplyr::bind_rows(combined_network_list)
  # # With do.call and rbind:
  # combined_network = do.call(rbind, combined_network_list)
  combined_network |>
    sample_n(10000) |>
    sf::st_geometry() |>
    plot()
  dim(combined_network) # ~700k rows for full build, 33 columns
  sf::write_sf(combined_network, file.path(output_folder, "combined_network_tile.geojson"), delete_dsn = TRUE)

  # Same for simplified_network.geojson:
  simplified_network_list = lapply(subfolders, function(folder) {
    simplified_network_file = paste0(folder, "/simplified_network.geojson")
    if (file.exists(simplified_network_file)) {
      network = sf::read_sf(simplified_network_file)
    }
  })
  simplified_network = dplyr::bind_rows(simplified_network_list)
  dim(simplified_network) # ~400k rows for full build, 33 columns
  sf::write_sf(simplified_network, file.path(output_folder, "simplified_network.geojson"), delete_dsn = TRUE)

  # Combine zones data:
  # DataZones file path: data_zones.geojson
  all_zones_tile_files = list()

  # Iterate over each region to collect all GeoJSON files
  for (region in region_names_lowercase) {
    # Define the region folder path
    region_folder = file.path(output_folder, region)
    
    # List GeoJSON files in the region folder
    zones_tile_files = list.files(region_folder, pattern = "data_zones.*\\.geojson$", full.names = TRUE)
    
    # Append the files to the list
    all_zones_tile_files = c(all_zones_tile_files, zones_tile_files)
  }

  # Check if there are any files
  if (length(all_zones_tile_files) > 0) {
    # Read the GeoJSON files
    zones_tiles = lapply(all_zones_tile_files, sf::st_read)
    
    # Combine the GeoJSON files
    combined_zones_tiles = do.call(rbind, zones_tiles)
    
    # Define the output file name
    output_file = file.path(output_folder, glue::glue("data_zones_{date_folder}.geojson"))
    
    # Write the combined GeoJSON file to the output folder
    sf::write_sf(combined_zones_tiles, output_file, delete_dsn = TRUE)
    
    # Message to indicate successful file saving
    message(glue::glue("Combined geojson file saved: {output_file}"))
  } else {
    message("No geojson files found in any of the specified folders.")
  }

  # convert data_zones_{date_folder}.geojson to pmtiles

  command_tippecanoe = paste(
    glue::glue("tippecanoe -o {output_folder}/data_zones_{date_folder}.pmtiles"),
    "--name=data_zones",
    "--layer=data_zones",
    "--attribution=UniverstyofLeeds",
    "--minimum-zoom=6",
    "-zg",
    "--coalesce-smallest-as-needed",
    "--detect-shared-borders",
    "--extend-zooms-if-still-dropping",
    "--maximum-tile-bytes=2000000",
    "--simplification=10",
    "--buffer=5",
    glue::glue("--force  {output_folder}/data_zones_{date_folder}.geojson"),
    collapse = " "
  )
  responce = system(command_tippecanoe, intern = TRUE)   

  # combine od_commute_subset, zones_stats, school_stats, rnet_commute_fastest, rnet_primary_fastest, rnet_secondary_fastest, rnet_utility_fastest, and combined_network
  # Initialize lists to store all files
  od_commute_subsets = list()
  zones_stats_list = list()
  school_stats_list = list()
  rnet_commute_fastest_list = list()
  rnet_primary_fastest_list = list()
  rnet_secondary_fastest_list = list()
  rnet_utility_fastest_list = list()
  combined_network_list = list()

  # Iterate over each region to collect all relevant files
  for (region in region_names_lowercase) {
    # Define the region folder path
    region_folder = file.path(output_folder, region)
    
    # Read and combine RDS files
    if (file.exists(file.path(region_folder, "od_commute_subset.Rds"))) {
      od_commute_subsets[[region]] = readRDS(file.path(region_folder, "od_commute_subset.Rds"))
    }
    if (file.exists(file.path(region_folder, "zones_stats.Rds"))) {
      zones_stats_list[[region]] = readRDS(file.path(region_folder, "zones_stats.Rds"))
    }
    if (file.exists(file.path(region_folder, "school_stats.Rds"))) {
      school_stats_list[[region]] = readRDS(file.path(region_folder, "school_stats.Rds"))
    }

    # Read and combine GeoPackage files
    if (file.exists(file.path(region_folder, "rnet_commute_fastest.gpkg"))) {
      rnet_commute_fastest_list[[region]] = sf::st_read(file.path(region_folder, "rnet_commute_fastest.gpkg"))
    }
    if (file.exists(file.path(region_folder, "rnet_primary_fastest.gpkg"))) {
      rnet_primary_fastest_list[[region]] = sf::st_read(file.path(region_folder, "rnet_primary_fastest.gpkg"))
    }
    if (file.exists(file.path(region_folder, "rnet_secondary_fastest.gpkg"))) {
      rnet_secondary_fastest_list[[region]] = sf::st_read(file.path(region_folder, "rnet_secondary_fastest.gpkg"))
    }
    if (file.exists(file.path(region_folder, "rnet_utility_fastest.gpkg"))) {
      rnet_utility_fastest_list[[region]] = sf::st_read(file.path(region_folder, "rnet_utility_fastest.gpkg"))
    }
    if (file.exists(file.path(region_folder, "combined_network.gpkg"))) {
      combined_network_list[[region]] = sf::st_read(file.path(region_folder, "combined_network.gpkg"))
    }
  }

  # define function to find common columns
  find_common_columns = function(df_list) {
    common_columns = Reduce(intersect, lapply(df_list, colnames))
    return(common_columns)
  }

  # Combine the data
  common_columns_od_commute = find_common_columns(od_commute_subsets)
  od_commute_subsets = lapply(od_commute_subsets, function(df) df[, common_columns_od_commute, drop = FALSE])
  combined_od_commute_subset = do.call(rbind, od_commute_subsets)

  # For zones_stats_list
  common_columns_zones_stats = find_common_columns(zones_stats_list)
  zones_stats_list = lapply(zones_stats_list, function(df) df[, common_columns_zones_stats, drop = FALSE])
  combined_zones_stats = do.call(rbind, zones_stats_list)

  # For school_stats_list
  common_columns_school_stats = find_common_columns(school_stats_list)
  school_stats_list = lapply(school_stats_list, function(df) df[, common_columns_school_stats, drop = FALSE])
  combined_school_stats = do.call(rbind, school_stats_list)

  combined_rnet_commute_fastest = do.call(rbind, rnet_commute_fastest_list)
  combined_rnet_primary_fastest = do.call(rbind, rnet_primary_fastest_list)
  combined_rnet_secondary_fastest = do.call(rbind, rnet_secondary_fastest_list)
  combined_rnet_utility_fastest = do.call(rbind, rnet_utility_fastest_list)
  combined_combined_network = do.call(rbind, combined_network_list)

  # Save the combined data
  saveRDS(combined_od_commute_subset, file.path(output_folder, "od_commute_subset.Rds"))
  saveRDS(combined_zones_stats, file.path(output_folder, "zones_stats.Rds"))
  saveRDS(combined_school_stats, file.path(output_folder, "school_stats.Rds"))
  sf::write_sf(combined_rnet_commute_fastest, file.path(output_folder, "rnet_commute_fastest.gpkg"))
  sf::write_sf(combined_rnet_primary_fastest, file.path(output_folder, "rnet_primary_fastest.gpkg"))
  sf::write_sf(combined_rnet_secondary_fastest, file.path(output_folder, "rnet_secondary_fastest.gpkg"))
  sf::write_sf(combined_rnet_utility_fastest, file.path(output_folder, "rnet_utility_fastest.gpkg"))
  sf::write_sf(combined_combined_network, file.path(output_folder, "combined_network.gpkg"), delete_dsn = TRUE)
  # Same for school_locations.geojson

  school_locations_file = glue::glue("{output_folder}/school_locations.geojson")
  if (file.exists(school_locations_file)) {
    school_locations = sf::read_sf(school_locations_file)
  }

  # Export SchoolStats:

  school_stats_file = glue::glue("{output_folder}/school_stats.Rds")
  if (file.exists(school_stats_file)) {
    school_stats = readRDS(school_stats_file)
  }

  export_zone_json(school_stats, "SeedCode", path = output_folder)

  # Same for zones_stats.Rds

  zones_stats_file = glue::glue("{output_folder}/zones_stats.Rds")
  if (file.exists(zones_stats_file)) {
    # Read the RDS file
    zones_stats = readRDS(zones_stats_file)
    
    # Call the function to export the data to JSON, assuming the function and its parameters are correctly defined
    export_zone_json(zones_stats, "DataZone", path = output_folder)
  } else {
    # Optionally, you can add a message if the file does not exist
    message("File does not exist: ", zones_stats_file)
  }

  # Combined network tiling
  # Check the combined_network_tile.geojson file is there:
  if (!file.exists(glue::glue(output_folder, "/combined_network_tile.geojson"))) {
    stop("combined_network_tile.geojson file not found.")
  } else {
    command_tippecanoe = paste(
      glue::glue("tippecanoe -o {output_folder}/rnet_{date_folder}.pmtiles"),
      "--name=rnet",
      "--layer=rnet",
      "--attribution=UniverstyofLeeds",
      "--minimum-zoom=6",
      "--maximum-zoom=13",
      "--drop-smallest-as-needed",
      "--maximum-tile-bytes=2000000",
      "--simplification=10",
      "--buffer=5",
      # To ensure that largest values shown on top:
      "--order-by=all_fastest_bicycle_go_dutch",
      glue::glue("--force  {output_folder}/combined_network_tile.geojson"),
      collapse = " "
    )
    responce = system(command_tippecanoe, intern = TRUE)
  }
  # Simplified network tiling

  # Check the combined_network_tile.geojson file is there:
  if (!file.exists(glue::glue(output_folder, "/simplified_network.geojson"))) {
    stop("simplified_network.geojson file not found.")
  } else {
    command_tippecanoe = paste(
      glue::glue("tippecanoe -o {output_folder}/rnet_simplified_{date_folder}.pmtiles"),
      "--name=rnet_simplified",
      "--layer=rnet_simplified",
      "--attribution=UniverstyofLeeds",
      "--minimum-zoom=6",
      "--maximum-zoom=13",
      "--drop-smallest-as-needed",
      "--maximum-tile-bytes=2000000",
      "--simplification=10",
      "--buffer=5",
      # To ensure that largest values shown on top:
      "--order-by=all_fastest_bicycle_go_dutch",
      glue::glue("--force  {output_folder}/simplified_network.geojson"),
      collapse = " ")
    responce = system(command_tippecanoe, intern = TRUE)
  }

  # Dasymetric population data:
  b_verylow = read_TEAMS("open_data/os_buildings/buildings_low_nat_lsoa_split.Rds")
  b_low = read_TEAMS("open_data/os_buildings/buildings_low_reg_lsoa_split.Rds")
  b_med = read_TEAMS("open_data/os_buildings/buildings_med_lsoa_split.Rds")
  b_high = read_TEAMS("open_data/os_buildings/buildings_high_lsoa_split.Rds")
  # TODO: Check zones_tile
  zones = sf::st_drop_geometry(zones_tiles)
  zones = do.call(rbind, zones) 
  zones = sf::st_set_geometry(zones, NULL)
  b_verylow = dplyr::left_join(b_verylow, zones, by = c("geo_code" = "DataZone"))
  b_low = dplyr::left_join(b_low, zones, by = c("geo_code" = "DataZone"))
  b_med = dplyr::left_join(b_med, zones, by = c("geo_code" = "DataZone"))
  b_high = dplyr::left_join(b_high, zones, by = c("geo_code" = "DataZone"))
  make_geojson_zones(b_verylow, file.path(output_folder, "dasymetric_verylow.geojson"))
  make_geojson_zones(b_low, file.path(output_folder, "dasymetric_low.geojson"))
  make_geojson_zones(b_med, file.path(output_folder, "dasymetric_med.geojson"))
  make_geojson_zones(b_high, file.path(output_folder, "dasymetric_high.geojson"))

  tippecanoe_verylow = paste(
    glue::glue("tippecanoe -o {output_folder}/dasymetric_verylow.pmtiles"),
    "--name=dasymetric",
    "--layer=dasymetric",
    "--attribution=OS",
    "--minimum-zoom=4",
    "--maximum-zoom=6",
    "--coalesce-smallest-as-needed",
    "--detect-shared-borders",
    "--maximum-tile-bytes=2000000",
    "--simplification=1",
    "--buffer=5",
    glue::glue("--force  {output_folder}/dasymetric_verylow.geojson"),
    collapse = " "
  )

  tippecanoe_low = paste(
    glue::glue("tippecanoe -o {output_folder}/dasymetric_low.pmtiles"),
    "--name=dasymetric",
    "--layer=dasymetric",
    "--attribution=OS",
    "--minimum-zoom=7",
    "--maximum-zoom=9",
    "--coalesce-smallest-as-needed",
    "--detect-shared-borders",
    "--maximum-tile-bytes=2000000",
    "--simplification=1",
    "--buffer=5",
    glue::glue("--force  {output_folder}/dasymetric_low.geojson"),
    collapse = " "
  )

  tippecanoe_med = paste(
    glue::glue("tippecanoe -o {output_folder}/dasymetric_med.pmtiles"),
    "--name=dasymetric",
    "--layer=dasymetric",
    "--attribution=OS",
    "--minimum-zoom=10",
    "--maximum-zoom=14",
    "--coalesce-smallest-as-needed",
    "--detect-shared-borders",
    "--maximum-tile-bytes=2000000",
    "--simplification=2",
    "--buffer=5",
    glue::glue("--force  {output_folder}/dasymetric_med.geojson"),
    collapse = " "
  )

  tippecanoe_high = paste(
    glue::glue("tippecanoe -o {output_folder}/dasymetric_high.pmtiles"),
    "--name=dasymetric",
    "--layer=dasymetric",
    "--attribution=OS",
    "-zg",
    "--minimum-zoom=15",
    "--maximum-zoom=15",
    "--extend-zooms-if-still-dropping",
    "--coalesce-smallest-as-needed",
    "--detect-shared-borders",
    "--maximum-tile-bytes=2000000",
    "--simplification=5",
    "--buffer=5",
    glue::glue("--force  {output_folder}/dasymetric_high.geojson"),
    collapse = " "
  )

  tippecanoe_join = paste(
    glue::glue("tile-join -o {output_folder}/dasymetric.pmtiles -pk --force"),
    glue::glue("{output_folder}/dasymetric_verylow.pmtiles"),
    glue::glue("{output_folder}/dasymetric_low.pmtiles"),
    glue::glue("{output_folder}/dasymetric_med.pmtiles"),
    glue::glue("{output_folder}/dasymetric_high.pmtiles"),
    collapse = " "
  )
  responce = system(tippecanoe_join, intern = TRUE)

  dir = getwd()
  command_start = "bash -c "
  command_cd = paste0("cd ", tolower(substr(dir, 1, 1)), substr(dir, 2, nchar(dir)), "/")
  command_all = paste(c(
    command_cd, tippecanoe_verylow, tippecanoe_low,
    tippecanoe_med, tippecanoe_high, tippecanoe_join
  ), collapse = "; ")
  command_all = paste0(command_start, '"', command_all, '"')

  responce = system(command_all, intern = TRUE)

  message(glue::glue("Generated PMTiles for data zones at {output_folder}/data_zones_{date_folder}.pmtiles"))
  message(glue::glue("Generated PMTiles for combined networks at {output_folder}/rnet_{date_folder}.pmtiles"))
  message(glue::glue("Generated PMTiles for simplified networks at {output_folder}/rnet_simplified_{date_folder}.pmtiles"))
  message(glue::glue("Generated PMTiles for dasymetric data level very low at {output_folder}/dasymetric_verylow.pmtiles"))
  message(glue::glue("Generated PMTiles for dasymetric data level low at {output_folder}/dasymetric_low.pmtiles"))
  message(glue::glue("Generated PMTiles for dasymetric data level medium at {output_folder}/dasymetric_med.pmtiles"))
  message(glue::glue("Generated PMTiles for dasymetric data level high at {output_folder}/dasymetric_high.pmtiles"))
  message(glue::glue("Joined PMTiles for all dasymetric data levels at {output_folder}/dasymetric.pmtiles"))
}

PUSH_TO_GITHUB = TRUE

if (PUSH_TO_GITHUB) {
  # Check contents of outputdata folder:
  outputdata_files = list.files(output_folder)
  outputdata_files

  commit = gert::git_log(max = 1)
  message("Commit: ", commit)
  full_build =
    # isFALSE(parameters$geo_subset) &&
    isFALSE(parameters$open_data_build) &&
      parameters$max_to_route > 20e3
  is_linux = Sys.info()[["sysname"]] == "Linux"
  if (full_build) {
    setwd(glue::glue(getwd(),"/", output_folder))
    system("gh release list")
    v = glue::glue("v{date_folder}")
    f = list.files(pattern = "pmtiles|gpkg|zip")
    f
    # Piggyback fails with error message so commented and using cust
    # piggyback::pb_upload(f)
    msg = glue::glue("gh release create {v} --generate-notes")
    message("Creating new release and folder to save the files: ", v)
    dir.create(v)
    message("Going to try to upload the following files: ", paste0(f, collapse = ", "))
    message("With sizes: ", paste0(fs::file_size(f), collapse = ", "))
    system(msg)
    for (i in f) {
      gh_release_upload(file = i, tag = v)
      # Move into a new directory
      file.copy(from = i, to = file.path(v, i))
    }
    message("Files stored in output folder: ", v)
    message("Which contains: ", paste0(list.files(v), collapse = ", "))
    # For specific version:
    # system("gh release create v0.0.1 --generate-notes")
    file.remove(f)
    setwd("..")
  } else {
    message("Not full build or gh command line tool not available")
    message("Not uploading files: manually move contents of outputdata (see upload_data target for details)")
  }

}
# # Copy pmtiles into app folder (optional)
# app_tiles_directory = "../nptscot.github.io/tiles"
# list.files(app_tiles_directory) # list current files
# pmtiles = list.files("outputdata", pattern = "*05-23*.+pmtiles", full.names = TRUE)
# pmtiles_new = file.path(app_tiles_directory, basename(pmtiles))
# file.copy(pmtiles, pmtiles_new, overwrite = TRUE)

# # Test for central Edinburgh (optional)
# edinburgh_central = zonebuilder::zb_zone("Edinburgh", n_circles = 2)
# tar_load(simplified_network)
# simplified_central = simplified_network[edinburgh_central, ]
# mapview::mapview(simplified_central)
