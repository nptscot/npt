# Aim: combine regional outputs, create and upload outputs

library(tidyverse)
library(targets)
library(tidygraph)
library(osmextract)
tar_source()

parameters = jsonlite::read_json("parameters.json", simplifyVector = T)
lads = sf::read_sf("inputdata/boundaries/la_regions_2023.geojson")
date_folder = parameters$date_routing
output_folder = file.path("outputdata", date_folder)

# Start with Glasgow:
region_names = unique(lads$Region)[c(3, 2, 1, 4, 5, 6)] 
# Test for 2 regions:
# region_names = unique(lads$Region)[c(3, 4)]
cities_region_names = lapply(
  region_names,
  function(region) {
    cities_in_region = lads |>
      filter(Region == region) |>
      pull(LAD23NM) |>
      unique() # Ensure unique city names
  }
)
names(cities_region_names) = region_names
region_names_lowercase = snakecase::to_snake_case(region_names)

# First loop: Attempt to process each region and capture any failures
region = region_names[1]
for (region in region_names) {
  message("Processing region: ", region)
  parameters$region = region
  jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)
  targets::tar_make()
}

# CbD classification of networks ---------------------------------------------

remotes::install_github("nptscot/osmactive")
library(osmactive)
# See https://github.com/nptscot/osmactive/blob/main/code/classify-roads.R and traffic-volumes.R
f_traffic = "scottraffic/final_estimates_Scotland.gpkg"
if (!file.exists(f_traffic)) {
  system("gh repo clone nptscot/scottraffic")
  setwd("scottraffic")
  system("gh release download v4")
  setwd("..")
}
traffic_volumes_scotland = sf::read_sf(f_traffic)

# Generate cycle_net - this is slow, we should save the file
osm_national = get_travel_network("Scotland")
# saveRDS(osm_national, "inputdata/osm_national_2024_05_23")

# Generate road segment midpoints
osm_centroids = osm_national |> 
  sf::st_point_on_surface() |> 
  select(osm_id)
  

# Run for each district within each Scottish region
region_geom = lads |> 
  filter(Region == region)

district = region_geom[1,]
for (district in region_geom) {
  district_centroids = osm_centroids[district, ]
  district_centroids = sf::st_drop_geometry(district_centroids)
  osm_district = inner_join(osm_national, district_centroids)
  nrow(osm_district) / nrow(osm_national)
  # 6% network, could be 20x+ slower for Scotland
  # [1] 0.01831887 for East Ayrshire (raw road data)
  # [1] 0.01815918 for East Ayrshire (using line midpoints)
  # # # ---
  
  cycle_net = osmactive::get_cycling_network(osm_district)
  drive_net = osmactive::get_driving_network_major(osm_district)
  cycle_net = osmactive::distance_to_road(cycle_net, drive_net)
  cycle_net = osmactive::classify_cycle_infrastructure(cycle_net)
  
  drive_net = osmactive::clean_speeds(drive_net)
  cycle_net = osmactive::clean_speeds(cycle_net)
  
  drive_net = osmactive::estimate_traffic(drive_net)
  cycle_net = osmactive::estimate_traffic(cycle_net)
  
  # See tutorial: https://github.com/acteng/network-join-demos
  cycle_net_joined_polygons = stplanr::rnet_join(
    rnet_x = cycle_net,
    rnet_y = drive_net |>
      transmute(
        maxspeed_road = maxspeed_clean,
        highway_join = highway,
        volume_join = assumed_volume
      ) |>
      sf::st_cast(to = "LINESTRING"),
    dist = 20,
    segment_length = 10
  )
  
  # group by + summarise stage
  cycleways_with_road_speeds_df = cycle_net_joined_polygons |>
    sf::st_drop_geometry() |>
    group_by(osm_id) |>
    summarise(
      maxspeed_road = osmactive:::most_common_value(maxspeed_road),
      highway_join = osmactive:::most_common_value(highway_join),
      volume_join = osmactive:::most_common_value(volume_join)
    ) |>
    mutate(
      maxspeed_road = as.numeric(maxspeed_road),
      volume_join = as.numeric(volume_join)
    )
  
  # join back onto cycle_net
  cycle_net_joined = left_join(cycle_net, cycleways_with_road_speeds_df)
  
  cycle_net_joined = cycle_net_joined |>
    mutate(
      final_speed = case_when(
        !is.na(maxspeed_clean) ~ maxspeed_clean,
        TRUE ~ maxspeed_road
      ),
      final_volume = case_when(
        !is.na(assumed_volume) ~ assumed_volume,
        TRUE ~ volume_join
      )
    )
  
  traffic_volumes_region = traffic_volumes_scotland[osm_district, ] # change this to an inner join or a simple polygon district outline to speed up the build?
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
        detailed_segregation == "Cycle track" ~ 0,
        highway %in% c("residential", "service") & road_classification %in% c("A Road", "B Road", "Classified Unnumbered") & pred_flows >= 4000 ~ final_volume,
        !is.na(pred_flows) ~ pred_flows,
        TRUE ~ final_volume
      )
    )
  
  # Check results
  
  cycle_net_traffic = cycle_net_traffic |>
    mutate(`Level of Service` = case_when(
      detailed_segregation == "Cycle track" ~ "High",
      detailed_segregation == "Level track" & final_speed <= 30 ~ "High",
      detailed_segregation == "Stepped or footway" & final_speed <= 20 ~ "High",
      detailed_segregation == "Stepped or footway" & final_speed == 30 & final_traffic < 4000 ~ "High",
      detailed_segregation == "Light segregation" & final_speed <= 20 ~ "High",
      detailed_segregation == "Light segregation" & final_speed == 30 & final_traffic < 4000 ~ "High",
      detailed_segregation == "Cycle lane" & final_speed <= 20 & final_traffic < 4000 ~ "High",
      detailed_segregation == "Cycle lane" & final_speed == 30 & final_traffic < 1000 ~ "High",
      detailed_segregation == "Mixed traffic" & final_speed <= 20 & final_traffic < 2000 ~ "High",
      detailed_segregation == "Mixed traffic" & final_speed == 30 & final_traffic < 1000 ~ "High",
      detailed_segregation == "Level track" & final_speed == 40 ~ "Medium",
      detailed_segregation == "Level track" & final_speed == 50 & final_traffic < 1000 ~ "Medium",
      detailed_segregation == "Stepped or footway" & final_speed <= 40 ~ "Medium",
      detailed_segregation == "Stepped or footway" & final_speed == 50 & final_traffic < 1000 ~ "Medium",
      detailed_segregation == "Light segregation" & final_speed == 30 ~ "Medium",
      detailed_segregation == "Light segregation" & final_speed == 40 & final_traffic < 2000 ~ "Medium",
      detailed_segregation == "Light segregation" & final_speed == 50 & final_traffic < 1000 ~ "Medium",
      detailed_segregation == "Cycle lane" & final_speed <= 20 ~ "Medium",
      detailed_segregation == "Cycle lane" & final_speed == 30 & final_traffic < 4000 ~ "Medium",
      detailed_segregation == "Cycle lane" & final_speed == 40 & final_traffic < 1000 ~ "Medium",
      detailed_segregation == "Mixed traffic" & final_speed <= 20 & final_traffic < 4000 ~ "Medium",
      detailed_segregation == "Mixed traffic" & final_speed == 30 & final_traffic < 2000 ~ "Medium",
      detailed_segregation == "Mixed traffic" & final_speed == 40 & final_traffic < 1000 ~ "Medium",
      detailed_segregation == "Level track" ~ "Low",
      detailed_segregation == "Stepped or footway" ~ "Low",
      detailed_segregation == "Light segregation" & final_speed <= 50 ~ "Low",
      detailed_segregation == "Light segregation" & final_speed == 60 & final_traffic < 1000 ~ "Low",
      detailed_segregation == "Cycle lane" & final_speed <= 50 ~ "Low",
      detailed_segregation == "Cycle lane" & final_speed == 60 & final_traffic < 1000 ~ "Low",
      detailed_segregation == "Mixed traffic" & final_speed <= 30 ~ "Low",
      detailed_segregation == "Mixed traffic" & final_speed == 40 & final_traffic < 2000 ~ "Low",
      detailed_segregation == "Mixed traffic" & final_speed == 60 & final_traffic < 1000 ~ "Low",
      detailed_segregation == "Light segregation" ~ "Should not be used",
      detailed_segregation == "Cycle lane" ~ "Should not be used",
      detailed_segregation == "Mixed traffic" ~ "Should not be used",
      TRUE ~ "Unknown"
    )) |>
    dplyr::mutate(`Level of Service` = factor(
      `Level of Service`,
      levels = c("High", "Medium", "Low", "Should not be used"),
      ordered = TRUE
    ))
  
  cbd_layer = cycle_net_traffic |>
    transmute(
      osm_id,
      `Traffic volume` = final_traffic,
      `Speed limit` = final_speed,
      `Infrastructure type` = cycle_segregation,
      `Infrastructure type (detailed)` = detailed_segregation,
      `Level of Service`
    )
  
  sf::write_sf(cbd_layer, "cbd_layer.geojson", delete_dsn = TRUE)
  # library(tmap)
  # tmap_mode("view")
  # tm_shape(cbd_layer |> slice_sample(1000)) + tm_lines("Level of Service", lwd = 2, palette = "viridis")
  
  # PMTiles:
  pmtiles_msg = glue::glue("tippecanoe -o cbd_layer_{date_folder}.pmtiles",
                           "--name=cbd_layer",
                           "--layer=cbd_layer",
                           "--attribution=UniversityofLeeds",
                           "--minimum-zoom=6",
                           "--maximum-zoom=13",
                           "--drop-smallest-as-needed",
                           "--maximum-tile-bytes=5000000",
                           "--simplification=10",
                           "--buffer=5",
                           "--force  cbd_layer.geojson",
                           collapse = " "
  )
  system(pmtiles_msg)
  # Rename and upload:
  file.rename(
    glue::glue("cbd_layer_{date_folder}.pmtiles"),
    glue::glue("outputdata/cbd_layer_{date_folder}.pmtiles")
  )
  file.rename("cbd_layer.geojson", "outputdata/cbd_layer.geojson")
  setwd("outputdata")
  system("gh release list")
  system(glue::glue("gh release upload {date_folder} cbd_layer_{date_folder}.pmtiles"))
}

# Generate coherent network ---------------------------------------------------

# Read the open roads data outside the loop for only once
# Define the path to the file
file_path = "inputdata/open_roads_scotland.gpkg"
if (!file.exists(file_path)) {
  setwd("inputdata")
  system("gh release download OS_network --skip-existing")
  setwd("..")
}
open_roads_scotland = sf::read_sf(file_path)
sf::st_geometry(open_roads_scotland) = "geometry"

# Generate the coherent network for the region
for (region in region_names) {
  message("Processing coherent network for region: ", region)
  region_snake = snakecase::to_snake_case(region)
  coherent_area = cities_region_names[[region]]

  cnet_path = file.path(output_folder, region_snake, "combined_network_tile.geojson")
  combined_net = sf::read_sf(cnet_path) |>
    sf::st_transform(crs = "EPSG:27700")

  folder_path = file.path(output_folder, region_snake, "coherent_networks/")

  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }

  for (city in coherent_area) {
    city_filename = snakecase::to_snake_case(city)
    tryCatch(
      {
        message("Generating coherent network for: ", city)
        city_boundary = filter(lads, LAD23NM == city) |>
          sf::st_transform(crs = "EPSG:27700")

        combined_net_city_boundary = combined_net[sf::st_union(city_boundary), , op = sf::st_intersects]

        min_percentile_value = stats::quantile(combined_net_city_boundary$all_fastest_bicycle_go_dutch, probs = 0.938, na.rm = TRUE)

        open_roads_scotland_city_boundary = open_roads_scotland[sf::st_union(city_boundary), , op = sf::st_intersects]

        OS_combined_net_city_boundary = corenet::cohesive_network_prep(
          base_network = open_roads_scotland_city_boundary,
          influence_network = combined_net_city_boundary,
          city_boundary,
          crs = "EPSG:27700",
          key_attribute = "road_function",
          attribute_values = c("A Road", "B Road", "Minor Road")
        )

        cohesive_network_city_boundary = corenet::corenet(combined_net_city_boundary, OS_combined_net_city_boundary, city_boundary,
          key_attribute = "all_fastest_bicycle_go_dutch",
          crs = "EPSG:27700", dist = 10, threshold = min_percentile_value,
          road_scores = list("A Road" = 1, "B Road" = 1, "Minor Road" = 10000000)
        )

        grouped_network = corenet::coherent_network_group(cohesive_network_city_boundary, key_attribute = "all_fastest_bicycle_go_dutch")

        # Use city name in the filename
        corenet::create_coherent_network_PMtiles(folder_path = folder_path, city_filename = glue::glue("{city_filename}_{date_folder}"), cohesive_network = grouped_network)

        message("Coherent network for: ", city, " generated successfully")
      },
      error = function(e) {
        message(sprintf("An error occurred with %s: %s", city, e$message))    
      }
    )
  }
}

# Combine regional outputs ---------------------------------------------------
output_folders = list.dirs(output_folder)[-1]
regional_output_files = list.files(output_folders[1])
regional_output_files

# Combine regional route networks:
combined_network_list = lapply(output_folders, function(folder) {
  combined_network_file = paste0(folder, "/combined_network_tile.geojson")
  if (file.exists(combined_network_file)) {
    network = sf::read_sf(combined_network_file)
  }
})
# TODO: try with stplanr:::bind_sf(combined_network_list)
combined_network = dplyr::bind_rows(combined_network_list)
# # With do.call and rbind:
# combined_network = do.call(rbind, combined_network_list)
combined_network |>
  sample_n(1000) |>
  select(1) |>
  plot()
sf::write_sf(combined_network, file.path("outputdata", "combined_network_tile.geojson"), delete_dsn = TRUE)

# Same for simplified_network.geojson:
simplified_network_list = lapply(output_folders, function(folder) {
  simplified_network_file = paste0(folder, "/simplified_network.geojson")
  if (file.exists(simplified_network_file)) {
    network = sf::read_sf(simplified_network_file)
  }
})
simplified_network = dplyr::bind_rows(simplified_network_list)
sf::write_sf(simplified_network, file.path("outputdata", "simplified_network.geojson"), delete_dsn = TRUE)


# Combine zones data:
# DataZones file path: data_zones.geojson

zones_tile_file = paste0("outputdata/", date_folder, "/data_zones.geojson")
if (file.exists(zones_tile_file)) {
  zones_tile = sf::read_sf(zones_tile_file)
}

sf::write_sf(zones_tile, file.path("outputdata", "zones_tile.geojson"), delete_dsn = TRUE)

# Same for school_locations.geojson

school_locations_file = "outputdata/school_locations.geojson"
if (file.exists(school_locations_file)) {
  school_locations = sf::read_sf(school_locations_file)
}

# Export SchoolStats:

school_stats_file = "outputdata/school_stats.Rds"
if (file.exists(school_stats_file)) {
  school_stats = readRDS(school_stats_file)
}

export_zone_json(school_stats, "SeedCode", path = "outputdata")

# Same for zones_stats.Rds

zones_stats_file = "outputdata/zones_stats.Rds"
if (file.exists(zones_stats_file)) {
  zones_stats = readRDS(zones_stats_file)
}

export_zone_json(zones_stats, "DataZone", path = "outputdata")

# Combined network tiling
setwd("outputdata")
# Check the combined_network_tile.geojson file is there:
file.exists("combined_network_tile.geojson")
command_tippecanoe = glue::glue("tippecanoe -o rnet_{date_folder}.pmtiles",
  "--name=rnet",
  "--layer=rnet",
  "--attribution=UniverstyofLeeds",
  "--minimum-zoom=6",
  "--maximum-zoom=13",
  "--drop-smallest-as-needed",
  "--maximum-tile-bytes=5000000",
  "--simplification=10",
  "--buffer=5",
  "--force  combined_network_tile.geojson",
  collapse = " "
)

responce = system(command_tippecanoe, intern = TRUE)

# Simplified network tiling
setwd("outputdata")
# Check the combined_network_tile.geojson file is there:
file.exists("simplified_network.geojson")
command_tippecanoe = glue::glue("tippecanoe -o rnet_simplified_{date_folder}.pmtiles",
  "--name=rnet_simplified",
  "--layer=rnet_simplified",
  "--attribution=UniverstyofLeeds",
  "--minimum-zoom=6",
  "--maximum-zoom=13",
  "--drop-smallest-as-needed",
  "--maximum-tile-bytes=5000000",
  "--simplification=10",
  "--buffer=5",
  "--force  simplified_network.geojson",
  collapse = " "
)

responce = system(command_tippecanoe, intern = TRUE)

# Re-set working directory:
setwd("..")

# Dasymetric population data:
b_verylow = read_TEAMS("open_data/os_buildings/buildings_low_nat_lsoa_split.Rds")
b_low = read_TEAMS("open_data/os_buildings/buildings_low_reg_lsoa_split.Rds")
b_med = read_TEAMS("open_data/os_buildings/buildings_med_lsoa_split.Rds")
b_high = read_TEAMS("open_data/os_buildings/buildings_high_lsoa_split.Rds")
zones = sf::st_drop_geometry(zones_tile)
b_verylow = dplyr::left_join(b_verylow, zones, by = c("geo_code" = "DataZone"))
b_low = dplyr::left_join(b_low, zones, by = c("geo_code" = "DataZone"))
b_med = dplyr::left_join(b_med, zones, by = c("geo_code" = "DataZone"))
b_high = dplyr::left_join(b_high, zones, by = c("geo_code" = "DataZone"))
make_geojson_zones(b_verylow, file.path("outputdata", "dasymetric_verylow.geojson"))
make_geojson_zones(b_low, file.path("outputdata", "dasymetric_low.geojson"))
make_geojson_zones(b_med, file.path("outputdata", "dasymetric_med.geojson"))
make_geojson_zones(b_high, file.path("outputdata", "dasymetric_high.geojson"))

tippecanoe_verylow = paste("tippecanoe -o dasymetric_verylow.pmtiles",
  "--name=dasymetric",
  "--layer=dasymetric",
  "--attribution=OS",
  "--minimum-zoom=4",
  "--maximum-zoom=6",
  "--coalesce-smallest-as-needed",
  "--detect-shared-borders",
  "--maximum-tile-bytes=5000000",
  "--simplification=1",
  "--buffer=5",
  "--force dasymetric_verylow.geojson",
  collapse = " "
)

tippecanoe_low = paste("tippecanoe -o dasymetric_low.pmtiles",
  "--name=dasymetric",
  "--layer=dasymetric",
  "--attribution=OS",
  "--minimum-zoom=7",
  "--maximum-zoom=9",
  "--coalesce-smallest-as-needed",
  "--detect-shared-borders",
  "--maximum-tile-bytes=5000000",
  "--simplification=1",
  "--buffer=5",
  "--force dasymetric_low.geojson",
  collapse = " "
)

tippecanoe_med = paste("tippecanoe -o dasymetric_med.pmtiles",
  "--name=dasymetric",
  "--layer=dasymetric",
  "--attribution=OS",
  "--minimum-zoom=10",
  "--maximum-zoom=14",
  "--coalesce-smallest-as-needed",
  "--detect-shared-borders",
  "--maximum-tile-bytes=5000000",
  "--simplification=2",
  "--buffer=5",
  "--force dasymetric_med.geojson",
  collapse = " "
)

tippecanoe_high = paste("tippecanoe -o dasymetric_high.pmtiles",
  "--name=dasymetric",
  "--layer=dasymetric",
  "--attribution=OS",
  "-zg",
  "--minimum-zoom=15",
  "--extend-zooms-if-still-dropping",
  "--coalesce-smallest-as-needed",
  "--detect-shared-borders",
  "--maximum-tile-bytes=5000000",
  "--simplification=5",
  "--buffer=5",
  "--force dasymetric_high.geojson",
  collapse = " "
)

tippecanoe_join = paste("tile-join -o dasymetric.pmtiles -pk --force",
  "dasymetric_verylow.pmtiles",
  "dasymetric_low.pmtiles",
  "dasymetric_med.pmtiles",
  "dasymetric_high.pmtiles",
  collapse = " "
)



if (.Platform$OS.type == "unix") {
  command_cd = "cd outputdata"
  command_all = paste(c(
    command_cd, tippecanoe_verylow, tippecanoe_low,
    tippecanoe_med, tippecanoe_high, tippecanoe_join
  ), collapse = "; ")
} else {
  # Using WSL
  dir = getwd()
  command_start = "bash -c "
  command_cd = paste0("cd /mnt/", tolower(substr(dir, 1, 1)), substr(dir, 3, nchar(dir)), "/outputs")
  command_all = paste(c(
    command_cd, tippecanoe_verylow, tippecanoe_low,
    tippecanoe_med, tippecanoe_high, tippecanoe_join
  ), collapse = "; ")
  command_all = paste0(command_start, '"', command_all, '"')
}
responce = system(command_all, intern = TRUE)

# Copy pmtiles into app folder
app_tiles_directory = "../nptscot.github.io/tiles"
list.files(app_tiles_directory) # list current files
pmtiles = list.files("outputdata", pattern = "pmtiles", full.names = TRUE)
pmtiles_new = file.path(app_tiles_directory, basename(pmtiles))
file.copy(pmtiles, pmtiles_new, overwrite = TRUE)

# Check contents of outputdata folder:
outputdata_files = list.files("outputdata")
outputdata_files

commit = gert::git_log(max = 1)
message("Commit: ", commit)
full_build =
  # isFALSE(parameters$geo_subset) &&
  isFALSE(parameters$open_data_build) &&
    parameters$max_to_route > 20e3
is_linux = Sys.info()[["sysname"]] == "Linux"
if (full_build) {
  v = paste0("v", Sys.Date(), "_commit_", commit$commit)
  v = gsub(pattern = " |:", replacement = "-", x = v)
  setwd("outputdata")
  f = list.files(path = ".", pattern = "Rds|zip|pmtiles|.json")
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

# Test for central Edinburgh:
edinburgh_central = zonebuilder::zb_zone("Edinburgh", n_circles = 2)
tar_load(simplified_network)
simplified_central = simplified_network[edinburgh_central, ]
mapview::mapview(simplified_central)
