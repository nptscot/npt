# Aim: combine regional outputs, create and upload outputs

library(tidyverse)
library(targets)
library(tidygraph)
library(osmextract)
devtools::load_all()
tar_source()

parameters = jsonlite::read_json("parameters.json", simplifyVector = T)
lads = sf::read_sf("inputdata/boundaries/la_regions_2023.geojson")
region_names = unique(lads$Region)[c(3, 2, 1, 4, 5, 6)] # Start with Glasgow
cities_region_names = list()

for (region in region_names) {
# Assuming 'CityName' is the column that holds the names of cities or localities
  cities_in_region = lads |> 
    filter(Region == region) |> 
    pull(LAD23NM) |> 
    unique() # Ensure unique city names

# Add the city names to the list under the region name
  cities_region_names[[region]] = cities_in_region
}

region_names_lowercase = snakecase::to_snake_case(region_names)
region = region_names[4]
region_names_lowercase

# Initialize a vector to hold the names of regions that fail in the first attempt
failed_regions = character()

# First loop: Attempt to process each region and capture any failures
for (region in region_names[3:4]) {
  tryCatch({
    message("Processing region: ", region)
    parameters$region = region
    parameters$coherent_area = cities_region_names[[region]]
    jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)
    targets::tar_make()
  }, error = function(e) {
    message(paste("Error encountered in region", region, ". Error details: ", e$message))
    # Add the failed region to the vector for later retry
    print(paste("Adding", region, "to failed_regions"))
    failed_regions <<- c(failed_regions, region)
  })
}

# Second loop: Attempt to reprocess any regions that failed in the first attempt
if (length(failed_regions) > 0) {
  message("Attempting to reprocess failed regions...")
  for (region in failed_regions) {
    tryCatch({
      message("Reprocessing region: ", region)
      parameters$region = region
      parameters$coherent_area = cities_region_names[[region]]
      jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)
      targets::tar_make()
    }, error = function(e) {
      message(paste("Failed again in region", region, ". Error details: ", e$message))
      # Here you could log the failure or take additional recovery actions
    })
  }
} else {
  message("All regions processed successfully on the first attempt.")
}

# Zip the contents of the outputdata folder
output_folder = file.path("outputdata", parameters$date_routing)
list.files(output_folder)
setwd("outputdata")
zip_file = paste0(parameters$date_routing, "2.zip")
zip(zipfile = zip_file, parameters$date_routing, extras = "-x *.Rds")
dir.create("2024-03-14-geojson")
for(i in list.dirs("2024-03-14")) {
  r = gsub(pattern = "2024-03-14/", replacement = "", x = i)
  f = list.files(i, pattern = "geojson", full.names = TRUE)
  f_new = file.path("2024-03-14-geojson", paste0(r, "_", basename(f)))
  message(paste(f, collapse = "\n"))
  message(paste(f_new, collapse = "\n"))
  file.copy(f, f_new)
}

zip("2024-03-14-geojson.zip", files = "2024-03-14-geojson")
fs::file_size("2024-03-14-geojson.zip")
system("gh release list")
system("gh release create 2024-03-14-geojson")
system("gh release upload 2024-03-14-geojson 2024-03-14-geojson.zip")

# Generate coherent network
for (region in region_names[1:6]) {
    message("Processing coherent network for region: ", region)
    parameters$region = region
    parameters$coherent_area = cities_region_names[[region]]
    parameters$date_routing = "2024-05-11"
    # jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)

    combined_network_tile_path = paste0("outputdata/", parameters$date_routing,"/",  snakecase::to_snake_case(parameters$region), "/", "combined_network_tile.geojson")
    NPT = sf::read_sf(combined_network_tile_path) |>
      sf::st_transform(crs = "EPSG:27700")

    folder_path = paste0("outputdata/", parameters$date_routing,"/",  snakecase::to_snake_case(parameters$region), "/", "coherent_networks/")

    if(!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }

    for (city in parameters$coherent_area) { 
      city_filename = snakecase::to_snake_case(city)
      tryCatch({
        message("Generating coherent network for: ", city)
        zone = filter(lads, LAD23NM == city) |>
          sf::st_transform(crs = "EPSG:27700")

        NPT_zone = NPT[sf::st_union(zone), , op = sf::st_intersects]

        min_percentile_value = stats::quantile(NPT_zone$all_fastest_bicycle_go_dutch, probs = 0.938, na.rm = TRUE)

        open_roads_national_zone = open_roads_national[sf::st_union(zone), , op = sf::st_intersects]

        OS_NPT_zone = corenet::cohesive_network_prep(base_network = open_roads_national_zone, 
                                            influence_network = NPT_zone, 
                                            zone, 
                                            crs = "EPSG:27700", 
                                            key_attribute = "road_function", 
                                            attribute_values = c("A Road", "B Road", "Minor Road"))

        cohesive_network_zone = corenet::corenet(NPT_zone, OS_NPT_zone, zone, 
                                          key_attribute = "all_fastest_bicycle_go_dutch", 
                                          crs = "EPSG:27700", dist = 10, threshold = min_percentile_value, 
                                          road_scores = list("A Road" = 1, "B Road" = 1, "Minor Road" = 10000000))

        grouped_network = corenet::coherent_network_group(cohesive_network_zone, key_attribute = "all_fastest_bicycle_go_dutch")

        # Use city name in the filename
        corenet::create_coherent_network_PMtiles(folder_path = folder_path, city_filename = city_filename, cohesive_network = grouped_network)

        message("Coherent network for: ", city, " generated successfully")

      }, error = function(e) {
        message(sprintf("An error occurred with %s: %s", city, e$message))
      })  
    }
}   

output_folders = list.dirs(file.path("outputdata", parameters$date_routing))[-1]
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
zones_tile_list = lapply(output_folders, function(folder) {
  zones_tile_file = paste0(folder, "/data_zones.geojson")
  if (file.exists(zones_tile_file)) {
    zones_tile = sf::read_sf(zones_tile_file)
  }
})
# # Plot 1st:
# zones_tile_list[[1]] |>
#   sample_n(1000) |>
#   select(1) |>
#   plot()
zones_tile = dplyr::bind_rows(zones_tile_list)
sf::write_sf(zones_tile, file.path("outputdata", "zones_tile.geojson"), delete_dsn = TRUE)

# Same for school_locations.geojson
school_locations_list = lapply(output_folders, function(folder) {
  school_locations_file = paste0(folder, "/school_locations.geojson")
  if (file.exists(school_locations_file)) {
    school_locations = sf::read_sf(school_locations_file)
  }
})
# # Plot 1st:
school_locations_list[[1]] |>
  sample_n(1000) |>
  select(1) |>
  plot()

# Export SchoolStats:
school_stats_list = lapply(output_folders, function(folder) {
  school_stats_file = paste0(folder, "/school_stats.Rds")
  if (file.exists(school_stats_file)) {
    school_stats = readRDS(school_stats_file)
  }
})
school_stats = dplyr::bind_rows(school_stats_list)
export_zone_json(school_stats, "SeedCode", path = "outputdata")

# Same for zones_stats.Rds
zones_stats_list = lapply(output_folders, function(folder) {
  zones_stats_file = paste0(folder, "/zones_stats.Rds")
  if (file.exists(zones_stats_file)) {
    zones_stats = readRDS(zones_stats_file)
  }
})
zones_stats = dplyr::bind_rows(zones_stats_list)
export_zone_json(zones_stats, "DataZone", path = "outputdata")

#Combined network tiling
setwd("outputdata")
# Check the combined_network_tile.geojson file is there:
file.exists("combined_network_tile.geojson")
command_tippecanoe = paste('tippecanoe -o rnet.pmtiles',
                           '--name=rnet',
                           '--layer=rnet',
                           '--attribution=UniverstyofLeeds',
                           '--minimum-zoom=6',
                           '--maximum-zoom=13',
                           '--drop-smallest-as-needed',
                           '--maximum-tile-bytes=5000000',
                           '--simplification=10',
                           '--buffer=5',
                           '--force  combined_network_tile.geojson', collapse = " ")

responce = system(command_all, intern = TRUE)

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

tippecanoe_verylow = paste('tippecanoe -o dasymetric_verylow.pmtiles',
                           '--name=dasymetric',
                           '--layer=dasymetric',
                           '--attribution=OS',
                           '--minimum-zoom=4',
                           '--maximum-zoom=6',
                           '--coalesce-smallest-as-needed',
                           '--detect-shared-borders',
                           '--maximum-tile-bytes=5000000',
                           '--simplification=1',
                           '--buffer=5',
                           '--force dasymetric_verylow.geojson', 
                           collapse = " ")

tippecanoe_low = paste('tippecanoe -o dasymetric_low.pmtiles',
                       '--name=dasymetric',
                       '--layer=dasymetric',
                       '--attribution=OS',
                       '--minimum-zoom=7',
                       '--maximum-zoom=9',
                       '--coalesce-smallest-as-needed',
                       '--detect-shared-borders',
                       '--maximum-tile-bytes=5000000',
                       '--simplification=1',
                       '--buffer=5',
                       '--force dasymetric_low.geojson', 
                       collapse = " ")

tippecanoe_med = paste('tippecanoe -o dasymetric_med.pmtiles',
                       '--name=dasymetric',
                       '--layer=dasymetric',
                       '--attribution=OS',
                       '--minimum-zoom=10',
                       '--maximum-zoom=14',
                       '--coalesce-smallest-as-needed',
                       '--detect-shared-borders',
                       '--maximum-tile-bytes=5000000',
                       '--simplification=2',
                       '--buffer=5',
                       '--force dasymetric_med.geojson', 
                       collapse = " ")

tippecanoe_high = paste('tippecanoe -o dasymetric_high.pmtiles',
                        '--name=dasymetric',
                        '--layer=dasymetric',
                        '--attribution=OS',
                        '-zg',
                        '--minimum-zoom=15',
                        '--extend-zooms-if-still-dropping',
                        '--coalesce-smallest-as-needed',
                        '--detect-shared-borders',
                        '--maximum-tile-bytes=5000000',
                        '--simplification=5',
                        '--buffer=5',
                        '--force dasymetric_high.geojson', 
                        collapse = " ")

tippecanoe_join = paste('tile-join -o dasymetric.pmtiles -pk --force',
                        'dasymetric_verylow.pmtiles',
                        'dasymetric_low.pmtiles',
                        'dasymetric_med.pmtiles',
                        'dasymetric_high.pmtiles', 
                        collapse = " ")



if(.Platform$OS.type == "unix") {
  command_cd = 'cd outputdata'
  command_all = paste(c(command_cd, tippecanoe_verylow, tippecanoe_low, 
                        tippecanoe_med, tippecanoe_high, tippecanoe_join), collapse = "; ")
} else {
  # Using WSL
  dir = getwd()
  command_start = 'bash -c '
  command_cd = paste0('cd /mnt/',tolower(substr(dir,1,1)),substr(dir,3,nchar(dir)),'/outputs')
  command_all = paste(c(command_cd, tippecanoe_verylow, tippecanoe_low, 
                        tippecanoe_med, tippecanoe_high, tippecanoe_join), collapse = "; ")
  command_all = paste0(command_start,'"',command_all,'"')
}
responce = system(command_all, intern = TRUE)

# Copy pmtiles into app folder
app_tiles_directory = "../nptscot.github.io/tiles"
list.files(app_tiles_directory) # list current files
pmtiles = list.files("outputdata", pattern = "pmtiles", full.names = TRUE)
pmtiles_new = file.path(app_tiles_directory, basename(pmtiles))
file.copy(pmtiles, pmtiles_new,  overwrite = TRUE)

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
