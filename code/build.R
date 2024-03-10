# Aim: combine regional outputs, create and upload outputs

library(tidyverse)
library(targets)
tar_source()

parameters = jsonlite::read_json("parameters.json", simplifyVector = T)
lads = sf::read_sf("inputdata/boundaries/la_regions_2023.geojson")
region_names = unique(lads$Region)
region_names_lowercase = snakecase::to_snake_case(region_names)
region = region_names[1]

for (region in region_names[1:3]) {
  message("Processing region: ", region)
  parameters$region = region
  jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)
  targets::tar_make()
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
# combined_network |>
#   sample_n(1000) |>
#   select(1) |>
#   plot()
sf::write_sf(combined_network, file.path("outputdata", "combined_network.geojson"), delete_dsn = TRUE)

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
tar_load(school_stats)
# Same for school_stats.Rds
school_stats_list = lapply(output_folders, function(folder) {
  school_stats_file = paste0(folder, "/school_stats.Rds")
  if (file.exists(school_stats_file)) {
    school_stats = readRDS(school_stats_file)
  }
})
school_stats_list[[1]]
school_stats_list[[2]]
school_stats = dplyr::bind_rows(school_stats_list)
export_zone_json(zones_stats, "DataZone", path = "outputdata")
export_zone_json(school_stats, "SeedCode", path = "outputdata")

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
make_geojson_zones(b_verylow, file.path(output_folder, "dasymetric_verylow.geojson"))
make_geojson_zones(b_low, file.path(output_folder, "dasymetric_low.geojson"))
make_geojson_zones(b_med, file.path(output_folder, "dasymetric_med.geojson"))
make_geojson_zones(b_high, file.path(output_folder, "dasymetric_high.geojson"))

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
