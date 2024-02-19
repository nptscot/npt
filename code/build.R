# Aim: run targets script recursively

parameters = jsonlite::read_json("parameters.json", simplifyVector = T)

lads = sf::read_sf("inputdata/boundaries/la_regions_2023.geojson")
region_names = unique(lads$Region)
region_names_lowercase = snakecase::to_snake_case(region_names)

region = region_names[1]

for (region in region_names) {
  parameters$region = region
  jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)
  targets::tar_make()
}

# Combine the outputs:

output_folders = list.dirs(file.path("outputdata", parameters$date_routing))

combined_network_list = lapply(output_folders, function(folder) {
  combined_network_file = paste0(folder, "/combined_network_tile.geojson")
  if (file.exists(combined_network_file)) {
    network = sf::read_sf(combined_network_file)
  }
})

# combined_network = stplanr:::bind_sf(combined_network_list)

# With do.call and rbind:
combined_network = do.call(rbind, combined_network_list)
plot(combined_network$geometry)

sf::write_sf(combined_network, file.path("outputdata", parameters$date_routing, "combined_network.geojson"), delete_dsn = TRUE)
