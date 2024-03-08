# Aim: combine regional outputs, create and upload outputs

parameters = jsonlite::read_json("parameters.json", simplifyVector = T)
lads = sf::read_sf("inputdata/boundaries/la_regions_2023.geojson")
region_names = unique(lads$Region)
region_names_lowercase = snakecase::to_snake_case(region_names)
region = region_names[1]

for (region in region_names[1:2]) {
  message("Processing region: ", region)
  parameters$region = region
  jsonlite::write_json(parameters, "parameters.json", pretty = TRUE)
  targets::tar_make()
}

output_folders = list.dirs(file.path("outputdata", parameters$date_routing))
combined_network_list = lapply(output_folders, function(folder) {
  combined_network_file = paste0(folder, "/combined_network_tile.geojson")
  if (file.exists(combined_network_file)) {
    network = sf::read_sf(combined_network_file)
  }
})

# TODO: try with stplanr:::bind_sf(combined_network_list)
# With do.call and rbind:
combined_network = do.call(rbind, combined_network_list)
plot(combined_network$geometry)

sf::write_sf(combined_network, file.path("outputdata", "combined_network.geojson"), delete_dsn = TRUE)

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
