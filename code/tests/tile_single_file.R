# Aim: create tiles for single release

library(tidyverse)
targets::tar_source()

v = "v2023-03-28-15-53-50_commit_e59db4597b54b75d29407a61a326436fa7402c60"
directory = paste0("outputdata/", v)
directory_previous = setwd(directory)
system("gh release download --help")
system("gh release download v2023-03-28-15-53-50_commit_e59db4597b54b75d29407a61a326436fa7402c60 -p *over* --clobber")
setwd(directory_previous)
rnet_combined = readRDS(paste0(directory, "/rnet_combined_after_overline.Rds"))

rnet_combined = rnet_combined |> 
  rowwise() |> 
  mutate(gradient = max(fastest_gradient, balanced_gradient, quietest_gradient, ebike_gradient)) |> 
  mutate(quietness = max(fastest_quietness, balanced_quietness, quietest_quietness, ebike_quietness)) 

rnet = rnet_combined |> 
  select(-matches("_Q|_Gr")) |> 
  mutate(across(matches("bicycle", round))) |> 
  mutate(gradient = round(gradient, digits = 1))
# # TODO: check gradients
# table(rnet_combined$gradient)

rnet = rnet |>
  rowwise() |>
  mutate(total_cyclists = sum(fastest_bicycle:ebike_bicycle_go_dutch))
summary(rnet$total_cyclists)
names(rnet)[1:8] = paste0("commute_", names(rnet))[1:8]
rnet = rnet |> 
  filter(total_cyclists > 0) |> 
  select(-total_cyclists) |> 
  as.data.frame() |> 
  sf::st_as_sf()
saveRDS(rnet, "outputdata/combined_network.Rds")

combined_network = readRDS("outputdata/combined_network.Rds")
make_geojson_zones(combined_network, "outputdata/combined_network.geojson")
zip(zipfile = "outputdata/combined_network.zip", "outputdata/combined_network.geojson")
file.rename("outputdata/combined_network.geojson", "rnet.geojson")

system("bash code/tile.sh")

setwd("outputdata")
f = list.files(path = ".", pattern = "pmtiles")
# Piggyback fails with error message so commented and using cust
# piggyback::pb_upload(f)
msg = glue::glue("gh release create {v} --generate-notes")
# message("Creating new release and folder to save the files: ", v)
# dir.create(v)
message("Going to try to upload the following files: ", paste0(f, collapse = ", "))
message("With sizes: ", paste0(fs::file_size(f), collapse = ", "))
system(msg)
for(i in f) {
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
