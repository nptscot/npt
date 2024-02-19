# Aim: run targets script recursively

parameters = jsonlite::read_json("parameters.json", simplifyVector = T)

lads = sf::read_sf("inputdata/boundaries/la_regions_2023.geojson")
region_names = unique(lads$Region)

for (region in region_names) {
  parameters$region = region
  jsonlite::write_json(parameters, "parameters.json")
  targets::tar_make()
}
```
  