# The aim of this script is to add open datasets to data-raw
library(targets)
library(tidyverse)
library(sf)

add_normally_distributed_noise = function(x, sd = 5, runif_max = 10) {
  x_norm = x + rnorm(length(x), mean = 0, sd = sd * sd(x))
  x_runif = x_norm + runif(length(x), min = -runif_max, max = runif_max)
  # Any values below 0 are set to 0:
  x_runif[x_runif < 0] = 0  
  round(x_runif, 0)
}

# Save study_area target
tar_load(study_area)
mapview::mapview(study_area)
study_area$geometry
sf::write_sf(study_area, "data-raw/study_area.geojson", delete_dsn = TRUE)

study_area = sf::read_sf("data-raw/study_area.geojson")
tar_load(zones)
plot(zones$geometry)
zones = rmapshaper::ms_simplify(zones, keep = 0.04)
sf::write_sf(zones, "data-raw/DataZones.geojson")
fs::file_size("data-raw/DataZones.geojson") # 400 KB

# Open schools data:
path_teams = Sys.getenv("NPT_TEAMS_PATH")
if(nchar(path_teams) == 0){
  stop("Can't find Teams folder of secure data. Use usethis::edit_r_environ() to define NPT_TEAMS_PATH ")
}
if(file.exists(file.path(path_teams,"secure_data/schools/school_dl_sub30km.Rds"))){
  schools_dl = readRDS(file.path(path_teams, "secure_data/schools/school_dl_sub30km.Rds"))
} else {
  stop("Can't find ",file.path(path_teams,"secure_data/schools/school_dl_sub30km.Rds"))
}
if(parameters$geo_subset) {
  schools_dl = schools_dl[study_area, op = sf::st_within]
}
schools_dl = schools_dl |>
  slice_max(order_by = all, n = parameters$max_to_route, with_ties = FALSE) |>
  mutate(across(where(is.numeric), add_normally_distributed_noise)) 
summary(schools_dl)
sf::write_sf(schools_dl, "data-raw/school_desire_lines_open.geojson", delete_dsn = TRUE)

plot(schools_dl$geometry)

tar_load(od_data)
tar_load(od_commute_subset)
waldo::compare(names(od_data), names(od_commute_subset))
od_data = od_data |>
  filter(paste(geo_code1, geo_code2) %in% paste(od_commute_subset$geo_code1, od_commute_subset$geo_code2))
summary(od_data)
# Randomise values:
od_data_open = od_data |>
  # Randomise values, but keep the same distribution:
    mutate(across(where(is.numeric), add_normally_distributed_noise)) 
od_data_open$taxi = round(od_data_open$car / 50)
summary(od_data_open)
# Save:
write_csv(od_data_open, "data-raw/od_data_dz_synthetic.csv")

# Commute data:
desire_lines_raw = read_TEAMS("secure_data/commute/commute_dl_sub30km.Rds")
od_raw = as_tibble(sf::st_drop_geometry(desire_lines_raw))
summary(od_raw)
tar_load(od_data)
waldo::compare(names(od_data), names(od_raw))
