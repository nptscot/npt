# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint
library(tidyverse)
library(tmap)
library(stplanr)
library(sf)
remotes::install_dev("cyclestreets")
date_routing = "2023-01-18"
# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(dl_data, {
    setwd("inputdata")
    gh_release_downlad(tag = "v1")
    setwd("..")
  }),
  tar_target(zones,
    command = {
      
      # For Edinburgh data (test):
      # sf::read_sf("data-raw/zones_edinburgh.geojson")
      # For national data:
      readRDS("inputdata/zones_national_simple.Rds") # 1230 zones
    }),
  tar_target(od_commute_raw, {
    # read_csv("data-raw/od_subset.csv")
    readRDS("inputdata/od_izo.Rds")
  }),
  # tar_target(od_schools_raw, {
  #   # read_csv("data-raw/od_subset.csv")
  #   # read_csv("data-raw/od_subset.csv")
  # }),
  tar_target(od_data, {
    desire_lines_raw = readRDS("inputdata/desire_lines_scotland.Rds")
    od_raw = as_tibble(sf::st_drop_geometry(desire_lines_raw))
    od_subset = od_raw %>%
      filter(geo_code1 %in% zones$InterZone) %>%
      filter(geo_code2 %in% zones$InterZone) %>%
      filter(dist_euclidean < 5000) %>% 
      filter(dist_euclidean > 1000) %>% 
      filter(all >= 50)
    # write_csv(od_subset, "data-raw/od_subset.csv")
  }),
  tar_target(subpoints_origins, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/oas.geojson")
    readRDS("inputdata/oas.Rds")
  }),
  tar_target(subpoints_destinations, {
    # source("data-raw/get_wpz.R")
    # sf::read_sf("data-raw/workplaces_simple_edinburgh.geojson")
    readRDS("inputdata/workplaces_simple.Rds")
  }),
  tar_target(od_commute_jittered, {
    # od_jittered = od_data # for no jittering:
    remotes::install_github("dabreegster/odjitter", subdir = "r")
    set.seed(2023)
    odj = odjitter::jitter(
      od = od_data,
      zones = zones,
      subpoints_origins = subpoints_origins,
      subpoints_destinations = subpoints_destinations,
      disaggregation_threshold = 60
      )
    odj$dist_euclidean_jittered = as.numeric(sf::st_length(odj))
    odj
    # saveRDS(odj, "inputdata/od_commute_jittered.Rds")
    # Read in test OD dataset for package development:
    # sf::read_sf("https://github.com/nptscot/npt/releases/download/v1/od_jittered_demo.geojson")
  }),
  tar_target(od_commute_subset, {
    odcs = od_commute_jittered %>%
      filter(dist_euclidean < 5000) %>% 
      filter(dist_euclidean > 1000)
    odcs
  }),
  tar_target(routes_commute, {
    # For testing:
    # route(l = od_commute_jittered, route_fun = cyclestreets::journey, plan = "balanced")
    message("Calculating ", nrow(od_commute_subset), " routes")
    get_routes(od_commute_subset, plans = "balanced", purpose = "commute",
               folder = "outputdata", batch = FALSE)
  
  }),
  tar_target(uptake_commute, {
    get_scenario_go_dutch(routes_commute$balanced)
  }),
  tar_target(rnet_commute, {
    overline(uptake_commute, attrib = c("bicycle", "bicycle_go_dutch")) %>% 
      dplyr::arrange(bicycle)
    
    rnet_raw = stplanr::overline(
      uptake_commute,
      attrib = c("bicycle", "bicycle_go_dutch", "quietness", "gradient_smooth"), # todo: add other modes
      fun = list(sum = sum, mean = mean)
    )
    rnet = rnet_raw %>%
      transmute(
        bicycle = round(bicycle_sum),
        # `Bicycle (Near Market)` = round(cyclists_near_sum),
        bicycle_go_dutch = round(bicycle_go_dutch_sum),
        # `Bicycle (Ebike)` = round(cyclists_ebike_sum),
        Gradient = round(gradient_smooth_mean * 100),
        Quietness = round(quietness_mean)
        # col = cut(Quietness, quietness_breaks, labels = pal_quietness, right = FALSE)
      )
    
  }),
  tar_target(rnet, {
    rnet_commute
  }),
  tar_target(save_outputs, {
    saveRDS(rnet_commute, "outputdata/rnet_commute.Rds")
    saveRDS(uptake_commute, "outputdata/uptake_commute.Rds")
  }),
  tar_target(plot_zones, {
    # tm_shape(zones) +
    m = tm_shape(zones) +
      tm_fill(col = "TotPop2011", palette = "viridis")
    tmap_save(m, "figures/test-plot.png")
  }),
  # tar_target(visualise_rnet, {
  #   # tar_source("code/vis_network.R")
  #   # tarchetypes::tar_
  # }),
  tarchetypes::tar_render(visualise_rnet, path = "code/vis_network.Rmd", params = list(rnet_commute)),
  
  tar_target(calculate_benefits, {
    benefits = function(x) x
    benefits(routes_commute)
  }),
  tarchetypes::tar_render(report, path = "README.Rmd", params = list(zones, rnet))
  # tar_source(files = "data-raw/test-tiles.R") # how to source script as target?
)
