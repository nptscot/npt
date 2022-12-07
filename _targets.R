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
  # tar_target(dl_data, {
  #   setwd("inputdata")
  #   gh_release_downlad()
  #   setwd("..")
  # }),
  tar_target(zones,
    command = {
      
      # For Edinburgh data (test):
      sf::read_sf("data-raw/zones_edinburgh.geojson")
      # For national data:
      # u = "https://github.com/ITSLeeds/cyclingPotentialEdinburgh/releases/download/1/zones_iz.Rds"
      # f = basename(u)
      # readRDS(f) # 1230 zones
    }),
  tar_target(od_data, {
    # desire_lines_raw = readRDS("inputdata/desire_lines_scotland.Rds")
    # od_raw = as_tibble(sf::st_drop_geometry(desire_lines_raw))
    # od_subset = od_raw %>% 
    #   filter(geo_code1 %in% zones$InterZone) %>% 
    #   filter(geo_code2 %in% zones$InterZone) %>% 
    #   filter(all >= 10)
    # write_csv(od_subset, "data-raw/od_subset.csv")
    read_csv("data-raw/od_subset.csv")
  }),
  tar_target(subpoints_origins, {
    # source("data-raw/get_wpz.R")
    sf::read_sf("data-raw/oas.geojson")
  }),
  tar_target(subpoints_destinations, {
    # source("data-raw/get_wpz.R")
    sf::read_sf("data-raw/workplaces_simple_edinburgh.geojson")
  }),
  tar_target(od_jittered, {
    # od_jittered = od_data # for no jittering:
    # remotes::install_github("dabreegster/odjitter", subdir = "r")
    # odjitter::jitter(
    #   od = od_data,
    #   zones = zones,
    #   subpoints_origins = subpoints_origins,
    #   subpoints_destinations = subpoints_destinations,
    #   disaggregation_threshold = 20
    #   )
    # Read in test OD dataset for package development:
    od_jittered = sf::read_sf("https://github.com/atumscot/atumscot/releases/download/v1/od_jittered_demo.geojson")
  }),
  tar_target(routes, {
    # For testing:
    # route(l = od_jittered[1:5, ], route_fun = cyclestreets::journey, plan = "balanced")
    # route(l = od_jittered, route_fun = cyclestreets::journey, plan = "balanced")
    od_jittered
    readRDS(url("https://github.com/atumscot/atumscot/releases/download/v1/routes_edinburgh_simple.Rds"))
  }),
  tar_target(uptake, {
    
  }),
  tar_target(rnet, {
    overline(routes, attrib = "bicycle") %>% 
      dplyr::arrange(bicycle)
  }),
  tar_target(save_geojson, {
    sf::write_sf(rnet, "overline.geojson", delete_dsn = TRUE)
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
  tarchetypes::tar_render(visualise_rnet, path = "code/vis_network.Rmd", params = list(rnet)),
  
  tar_target(calculate_benefits, {
    benefits = function(x) x
    benefits(routes)
  }),
  tarchetypes::tar_render(report, path = "README.Rmd", params = list(zones, rnet))
  # tar_source(files = "data-raw/test-tiles.R") # how to source script as target?
)
