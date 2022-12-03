# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint
library(tidyverse)
library(tmap)

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
  tar_target(plot_zones, {
    # tm_shape(zones) +
    m = tm_shape(zones) +
      tm_fill(col = "TotPop2011", palette = "viridis")
    tmap_save(m, "figures/test-plot.png")
  }),
  tarchetypes::tar_render(report, path = "README.Rmd", params = list(zones))
)
