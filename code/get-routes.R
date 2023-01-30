# Aim: just get the routes

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

# Build parameters --------------------------------------------------------

plans = c("fastest", "balanced")
min_flow = 1 # Set to 1 for full build, set to high value (e.g. 400) for tests

# Computation done outside of the pipeline --------------------------------

tar_load(od_commute_subset)
fs::file_size("outputdata/routes_max_dist_commute_fastest.Rds")
r_commute = get_routes(od_commute_subset, plans = plans, purpose = "commute",
                       folder = "outputdata", batch = FALSE)