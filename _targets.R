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
  #   gh_release_downlad("desire_lines_scotland.Rds")
  #   setwd("..")
  # }),
  tar_target(zones_national,
    command = {
      readRDS("inputdata/iz_zones11_ed.Rds")
    }),
  tar_target(plot_zones, {
    # tm_shape(zones_national) +
    m = tm_shape(zones_national) +
      tm_fill(col = "TotPop2011", palette = "viridis")
    tmap_save(m, "figures/test-plot.png")
  }),
  
  tar_target(report, rmarkdown::render("README.Rmd"))
    
#   format = "feather" # efficient storage of large data frames # nolint
  # tar_target(
  #   name = model,
  #   command = coefficients(lm(y ~ x, data = data))
  # )
)
