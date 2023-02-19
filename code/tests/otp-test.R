# Aim: set-up OTP


# Build graph -------------------------------------------------------------

# TBC for latest OSM changes starting with 
# osmextract::


# Set-up OTP --------------------------------------------------------------

remotes::install_github("ropensci/opentripplanner")
library(opentripplanner)
piggyback::pb_releases()
targets::tar_source()
setwd("inputdata/")
gh_release_downlad(file = "graph.obj", tag = "otp")
gh_release_downlad(file = "build-config.json", tag = "otp")
gh_release_downlad(file = "router-config.json", tag = "otp")
dir.create("graphs")
dir.create("graphs/scotland")
# TODO: move files
setwd("..")

otp_setup(
  otp = "/home/rlatemin/R/x86_64-pc-linux-gnu-library/4.2/opentripplanner/jar/otp-2.1.0-shaded.jar", 
  dir = "inputdata", 
  router = "ire",
  memory = 5000,
  port = 8080
)

