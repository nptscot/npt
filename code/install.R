pkgs = c(
  "crew", # For targets with workers
  "cyclestreets", # For routing
  "geojsonsf", # For converting geojson to sf
  "geos", # For geometric operations
  "gert", # For interactive with git
  "glue", # For string interpolation
  "lubridate", # For working with dates and times
  "lwgeom", # For working with spatial data
  "osmextract", # For extracting OpenStreetMap data
  "pct", # PCT interface
  "remotes", # For installing packages from remote sources
  "sf", # For working with spatial data
  "simodels", # For spatial interaction models
  "snakecase", # For converting strings to snake case
  "stplanr", # For sustainable transport planning
  "targets", # For managing targets in a workflow
  "tidyverse", # Includes dplyr, ggplot2, tidyr, stringr etc.
  "zonebuilder" # For creating zones for spatial analysis
)

# Do you want to reinstall github packages, set to TRUE for first run
update_github_packages = TRUE
options(Ncpus = 4)
remotes::install_cran(pkgs)

# Repeated builds can it GitHub API limit, set to TRUE in _targets.R to check for package updates
if (update_github_packages) {
  remotes::install_dev("cyclestreets")
  remotes::install_github("dabreegster/odjitter", subdir = "r")
  remotes::install_github("robinlovelace/ukboundaries")
  remotes::install_github("robinlovelace/simodels")
  remotes::install_dev("rsgeo")
  remotes::install_dev("od")
  remotes::install_dev("osmextract")
  remotes::install_github("nptscot/corenet")
}
