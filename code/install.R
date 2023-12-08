# Set target options:
pkgs  = c(
  "tibble","zonebuilder","dplyr","lubridate",
  "stringr","sf","tidyr","data.table", "targets",
  "glue","zip","jsonlite","remotes","gert","collapse","pct",
  "readr", "future", "future.callr", "future.batchtools",
  "bs4Dash", "DT", "gt", "pingr", "shinybusy", "shinyWidgets","geos",
  "odjitter", "cyclestreets" ,"stplanr", "ukboundaries", "simodels",
  "geojsonsf"
  #,"rsgeo"
)

# Repeated builds can it GitHub API limit, set to TRUE in _targets.R to check for package updates
if(update_github_packages){ 
  remotes::install_dev("cyclestreets")
  remotes::install_github("dabreegster/odjitter", subdir = "r")
  remotes::install_github("ropensci/stplanr")# Improved overline
  remotes::install_github("robinlovelace/ukboundaries")
  remotes::install_github("robinlovelace/simodels")
  remotes::install_dev("rsgeo")
}
