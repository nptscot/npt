# Set target options:
pkgs = packages = c(
  "tibble","zonebuilder","dplyr","lubridate",
  "stringr","sf","tidyr","data.table", "targets",
  "glue","zip","jsonlite","remotes","gert","collapse","pct",
  "readr", "future", "future.callr", "future.batchtools",
  "bs4Dash", "DT", "gt", "pingr", "shinybusy", "shinyWidgets","geos"
)
remotes::install_cran(pkgs)

if(TRUE){ # Repeated builds can it GitHub API limit, set to TRUE to check for package updates
  remotes::install_dev("cyclestreets")
  remotes::install_github("dabreegster/odjitter", subdir = "r")
  remotes::install_github("ropensci/stplanr")# Improved overline
  remotes::install_github("robinlovelace/ukboundaries")
  remotes::install_github("robinlovelace/simodels")
}
