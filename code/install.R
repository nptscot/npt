pkgs  = c(
  "tibble","zonebuilder","dplyr","lubridate",
  "stringr","sf","tidyr","data.table", "targets",
  "glue","zip","jsonlite","remotes","gert","collapse","pct",
  "readr",
  "bs4Dash", "DT", "gt", "pingr", "shinybusy", "shinyWidgets","geos",
  "cyclestreets" ,"stplanr", "simodels",
  "geojsonsf","lwgeom","targets","tidyverse", "crew"
)

#Do you want to reinstall github packages, set to TRUE for first run
update_github_packages = TRUE

remotes::install_cran(pkgs)

# Repeated builds can it GitHub API limit, set to TRUE in _targets.R to check for package updates
if(update_github_packages){ 
  remotes::install_dev("cyclestreets")
  remotes::install_github("dabreegster/odjitter", subdir = "r")
  remotes::install_github("ropensci/stplanr")# Improved overline
  remotes::install_github("robinlovelace/ukboundaries")
  remotes::install_github("robinlovelace/simodels")
  remotes::install_dev("rsgeo")
}
