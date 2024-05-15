pkgs = c(
  "bs4Dash",
  "collapse",
  "crew",
  "cyclestreets",
  "data.table",
  "dplyr",
  "geojsonsf",
  "geos",
  "gert",
  "glue",
  "jsonlite",
  "lubridate",
  "lwgeom",
  "osmextract",
  "pct",
  "readr",
  "remotes",
  "sf",
  "shinyWidgets",
  "simodels",
  "snakecase",
  "stplanr",
  "stringr",
  "targets",
  "tidyverse",      # Includes dplyr, ggplot2, tidyr, stringr etc.
  "zonebuilder"
)

#Do you want to reinstall github packages, set to TRUE for first run
update_github_packages = TRUE
options(Ncpus = 4)
remotes::install_cran(pkgs)

# Repeated builds can it GitHub API limit, set to TRUE in _targets.R to check for package updates
if(update_github_packages){ 
  remotes::install_dev("cyclestreets")
  remotes::install_github("dabreegster/odjitter", subdir = "r")
  remotes::install_github("robinlovelace/ukboundaries")
  remotes::install_github("robinlovelace/simodels")
  remotes::install_dev("rsgeo")
  remotes::install_dev("od")
  remotes::install_dev("osmextract")
  remotes::install_github("nptscot/corenet")
}
