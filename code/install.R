# Run as part of _targets.R to install packages
# Do you want to reinstall github packages, set to TRUE for first run
update_github_packages = TRUE
options(Ncpus = 4)
source("R/pkgs.R")
pkgs = get_pkgs()
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
