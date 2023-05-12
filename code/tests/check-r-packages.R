# Check which packages were installed from binary versions:
library(tidyverse)
pkgs = installed.packages()[, 1:2]
nrow(pkgs) # 441
head(pkgs)
# Find packages that have LibPath different from /usr/lib/R/library:
pkgs_installed_home = pkgs |>
  as_tibble() |>
  filter(str_detect(LibPath, "/home/")) 
pkgs_installed_home
nrow(pkgs_installed_home) # 9
pkgs_not_installed_home = pkgs |>
  as_tibble() |>
  filter(!str_detect(LibPath, "/home/"))
# Reinstall all packages not installed in home directory, using 8 threads:
pkgs_not_installed_home |>
  as_tibble() |>
  pmap(~install.packages(..1, lib = ..2, Ncpus = 8, ask = FALSE))
install.packages()

# Check how many packages need to be updated:
# Source: https://stackoverflow.com/questions/2563511/in-r-how-can-i-know-if-my-packages-are-up-to-date
i <- installed.packages()
a <- available.packages()
ia <- merge(i, a, by="Package")[,c("Package", "Version.x", "Version.y")]
out_of_date_pkg = ia[as.character(ia$Version.x) != as.character(ia$Version.y),]
nrow(out_of_date_pkg) #43
