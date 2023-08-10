library(targets)
library(tidyverse)
library(sf)

tar_load(parameters)
tar_load(uptake_list_school)

rnet_primary_list = sapply(parameters$plans, function(x) NULL)
for(p in parameters$plans) {
  message("Building Primary ", p, " network")
  rp = uptake_list_school[[p]]
  rp = rp[rp$schooltype == "Primary",]
  rnet = make_rnets(rp, ncores = 1)
  
  f = paste0("outputdata/rnet_primary_school_", p, ".Rds")
  saveRDS(rnet, f)
  rnet_primary_list[[p]] = rnet
}
# Building Primary fastest network
# Error in if (as.character(unique(sf::st_geometry_type(sl))) == "MULTILINESTRING") { : 
#     argument is of length zero


#Secondary
rnet_secondary_list = sapply(parameters$plans, function(x) NULL)
for(p in parameters$plans) {
  message("Building Secondary ", p, " network")
  rs = uptake_list_school[[p]]
  rs = rs[rs$schooltype == "Secondary",]
  rnet = make_rnets(rp, ncores = 1)
  
  f = paste0("outputdata/rnet_secondary_school_", p, ".Rds")
  saveRDS(rnet, f)
  rnet_secondary_list[[p]] = rnet
}

rnet_school_list =list(rnet_primary_list, rnet_secondary_list)
names(rnet_school_list) = c("Primary","Secondary")

saveRDS(rnet_school_list, "outputdata/rnet_school_list.Rds")
rnet_school_list
