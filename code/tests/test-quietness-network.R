# Aim: debug issue 125

remotes::install_cran("zonebuilder")

dundee_buffer_3km = zonebuilder::zb_zone(x = "Dundee", n_circles = 2)
mapview::mapview(dundee_buffer_3km)

# Load per plan network

library(targets)
tar_load(rnet_commute_list)
rnet_quietest_dundee = rnet_commute_list$quietest[dundee_buffer_3km, ]
mapview::mapview(rnet_quietest_dundee, zcol = "bicycle_go_dutch")
