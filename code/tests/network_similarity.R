setwd("~/nptscot/npt")
library(targets)
library(tidyverse)
library(sf)

tar_load(combined_network)
names(combined_network)
# [1] "commute_fastest_bicycle"           "commute_fastest_bicycle_go_dutch"  "commute_balanced_bicycle"          "commute_balanced_bicycle_go_dutch"
# [5] "commute_quietest_bicycle"          "commute_quietest_bicycle_go_dutch" "commute_ebike_bicycle"             "commute_ebike_bicycle_go_dutch"   
# [9] "Gradient"                          "Quietness"                         "geometry" 
combined_network %>% select(commute_fastest_bicycle) %>%  plot()
combined_network %>% select(commute_quietest_bicycle) %>%  plot()

# Stepping back to a previous target:
tar_load(rnet_commute_list)
names(rnet_commute_list)
plot(rnet_commute_list$fastest)
plot(rnet_commute_list$quietest)
# Each is identical!

# Test the routes:
tar_load(r_commute)
names(r_commute)
plot(r_commute$fastest)
plot(r_commute$quietest)
# Finding from previous plots: routes are different

tar_load(parameters)
parameters$plans

# Update list?
tar_load(uptake_list)
plot(uptake_list$fastest)
plot(uptake_list$quietest)
# That's the faulty target: two previous plots are the same
