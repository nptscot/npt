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
