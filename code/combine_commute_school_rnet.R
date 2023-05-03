rnet_school = readRDS("D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_rnet_sub30km.Rds")
rnet_school$Gradient <- round(rnet_school$gradient * 100)
rnet_school$Quietness <- rnet_school$quietness
rnet_school$quietness <- NULL
rnet_school$gradient <- NULL

rnet_commute = readRDS("../outputdata/combined_network.Rds")

rnet_long = list(rnet_school, rnet_commute)

rnet_long = data.table::rbindlist(rnet_long, fill = TRUE)
rnet_long = rnet_long[,c(1:8,10:19,9)]
rnet_long = rnet_long %>% 
  mutate(across(school_fastest_bicycle:commute_ebike_bicycle_go_dutch, function(x) tidyr::replace_na(x, 0))) %>% 
  as_tibble()

rnet_long$geometry = sf::st_sfc(rnet_long$geometry, recompute_bbox = TRUE)
rnet_long = sf::st_as_sf(rnet_long)

rnet_combined = overline(rnet_long, 
                         attrib = c("school_fastest_bicycle","school_fastest_bicycle_go_dutch","school_quietest_bicycle",
                                    "school_quietest_bicycle_go_dutch","school_balanced_bicycle","school_balanced_bicycle_go_dutch",
                                    "school_ebike_bicycle","school_ebike_bicycle_go_dutch",                 
                                    "Gradient","Quietness","commute_fastest_bicycle",
                                    "commute_fastest_bicycle_go_dutch","commute_balanced_bicycle","commute_balanced_bicycle_go_dutch",
                                    "commute_quietest_bicycle","commute_quietest_bicycle_go_dutch","commute_ebike_bicycle",
                                    "commute_ebike_bicycle_go_dutch"), 
                         fun = list(sum = sum, max = max),
                         regionalise = 1e3,
                         ncores = 20)

rnet_combined = rnet_combined[,c("school_fastest_bicycle_sum","school_fastest_bicycle_go_dutch_sum","school_quietest_bicycle_sum",
                                 "school_quietest_bicycle_go_dutch_sum","school_balanced_bicycle_sum","school_balanced_bicycle_go_dutch_sum",
                                 "school_ebike_bicycle_sum","school_ebike_bicycle_go_dutch_sum","Gradient_sum",
                                 "Quietness_sum","commute_fastest_bicycle_sum","commute_fastest_bicycle_go_dutch_sum",
                                 "commute_balanced_bicycle_sum","commute_balanced_bicycle_go_dutch_sum","commute_quietest_bicycle_sum",
                                 "commute_quietest_bicycle_go_dutch_sum","commute_ebike_bicycle_sum","commute_ebike_bicycle_go_dutch_sum",
                               "Gradient_max","Quietness_max","geometry")]
names(rnet_combined) = gsub("_sum","",names(rnet_combined))
names(rnet_combined) = gsub("_max","",names(rnet_combined))

rnet_combined$all_fastest_bicycle = rnet_combined$school_fastest_bicycle + rnet_combined$commute_fastest_bicycle
rnet_combined$all_fastest_bicycle_go_dutch = rnet_combined$school_fastest_bicycle_go_dutch + rnet_combined$commute_fastest_bicycle_go_dutch
rnet_combined$all_quietest_bicycle = rnet_combined$school_quietest_bicycle + rnet_combined$commute_quietest_bicycle
rnet_combined$all_quietest_bicycle_go_dutch = rnet_combined$school_quietest_bicycle_go_dutch + rnet_combined$commute_quietest_bicycle_go_dutch
rnet_combined$all_balanced_bicycle = rnet_combined$school_balanced_bicycle + rnet_combined$commute_balanced_bicycle
rnet_combined$all_balanced_bicycle_go_dutch = rnet_combined$school_balanced_bicycle_go_dutch + rnet_combined$commute_balanced_bicycle_go_dutch
rnet_combined$all_ebike_bicycle = rnet_combined$school_ebike_bicycle + rnet_combined$school_ebike_bicycle
rnet_combined$all_ebike_bicycle_go_dutch  = rnet_combined$school_ebike_bicycle_go_dutch + rnet_combined$commute_ebike_bicycle_go_dutch



saveRDS(rnet_combined, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_commute_rnet.Rds")

st_precision(rnet_combined) <- 1000000
write_sf(rnet_combined, "outputs/rnet_school_commute2.geojson", delete_dsn = TRUE)


