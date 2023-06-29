library(sf)
library(tmap)
library(stplanr)
library(dplyr)
source("R/rnet_functions.R")


school_ebike <- readRDS("outputdata/school_ebike_sub30k.Rds")
school_fast <- readRDS("outputdata/school_fast_sub30k.Rds")
school_balanced <- readRDS("outputdata/school_balanced_sub30k.Rds")
school_quiet <- readRDS("outputdata/school_quietest_sub30k.Rds")

flow = readRDS("D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_dl_sub30km.Rds")

flow_fast = left_join(st_drop_geometry(flow), school_fast, by = c("route_id"), multiple = "all")
flow_quiet = left_join(st_drop_geometry(flow), school_quiet, by = c("route_id"), multiple = "all")
flow_balanced = left_join(st_drop_geometry(flow), school_balanced, by = c("route_id"), multiple = "all")
flow_ebike = left_join(st_drop_geometry(flow), school_ebike, by = c("route_id"), multiple = "all")


calc_flows = function(r){
  r = r[!is.na(r$length),]
  r = r %>% 
    rename(length_route = length) %>%
    rename(gradient = gradient_smooth) %>% 
    rename(all = count) %>% 
    group_by(route_id) %>% 
    mutate(route_hilliness = mean(gradient)) %>% 
    ungroup()
  
  r = r %>% 
    mutate(bicycle = round(all/50,1)) %>% 
    mutate(bicycle_go_dutch = round(pct::uptake_pct_godutch_2020(length_route,route_hilliness) * all,1)) 
  
  r = st_as_sf(r)
  r
}


flow_fast = calc_flows(flow_fast)
flow_quiet = calc_flows(flow_quiet)
flow_balanced = calc_flows(flow_balanced)
flow_ebike = calc_flows(flow_ebike)



rnet_fast = make_rnets(flow_fast, "school_fastest")
rnet_quiet = make_rnets(flow_quiet, "school_quietest")
rnet_balanced = make_rnets(flow_balanced, "school_balanced")
rnet_ebike = make_rnets(flow_ebike, "school_ebike")

saveRDS(rnet_fast, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_rnet_fast.Rds")
saveRDS(rnet_quiet, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_rnet_quiet.Rds")
saveRDS(rnet_balanced, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_rnet_balanced.Rds")
saveRDS(rnet_ebike, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_rnet_ebike.Rds")

# Combine rnets
rnet_long = list(rnet_fast, rnet_quiet, rnet_balanced, rnet_ebike)

rnet_long = data.table::rbindlist(rnet_long, fill = TRUE)
rnet_long = rnet_long[,c(1:4,6:11,5)]
rnet_long = rnet_long %>% 
  mutate(across(school_fastest_bicycle:school_ebike_bicycle_go_dutch, function(x) tidyr::replace_na(x, 0))) %>% 
  as_tibble()

rnet_long$geometry = sf::st_sfc(rnet_long$geometry, recompute_bbox = TRUE)
rnet_long = sf::st_as_sf(rnet_long)

rnet_combined = overline(rnet_long, 
                         attrib = c("school_fastest_bicycle","school_fastest_bicycle_go_dutch","gradient",                      
                         "quietness","school_quietest_bicycle","school_quietest_bicycle_go_dutch",
                         "school_balanced_bicycle","school_balanced_bicycle_go_dutch","school_ebike_bicycle",
                         "school_ebike_bicycle_go_dutch"), 
                         fun = list(sum = sum, max = max),
                         regionalise = 1e3,
                         ncores = 20)

rnet_combined = rnet_combined[,c("school_fastest_bicycle_sum","school_fastest_bicycle_go_dutch_sum",
                                 "school_quietest_bicycle_sum","school_quietest_bicycle_go_dutch_sum",
                                 "school_balanced_bicycle_sum","school_balanced_bicycle_go_dutch_sum","school_ebike_bicycle_sum",
                                 "school_ebike_bicycle_go_dutch_sum",
                                 "gradient_max","quietness_max","geometry" )]
names(rnet_combined) = gsub("_sum","",names(rnet_combined))
names(rnet_combined) = gsub("_max","",names(rnet_combined))

saveRDS(rnet_combined, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_rnet_sub30km.Rds")

#qtm(rnet_combined[1:100000,], lines.lwd = 3, lines.col = "school_fastest_bicycle_go_dutch")

#flow_fast$count_go_dutch = round(pct::uptake_pct_godutch_2020(flow_fast$length_route,  flow_fast$route_hilliness) * flow_fast$count)


# flow_nodup = flow[!duplicated(flow$geometry),]
# flow_dup = flow[duplicated(flow$geometry),]
# flow_dup$match_id <- flow_nodup$route_id[match(flow_dup$geometry, flow_nodup$geometry)]
# 
# flow_nodup <- st_drop_geometry(flow_nodup)
# flow_dup <- st_drop_geometry(flow_dup)
# 
# flow_nodup$route_id <- as.character(flow_nodup$route_id)
# flow_dup$route_id <- as.character(flow_dup$route_id)
# flow_dup$match_id <- as.character(flow_dup$match_id)
# 
# flow_fast_nodup <- left_join(school_fast, flow_nodup, by = c("id" = "route_id"))
# flow_fast_dup <- left_join(school_fast, flow_dup, by = c("id" = "match_id"), multiple = "all")
# flow_fast_dup <- flow_fast_dup[!is.na(flow_fast_dup$count),]
# flow_fast_dup$id = flow_fast_dup$route_id
# flow_fast_dup$route_id = NULL
# 
# flow_fast <- rbind(flow_fast_dup, flow_fast_nodup)
# 
# flow_fast$length <- as.numeric(flow_fast$length)
# flow_fast$gradient_smooth <- as.numeric(flow_fast$gradient_smooth)
# 
# flow_fast = flow_fast %>% 
#   rename(route_id = id) %>%
#   rename(length_route = length) %>%
#   rename(gradient = gradient_smooth) %>% 
#   group_by(route_id) %>% 
#   mutate(route_hilliness = mean(gradient)) %>% 
#   ungroup()
# 
# flow_fast$count_go_dutch = round(pct::uptake_pct_godutch_2020(flow_fast$length_route,  flow_fast$route_hilliness) * flow_fast$count)
# 
# 
# 
# source("R/uptake.R")
# 
# foo = get_scenario_go_dutch(flow_fast)
