plans = c("fastest", "balanced", "quietest", "ebike")

file.rename("outputdata/v2023-02-16-23-13-07_commit_abbeae8d4349655c6a87dc537fd5a4530b87f57d/rnet_commute.Rds", ".")
routes_commute = readRDS("outputdata/v2023-02-14-14-37-21_commit_e3bc9392c6ed72fee9250c3f0161300b835fff1e/routes_max_dist_commute_balanced.Rds")
# routes = get_routes(od_commute_subset,
#            plans = parameters$plans, purpose = "commute",
#            folder = "outputdata", batch = FALSE, nrow_batch = 10000)
class_routes = class(routes)
if(any("sf" %in% class(routes))) {
  routes = list(fastest = routes)
}
plans = parameters$plans
message("Getting these plans: ", paste0(plans, collapse = ", "))
uptake_list = sapply(plans, function(x) NULL)
for(p in parameters$plans) {
  uptake_list[[p]] = get_uptake_scenarios(routes[[p]])
}
uptake_list
}),
tar_target(rnet_commute, {
  rnet_commute_list = sapply(parameters$plans, function(x) NULL)
  for(p in parameters$plans) {
    message("Building ", p, " network")
    rnet_raw = stplanr::overline(
      r_commute[[p]],
      attrib = c("bicycle", "bicycle_go_dutch", "quietness", "gradient_smooth"), # todo: add other modes
      fun = list(sum = sum, mean = mean)
    )
    rnet = rnet_raw %>%
      transmute(
        bicycle = round(bicycle_sum),
        # `Bicycle (Near Market)` = round(cyclists_near_sum),
        bicycle_go_dutch = round(bicycle_go_dutch_sum),
        # `Bicycle (Ebike)` = round(cyclists_ebike_sum),
        gradient = round(gradient_smooth_mean * 100),
        quietness = round(quietness_mean)
        # col = cut(quietness, quietness_breaks, labels = pal_quietness, right = FALSE)
      ) %>% 
      dplyr::arrange(bicycle)
    rnet_commute_list[[p]] = rnet
  }
  rnet_commute_list