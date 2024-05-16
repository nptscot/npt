# Aim: get ebike and quietest route networks

targets::tar_source()

routes = list(
  quietest = readRDS("/tmp/routes_max_dist_commute_quietest.Rds"),
  ebike = readRDS("/tmp/routes_max_dist_commute_ebike.Rds")
)
plans = c("quietest", "ebike")

uptake_list = sapply(plans, function(x) NULL)
for(p in plans) {
  uptake_list[[p]] = get_uptake_scenarios(routes[[p]])
}
uptake_list

rnet_raw = stplanr::overline(
  uptake_list[["quietest"]],
  attrib = c("bicycle", "bicycle_go_dutch", "quietness", "gradient_smooth"), # todo: add other modes
  fun = list(sum = sum, mean = mean)
)

rnet_commute_list = sapply(plans, function(x) NULL)
for(p in plans) {
  message("Building ", p, " network")
  rnet_raw = stplanr::overline(
    uptake_list[[p]],
    attrib = c("bicycle", "bicycle_go_dutch", "quietness", "gradient_smooth"), # todo: add other modes
    fun = list(sum = sum, mean = mean)
  )
  rnet = rnet_raw |>
    transmute(
      bicycle = round(bicycle_sum),
      # `Bicycle (Near Market)` = round(cyclists_near_sum),
      bicycle_go_dutch = round(bicycle_go_dutch_sum),
      # `Bicycle (Ebike)` = round(cyclists_ebike_sum),
      gradient = round(gradient_smooth_mean * 100),
      quietness = round(quietness_mean)
      # col = cut(quietness, quietness_breaks, labels = pal_quietness, right = FALSE)
    ) |> 
    dplyr::arrange(bicycle)
  rnet_commute_list[[p]] = rnet
}
rnet_commute_list

saveRDS(rnet_commute_list, "rnet_commute_list.Rds")

piggyback::pb_upload("rnet_commute_list.Rds")
