# Aim: get ebike and quietest route networks

routes_ebike_raw = readRDS("inputdata/routes_commute_subset_ebike.Rds")
routes_ebike = cyclestreets:::add_columns(routes_ebike_raw)
r_commute = list()
r_commute$ebike = routes_ebike
p = "ebike"
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
    Gradient = round(gradient_smooth_mean * 100),
    Quietness = round(quietness_mean)
    # col = cut(Quietness, quietness_breaks, labels = pal_quietness, right = FALSE)
  ) %>% 
  dplyr::arrange(bicycle)
rnet_commute_list[[p]] = rnet