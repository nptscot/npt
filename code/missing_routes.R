#Missing routes

tar_load(r_commute)
tar_load(od_commute_subset)

routes = r_commute$fastest

od_missing = od_commute_subset[!od_commute_subset$route_number %in% routes$route_number,]

library(tmap)
tmap_mode("view")
qtm(od_missing)
saveRDS(od_missing,"data-raw/od_missing.Rds")

tar_load(subpoints_destinations)

points = subpoints_destinations[od_missing,]
