# Run first lines of _targets.R

list(plans = c("fastest", "balanced", "quietest", "ebike"),
     plans = c("fastest"),
     min_flow = 430, # Set to 1 for full build, set to high value (e.g. 400) for tests
     date_routing = "2023-02-14")

tar_load(od_commute_subset)
View(od_commute_subset)
# Visualise data on map
mapview::mapview(od_commute_subset)
r = route(l = od_commute_subset, route_fun = cyclestreets::journey, plan = "balanced")
mapview::mapview(r)
