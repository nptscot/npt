
library(targets)
library(sf)

# Aim: test batch routing
Sys.getenv("CYCLESTREETS_PW")
Sys.getenv("CYCLESTREETS_BATCH")
Sys.getenv("CYCLESTREETS_URL")

tar_load(od_commute_subset)

od_test_100 = od_commute_subset[1:100, ]
od_test_100$id = seq(nrow(od_test_100))
sf::write_sf(od_test_100, "od_test_100.geojson", delete_dsn = TRUE)

system.time({
batch_test_100 = cyclestreets::batch(
  desire_lines = od_test_100,
  serverId = "27",
  strategies = "fastest",
  username = "robinlovelace",
  password = Sys.getenv("CYCLESTREETS_PW"),
  pat = Sys.getenv("CYCLESTREETS_BATCH"),
  silent = FALSE
)
})

Sys.time(
  {
  route_test_100 <- router::route(desire_lines = od_test_100, route_fun = cyclestreets::journey, pat = "ca43ed677e5e6fe7")
}
)
100 / 15

od_test_1000 = od_commute_subset[1:1000, ]

Sys.time({
  batch_test_1000 = cyclestreets::batch(
    desire_lines = od_test_1000,
    serverId = "27",
    strategies = "fastest",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW"),
    pat = Sys.getenv("CYCLESTREETS_BATCH"),
    silent = FALSE
  )
})

od_test_2000 = od_commute_subset[1:2000, ]
system.time({
  batch_test_2000 <- cyclestreets::batch(
    desire_lines = od_test_2000,
    serverId = "27",
    strategies = "fastest",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW"),
    pat = Sys.getenv("CYCLESTREETS_BATCH"),
    silent = FALSE
  )
})
# 2000 routes, 254s, 7.9 routes/s
# user  system elapsed 
# 23.267   1.905 253.837 


od_test_10000 = od_commute_subset[1:10000, ]
system.time({
  batch_test_10000 <- cyclestreets::batch(
    desire_lines = od_test_10000,
    serverId = "27",
    strategies = "fastest",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW"),
    pat = Sys.getenv("CYCLESTREETS_BATCH"),
    silent = FALSE
  )
})
# 2000 routes, 254s, 7.9 routes/s
# user  system elapsed 
# 23.267   1.905 253.837 

od_test_10000 = od_commute_subset[1:10000, ]

system.time({
  batch_test_all <- cyclestreets::batch(
    desire_lines = od_commute_subset[1:10000, ],
    serverId = "27",
    strategies = "balanced",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW"),
    pat = Sys.getenv("CYCLESTREETS_BATCH"),
    silent = FALSE
  )
})
