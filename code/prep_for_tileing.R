library(sf)
library(dplyr)

rnet <- readRDS("outputs/rnet_commute.Rds")
routes <- readRDS("outputs/uptake_commute.Rds")
zones <- readRDS("../inputdata/iz_scotlands_uk.Rds")

# Clean RNET
rnet$bicycle <- round(rnet$bicycle)
rnet$bicycle_go_dutch <- round(rnet$bicycle_go_dutch)
rnet <- rnet[rnet$bicycle_go_dutch > 0,]

# Clean Routes
routes <- routes[,c("geo_code1","geo_code2",
                    "all","train","bus","car_driver","car_passenger","bicycle","foot",
                    "other","bicycle_go_dutch","dist_euclidean","route_number",
                    "plan","length_route","grammesCO2saved","calories",
                    "geometry")]

routes_single <- routes %>%
  group_by(geo_code1,geo_code2,route_number) %>%
  summarise(all = round(all[1],1),
            train = round(train[1],1),
            bus = round(bus[1],1),
            car_driver = round(car_driver[1],1),
            car_passenger = round(car_passenger[1],1),
            bicycle = round(bicycle[1],1),
            foot = round(foot[1],1),
            other = round(other[1],1),
            bicycle_go_dutch = round(bicycle_go_dutch[1],1),
            dist_euclidean = round(dist_euclidean[1],1),
            plan = plan[1],
            length_route = length_route[1],
            grammesCO2saved = grammesCO2saved[1],
            calories = calories[1])

routes_single$route_number <- NULL

# Clean Zones
zones <- zones[,c("InterZone","TotPop2011","geometry")]


# Write GeoJSON
zones <- st_transform(zones, 4326)


st_precision(rnet) <- 1000000
st_precision(routes_single) <- 1000000
st_precision(zones) <- 1000000

st_write(rnet,"outputs/rnet.geojson", delete_dsn = TRUE)
st_write(routes_single,"outputs/routes.geojson", delete_dsn = TRUE)
st_write(zones,"outputs/zones.geojson", delete_dsn = TRUE)

