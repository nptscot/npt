library(sf)
library(dplyr)

rnet <- readRDS("./inputdata/rnet_commute.Rds")
routes <- readRDS("outputs/uptake_commute.Rds")
zones <- readRDS("./inputdata/iz_scotlands_uk.Rds")

# Clean RNET
if(class(rnet) == "list"){
  # Merge rnet
  #rnet_fast <- rnet$fastest
  rnet_fast <- readRDS("outputdata/rnet_commute_fastest.Rds")
  rnet_bal <- rnet$balanced
  names(rnet_fast)[1:4] <- paste0("fast_",names(rnet_fast)[1:4])
  names(rnet_bal)[1:4] <- paste0("balance_",names(rnet_bal)[1:4])
  
  rnet_fast$balance_bicycle <- 0
  rnet_fast$balance_bicycle_go_dutch <- 0
  rnet_fast$balance_Gradient <- 0
  rnet_fast$balance_Quietness <- 0
  
  rnet_bal$fast_bicycle <- 0
  rnet_bal$fast_bicycle_go_dutch <- 0
  rnet_bal$fast_Gradient <- 0
  rnet_bal$fast_Quietness <- 0
  
  rnet_bal <- rnet_bal[,names(rnet_fast)]
  rnet <- rbind(rnet_fast, rnet_bal)
  rnet <- stplanr::overline2(rnet, attrib = c("balance_bicycle","balance_bicycle_go_dutch","balance_Gradient","balance_Quietness",
                                              "fast_bicycle","fast_bicycle_go_dutch","fast_Gradient","fast_Quietness"))
  
  rnet$Gradient <- ifelse(rnet$balance_Gradient > rnet$balance_Gradient, rnet$balance_Gradient, rnet$fast_Gradient)
  rnet$Quietness <- ifelse(rnet$balance_Quietness > rnet$balance_Quietness, rnet$balance_Quietness, rnet$fast_Quietness)
  
  rnet <- rnet[,c("balance_bicycle","balance_bicycle_go_dutch",
                  "fast_bicycle","fast_bicycle_go_dutch",
                  "Gradient","Quietness",
                  "geometry")]
  
  rnet$balance_bicycle <- round(rnet$balance_bicycle)
  rnet$balance_bicycle_go_dutch <- round(rnet$balance_bicycle_go_dutch)
  rnet$fast_bicycle <- round(rnet$fast_bicycle)
  rnet$fast_bicycle_go_dutch <- round(rnet$fast_bicycle_go_dutch)
  
  rnet$tot <- rowSums(sf::st_drop_geometry(rnet[,c("balance_bicycle","balance_bicycle_go_dutch",
                                                   "fast_bicycle","fast_bicycle_go_dutch")]))
  rnet <- rnet[rnet$tot > 0,]
  rnet$tot <- NULL
  
} else {
  rnet$bicycle <- round(rnet$bicycle)
  rnet$bicycle_go_dutch <- round(rnet$bicycle_go_dutch)
  rnet <- rnet[rnet$bicycle_go_dutch > 0,]
}





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

