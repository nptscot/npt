#demo_data

library(sf)
library(stplanr)
library(dplyr)

dl <- readRDS("C:/Users/earmmor/Downloads/desire_lines_scotland.Rds")
routes <- readRDS("../../mem48/atum/travel2work_routes.Rds")
routes <- left_join(st_drop_geometry(dl), routes, by = c("geo_code1" = "fromPlace",
                                                         "geo_code2" = "toPlace"))
routes <- st_as_sf(routes)
routes <- routes[!st_is_empty(routes),]
routes$bicycle_go_dutch <- routes$bicycle * 10

rnet <- overline2(routes, attrib = c("bicycle","bicycle_go_dutch","all"), ncores = 30)


saveRDS(rnet,"outputs/rnet_commute2.Rds")
saveRDS(routes,"outputs/uptake_commute2.Rds")
