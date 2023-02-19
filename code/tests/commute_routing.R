remotes::install_github("nptscot/cyclestreets-r")
library(cyclestreets)

od = readRDS("../atumscot/outputdata/od_commute_subset.Rds")
od = od[!duplicated(od$geometry),] #TODO; Fix bug with jittering?
#od = od[1:1000,]

fromPlace = lwgeom::st_startpoint(od)
toPlace = lwgeom::st_endpoint(od)

fromPlace <- st_as_sf(data.frame(geometry = fromPlace))
toPlace <- st_as_sf(data.frame(geometry = toPlace))


r_quiet <- journey2(fromPlace,
              toPlace,
              id = od$route_id,
              plan = "quietest", 
              host_con = 10,
              base_url = "http://1309a51d768b8d51-api.cyclestreets.net",
              pat = Sys.getenv("CYCLESTREETS_NPT"),
              segments = TRUE)

saveRDS(r_quiet,"../atumscot/outputdata/routes_commute_subset_quiet.Rds")

r_balance <- journey2(fromPlace,
                    toPlace,
                    id = od$route_id,
                    plan = "balanced", 
                    host_con = 10,
                    base_url = "http://1309a51d768b8d51-api.cyclestreets.net",
                    pat = Sys.getenv("CYCLESTREETS_NPT"),
                    segments = TRUE)

saveRDS(r_balance,"../atumscot/outputdata/routes_commute_subset_balance.Rds")

r_ebike <- journey2(fromPlace,
                      toPlace,
                      id = od$route_id,
                      plan = "ebike", 
                      host_con = 10,
                      base_url = "http://1309a51d768b8d51-api.cyclestreets.net",
                      pat = Sys.getenv("CYCLESTREETS_NPT"),
                      segments = TRUE)

saveRDS(r_ebike,"../atumscot/outputdata/routes_commute_subset_ebike.Rds")


# Check for missing

id_quiet <- unique(r_quiet$id)
id_balance <- unique(r_balance$id)
id_ebike <- unique(r_ebike$id)


#foo <- opentripplanner:::otp_clean_input(fromPlace, "fromPlace")
