# Aim: return data from journey2 with same contents as from cyclestreets::journey


# Get data-------------------------------------------------------------------------
setwd("inputdata/")
system("gh release download routes_commute --clobber")
setwd("..")

remotes::install_github("nptscot/cyclestreets-r")
library(cyclestreets)

targets::tar_load(od_commute_subset)
od = od_commute_subset
od = od[!duplicated(od$geometry),] #TODO; Fix bug with jittering?
od = od[3,]

fromPlace = lwgeom::st_startpoint(od)
toPlace = lwgeom::st_endpoint(od)

fromPlace <- st_as_sf(data.frame(geometry = fromPlace))
toPlace <- st_as_sf(data.frame(geometry = toPlace))

system.time({
  r_quiet <- journey2(fromPlace,
                      toPlace,
                      id = od$route_id,
                      plan = "quietest", 
                      host_con = 10,
                      base_url = "http://1309a51d768b8d51-api.cyclestreets.net",
                      pat = Sys.getenv("CYCLESTREETS_BATCH"),
                      segments = TRUE)
  
})

library(stplanr)
r_quiet_original = route(l = od, route_fun = journey, plan = "quietest")

waldo::compare(r_quiet_original, r_quiet)
waldo::compare(r_quiet_original$geometry, r_quiet$geometry)

geometries_in_original = r_quiet$geometry %in% r_quiet_original$geometry
r_quiet_updated = r_quiet[geometries_in_original, ]

sum(sf::st_length(r_quiet))
sum(sf::st_length(r_quiet_original))

# Take first 3 rows of each
r_1 = r_quiet[1:3, ]
r_2 = r_quiet_original[1:3, ]
waldo::compare(r_1, r_2)

setdiff(names(r_1), names(r_2))
setdiff(names(r_2), names(r_1))
od

# Mission: write code to convert R1 into R2 with reference to the dataframe
sf::write_sf(r_1, "../cyclestreets-r/data-raw/r_1.geojson")
sf::write_sf(r_1, "../cyclestreets-r/data-raw/r_2.geojson")
sf::write_sf(od, "../cyclestreets-r/data-raw/od-test.geojson")

# [17] "gradient_segment"        "elevation_change"        "gradient_smooth"  
