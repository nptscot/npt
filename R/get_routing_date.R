
# Original from get_routes:
# routes_filtered = routes_filtered %>% 
#   mutate(routing_integer = stringr::str_sub(edition, start = -6),
#          routing_date = lubridate::ymd(routing_integer))

# # Test data:
# from = stplanr::geo_code("Edinburgh")
# to = stplanr::geo_code("Edinburgh University")
# dput(round(from, 3))
# dput(round(to, 3))

get_routing_date = function(
    from = c(-3.188, 55.953),
    to =c(-3.188, 55.944), 
    base_url = paste0(
      "http://",
      Sys.getenv("CYCLESTREETS_BATCH"),
      "-api.cyclestreets.net"
    ),
    pat = Sys.getenv("CYCLESTREETS_BATCH")
) {
  single_route = cyclestreets::journey(from, to, pat = pat, base_url = base_url)
  route_date_raw = single_route$edition[1]
  # [1] "routing230701"
  route_date_integer = stringr::str_sub(route_date_raw, start = -6)
  routing_date = lubridate::ymd(route_date_integer)
  routing_date
}

# get_routing_date() # Works!
# [1] "2023-07-01"

