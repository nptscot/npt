# Aim: get generate a test area
# location_name = "Forth bridge"
get_area = function(location_name = "Forth bridge", d = 20) {
  location_sf = tmaptools::geocode_OSM(location_name, as.sf = TRUE)
  zonebuilder::zb_doughnut(x = location_sf, n_circles = 1, distance = d)
}
