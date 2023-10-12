library(sf)
nret = read_sf("outputs/simplified_network_npt.geojson")
nret = st_transform(nret, 4326)
sf::st_precision(nret) <- 1000000
st_write(nret, "outputs/simplified_network_npt_4326.geojson")
