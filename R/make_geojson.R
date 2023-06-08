#' Function to make geojson for tippecanoe (zones)
#' @param z sf data frame with zones
#' @param path folder to save results
#' @example 
# z_stats <- routes_to_zone_stats(r_fast, "fastest", "commute")
# zones <- readRDS("../inputdata/iz_scotlands_uk.Rds")
# zones <- zones[,c("InterZone","TotPop2011","geometry")]
# names(zones)[1] <- "geo_code"
# z <- dplyr::left_join(zones, z_stats, "geo_code")
# z$orig_pcycle_baseline_fastest_commute <- round(z$orig_bicycle_fastest_commute / z$orig_all_fastest_commute * 100, 1)
# z$orig_pcycle_godutch_fastest_commute <- round(z$orig_bicycle_go_dutch_fastest_commute / z$orig_all_fastest_commute * 100, 1)
# z$dest_pcycle_baseline_fastest_commute <- round(z$dest_bicycle_fastest_commute / z$dest_all_fastest_commute * 100, 1)
# z$dest_pcycle_godutch_fastest_commute <- round(z$dest_bicycle_go_dutch_fastest_commute / z$dest_all_fastest_commute * 100, 1)
# z <- z[,c("geo_code","TotPop2011",
#           "orig_pcycle_baseline_fastest_commute",
#           "orig_pcycle_godutch_fastest_commute",
#           "dest_pcycle_baseline_fastest_commute",
#           "dest_pcycle_godutch_fastest_commute",
#           "orig_quietness_mean_fastest_commute",
#           "orig_hilliness_mean_fastest_commute",
#           "dest_quietness_mean_fastest_commute",
#           "dest_hilliness_mean_fastest_commute"
# 
# )]
# make_geojson_zones(z, "outputs/zones.geojson")

make_geojson_zones <- function(z, path = "outputs/zones.geojson"){
  
  if(ncol(z) > 15){
    warning("Thats a lot of columns for the GeoJSON, are they all needed?")
  }
  z <- sf::st_transform(z, 4326)
  sf::st_precision(z) <- 1000000
  sf::st_write(z,path, delete_dsn = TRUE)
  
}

#' @examples
#' x = c(0, 1, 2.3, 9.9, 10, 10.1, 10.9, 20)
#' round_sdc(x)
round_sdc = function(x, threshold = 10, digits = 0) {
  sel_sdc = x < threshold & x > 0
  mean_sdc = mean(x[sel_sdc])
  x[sel_sdc] = mean_sdc
  max(round(x, digits = digits), 1)
}

