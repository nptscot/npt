# Aim: generate 'dasymetric' representations of key datasets.
# Note: first section of commented code was run once by @mem48 to generate the dasymetric datasets.

# # dasymetric map
# library(sf)
# library(tmap)
# tmap_mode("view")

# # Get data

# GB_full = readRDS("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_full.Rds")
# GB_gen = readRDS("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_generalised.Rds")
# GB_supgen = readRDS("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_super_generalised.Rds")

# GB_gen <- st_as_sf(GB_gen)
# GB_supgen <- st_as_sf(GB_supgen)
# GB_full <- st_as_sf(GB_full)

# buildings_high <- geojsonsf::geojson_sf("../../creds2/CarbonCalculator/data/zoomstackgeojson/local_buildings.geojson")
# buildings_med <- geojsonsf::geojson_sf("../../creds2/CarbonCalculator/data/zoomstackgeojson/district_buildings.geojson")
# buildings_low <- geojsonsf::geojson_sf("../../creds2/CarbonCalculator/data/zoomstackgeojson/urban_areas.geojson")

# sf_use_s2(FALSE)

# buildings_high$id <- 1:nrow(buildings_high)
# buildings_med$id <- 1:nrow(buildings_med)
# buildings_low$id <- 1:nrow(buildings_low)

# buildings_high <- st_make_valid(buildings_high)
# buildings_med <- st_make_valid(buildings_med)
# buildings_low <- st_make_valid(buildings_low)

# #buildings_low2 <- st_join(buildings_low, lsoa)
# buildings_med2 <- st_join(buildings_med, GB_full)
# buildings_high2 <- st_join(buildings_high, GB_full)


# #saveRDS(buildings_low2, "outputs/buildings_low_lsoa.Rds")
# saveRDS(buildings_med2, "outputs/buildings_med_lsoa.Rds")
# saveRDS(buildings_high2, "outputs/buildings_high_lsoa.Rds")

# #buildings_low2 = readRDS("outputs/buildings_low_lsoa.Rds")
# buildings_med2 = readRDS("outputs/buildings_med_lsoa.Rds")
# buildings_high2 = readRDS("outputs/buildings_high_lsoa.Rds")

# buildings_low_nat = buildings_low[buildings_low$type == "National",]
# buildings_low_reg = buildings_low[buildings_low$type == "Regional",]

# buildings_low_nat2 <- st_join(buildings_low_nat, GB_supgen)
# buildings_low_reg2 <- st_join(buildings_low_reg, GB_gen)

# saveRDS(buildings_low_nat2, "outputs/buildings_low_nat_lsoa.Rds")
# saveRDS(buildings_low_reg2, "outputs/buildings_low_reg_lsoa.Rds")


# # Split Duplicates

# buildings_high2_dup <- unique(buildings_high2$id[duplicated(buildings_high2$id)])
# buildings_med2_dup <- unique(buildings_med2$id[duplicated(buildings_med2$id)])
# #buildings_low2_dup <- unique(buildings_low2$id[duplicated(buildings_low2$id)])

# buildings_high_dup <- buildings_high2[buildings_high2$id %in% buildings_high2_dup,]
# buildings_high_nodup <- buildings_high2[!buildings_high2$id %in% buildings_high2_dup,]

# buildings_med_dup <- buildings_med2[buildings_med2$id %in% buildings_med2_dup,]
# buildings_med_nodup <- buildings_med2[!buildings_med2$id %in% buildings_med2_dup,]

# #buildings_low_dup <- buildings_low2[buildings_low2$id %in% buildings_low2_dup,]
# #buildings_low_nodup <- buildings_low2[!buildings_low2$id %in% buildings_low2_dup,]

# rm(buildings_high, buildings_high2, buildings_low, buildings_low2, buildings_med, buildings_med2)

# buildings_low_nat2_dup <- unique(buildings_low_nat2$id[duplicated(buildings_low_nat2$id)])
# buildings_low_nat_dup <- buildings_low_nat2[buildings_low_nat2$id %in% buildings_low_nat2_dup,]
# buildings_low_nat_nodup <- buildings_low_nat2[!buildings_low_nat2$id %in% buildings_low_nat2_dup,]

# buildings_low_reg2_dup <- unique(buildings_low_reg2$id[duplicated(buildings_low_reg2$id)])
# buildings_low_reg_dup <- buildings_low_reg2[buildings_low_reg2$id %in% buildings_low_reg2_dup,]
# buildings_low_reg_nodup <- buildings_low_reg2[!buildings_low_reg2$id %in% buildings_low_reg2_dup,]


# # Function to split up polygons
# split_buildings = function(b,z){
#   suppressMessages(b2 <- st_intersection(b, z))
#   b2 <- b2[!duplicated(b2$geometry),]
#   b2$geo_code = b2$geo_code.1
#   b2$geo_code.1 <- NULL
#   b2  
# }


# split_buildings2 = function(b,z){
#   if(nrow(z) > 2){
#     suppressMessages(wth <- st_within(z, b[1,]))
#     wth <- as.logical(lengths(wth))
#     zin <- z[wth,]
#     zout <- z[!wth,]
#     #qtm(b[1,]) + qtm(zin, fill = "red") + qtm(zout, fill = "blue")
#     suppressWarnings(suppressMessages(zout2 <- st_intersection(b[1,], zout)))
#     zout2$geo_code = zout2$geo_code.1
#     zout2$geo_code.1 <- NULL
#     b <- st_drop_geometry(b)
#     zin <- dplyr::left_join(zin, b, by = "geo_code")
#     zin <- zin[,names(zout2)]
#     fin <- rbind(zin, zout2)
#     return(fin)
#   } else {
#     suppressMessages(b2 <- st_intersection(b, z))
#     b2 <- b2[!duplicated(b2$geometry),]
#     b2$geo_code = b2$geo_code.1
#     b2$geo_code.1 <- NULL
#     return(b2)  
#   }
  
  
# }

# #Low National
# # names(buildings_low_nat_dup)[3] <- "geo_code"
# # names(GB_supgen)[1] <- "geo_code"
# # 
# # 
# # build_list <- dplyr::group_by(buildings_low_nat_dup, id)
# # build_list <- dplyr::group_split(build_list)
# # 
# # zone_list <- lapply(build_list, function(x){unique(x$geo_code)})
# # zone_list <- lapply(zone_list, function(x){GB_supgen[GB_supgen$geo_code %in% x,]})
# # 
# # buildings_low_nat_split = purrr::map2(build_list, zone_list, split_buildings, .progress = "Splitting National")
# # saveRDS(buildings_low_nat_split, "outputs/buildings_low_nat_split.Rds")
# # beepr::beep(8)

# #Low National
# names(buildings_low_nat_dup)[3] <- "geo_code"
# names(GB_supgen)[1] <- "geo_code"
# buildings_low_nat_dup <- st_as_sf(buildings_low_nat_dup)
# st_geometry(buildings_low_nat_dup) <- "geometry"
# #buildings_low_nat_dup$geometry <- st_as_sfc(buildings_low_nat_dup$geometry)
# #st_crs(buildings_low_nat_dup) = 4326
# buildings_low_nat_dup = st_make_valid(buildings_low_nat_dup)
# #buildings_low_nat_nodup$geometry <- st_as_sfc(buildings_low_nat_nodup$geometry)
# st_crs(buildings_low_nat_nodup) = 4326
# names(buildings_low_nat_nodup)[3] <- "geo_code"

# build_list <- dplyr::group_by(buildings_low_nat_dup, id)
# build_list <- dplyr::group_split(build_list)

# zone_list <- lapply(build_list, function(x){unique(x$geo_code)})
# zone_list <- lapply(zone_list, function(x){GB_supgen[GB_supgen$geo_code %in% x,]})

# buildings_low_nat_split = purrr::map2(build_list, zone_list, split_buildings2, .progress = "Splitting national")
# buildings_low_nat_split = dplyr::bind_rows(buildings_low_nat_split)
# buildings_low_nat = rbind(buildings_low_nat_split, buildings_low_nat_nodup)
# saveRDS(buildings_low_nat, "outputs/buildings_low_nat_lsoa_split.Rds")

# #Low Regional
# names(buildings_low_reg_dup)[3] <- "geo_code"
# names(GB_gen)[1] <- "geo_code"
# buildings_low_reg_dup <- st_as_sf(buildings_low_reg_dup)
# st_geometry(buildings_low_reg_dup) <- "geometry"
# buildings_low_reg_dup$geometry <- st_as_sfc(buildings_low_reg_dup$geometry)
# st_crs(buildings_low_reg_dup) = 4326
# buildings_low_reg_dup = st_make_valid(buildings_low_reg_dup)
# buildings_low_reg_nodup$geometry <- st_as_sfc(buildings_low_reg_nodup$geometry)
# st_crs(buildings_low_reg_nodup) = 4326
# names(buildings_low_reg_nodup)[3] <- "geo_code"

# build_list <- dplyr::group_by(buildings_low_reg_dup, id)
# build_list <- dplyr::group_split(build_list)

# zone_list <- lapply(build_list, function(x){unique(x$geo_code)})
# zone_list <- lapply(zone_list, function(x){GB_gen[GB_gen$geo_code %in% x,]})

# buildings_low_reg_split = purrr::map2(build_list, zone_list, split_buildings2, .progress = "Splitting Regional")
# buildings_low_reg_split = dplyr::bind_rows(buildings_low_reg_split)
# buildings_low_reg = rbind(buildings_low_reg_split, buildings_low_reg_nodup)
# saveRDS(buildings_low_reg, "outputs/buildings_low_reg_lsoa_split.Rds")


# #High
# names(GB_full)[1] <- "geo_code"

# build_list <- dplyr::group_by(buildings_high_dup, id)
# build_list <- dplyr::group_split(build_list)

# zone_list <- lapply(build_list, function(x){unique(x$geo_code)})
# zone_list <- lapply(zone_list, function(x){lsoa[lsoa$geo_code %in% x,]})

# buildings_high_split = purrr::map2(build_list, zone_list, split_buildings, .progress = "Splitting High")
# buildings_high_split = dplyr::bind_rows(buildings_high_split)

# buildings_high = rbind(buildings_high_nodup, buildings_high_split)
# saveRDS(buildings_high, "outputs/buildings_high_lsoa_split.Rds")
# beepr::beep(8)

# # Medium
# build_list <- dplyr::group_by(buildings_med_dup, id)
# build_list <- dplyr::group_split(build_list)

# zone_list <- lapply(build_list, function(x){unique(x$geo_code)})
# zone_list <- lapply(zone_list, function(x){GB_full[GB_full$geo_code %in% x,]})

# buildings_med_split = purrr::map2(build_list, zone_list, split_buildings2, .progress = "Splitting Med")
# buildings_med_split = dplyr::bind_rows(buildings_med_split)

# buildings_med = rbind(buildings_med_nodup, buildings_med_split)
# saveRDS(buildings_med, "outputs/buildings_med_lsoa_split.Rds")
# beepr::beep(8)