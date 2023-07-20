# Prepare Commute Flow Data

library(sf)
library(tmap)
library(stplanr)
library(dplyr)
library(tidyr)
tmap_mode("view")

secure_path = Sys.getenv("NPT_TEAMS_PATH")

fls = file.path(secure_path,"secure_data/commute/Sustrans_Leeds_Extract.csv")

flow = readr::read_csv(fls)

# Summarise to OD pair
names(flow) = c("mode_id","cars_household","geo_code1","LA_work","geo_code2")
flow = flow[,c(c("geo_code1","geo_code2","mode_id"))]

mode_match <- data.frame(mode_id = paste0("0",1:9), 
                         mode = c("from_home","train","underground","bus","car","bicycle","foot","taxi","no_fixed_place"))

flow = left_join(flow, mode_match, by = "mode_id")

flow$count = 1
flow$mode_id = NULL

flow_short = group_by(flow, geo_code1, geo_code2, mode)
flow_short = summarise(flow_short, count = sum(count))


flow_wide = pivot_wider(flow_short, names_from = "mode", values_from = "count",
                        id_cols = c("geo_code1","geo_code2"),values_fill = 0)



# Get Centroids
datazone_cent <- st_read("../inputdata/data_zone_centroids.geojson")

summary(flow_wide$geo_code1 %in% datazone_cent$DataZone)
summary(flow_wide$geo_code2 %in% datazone_cent$DataZone) # 18358, all home/ofshore etc
table(flow_wide$geo_code2[!flow_wide$geo_code2 %in% datazone_cent$DataZone])

#NOFIXEDWP OFF-SHORE WORKFHOME 
#6976      4406      6976 
flow_wide = flow_wide[flow_wide$geo_code2 %in% datazone_cent$DataZone,]

flow_wide$all = rowSums(flow_wide[,c("no_fixed_place","bus","car","taxi","foot","bicycle","train","from_home","underground")])
summary(flow_wide$all)

# Data Zones
dir.create(file.path(tempdir(),"SIMD"))
unzip("../inputdata/SIMD/simd2020_withgeog.zip",
      exdir = file.path(tempdir(),"SIMD"))
zones <- read_sf(file.path(tempdir(),"SIMD/simd2020_withgeog/sc_dz_11.shp"))
unlink(file.path(tempdir(),"SIMD"), recursive = TRUE)
zones = zones[,"DataZone"]

#OA Centroids
dir.create(file.path(tempdir(),"OA"))
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_OA_2011_Centroids.zip",
      exdir = file.path(tempdir(),"OA"))
subpoints_origins <- read_sf(file.path(tempdir(),"OA/OutputArea2011_PWC.shp"))
unlink(file.path(tempdir(),"OA"), recursive = TRUE)
subpoints_origins = subpoints_origins[,"code"]
subpoints_origins = st_transform(subpoints_origins, 4326)


test = flow_wide[flow_wide$all > 100,]

#Jitter
flow_jitter = odjitter::jitter(
  od = test,
  zones = zones,
  zones_d = zones,
  origin_key = "DataZone",
  destination_key = "DataZone",
  subpoints_origins = subpoints_origins,
  subpoints_destinations = subpoints_origins,
  disaggregation_key = "all",
  disaggregation_threshold = 30,
  show_command = TRUE
)

# command sent to the system:                                                                                               
# odjitter jitter --od-csv-path C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/od.csv --zones-path C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/zones.geojson --zone-name-key DataZone --origin-key DataZone --destination-key DataZone --subpoints-origins-path C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/subpoints_origins.geojson --subpoints-destinations-path C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/subpoints_destinations.geojson --disaggregation-key all --disaggregation-threshold 30 --rng-seed 38736 --deduplicate-pairs  --output-path C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/od_jittered.geojsonScraped 6976 zones from C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/zones.geojson
# Scraped 46351 subpoints from C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/subpoints_origins.geojson
# Scraped 46351 subpoints from C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh/subpoints_destinations.geojson
# Disaggregating OD data
# thread 'main' panicked at 'no entry found for key', C:\Users\earmmor\.cargo\git\checkouts\odjitter-2ee039e64a6d7c8d\af35f08\src\lib.rs:203:55
# note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
# Error: Cannot open "C:\Users\earmmor\AppData\Local\Temp\RtmpmasHPh\od_jittered.geojson"; The source could be corrupt or not supported. See `st_drivers()` for a list of supported formats.
# In addition: Warning messages:
#   1: In CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
#                        GDAL Error 1: At line 2, character 1: Unterminated array
#                      2: In CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
#                                           GDAL Error 4: Failed to read GeoJSON data

dl = od2line(flow_wide, datazone_cent)

# Match to old format
names(dl)[names(dl) == "car"] <- "car_driver"
dl$dist_euclidean <- as.numeric(st_length(dl))

#Drop to sub 30km
dl = dl[dl$dist_euclidean < 30000,]

saveRDS(dl, file.path(secure_path,"secure_data/commute/commute_dl_sub30km.Rds"))

