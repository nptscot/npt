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

# Standardised the modes with other data
flow_wide$public_transport = flow_wide$bus + flow_wide$train + flow_wide$underground
flow_wide$bus = NULL
flow_wide$train = NULL
flow_wide$underground = NULL
flow_wide$no_fixed_place = NULL
flow_wide$from_home = NULL

# Get Centroids
datazone_cent <- st_read("../inputdata/data_zone_centroids.geojson")

summary(flow_wide$geo_code1 %in% datazone_cent$DataZone)
summary(flow_wide$geo_code2 %in% datazone_cent$DataZone) # 18358, all home/ofshore etc
table(flow_wide$geo_code2[!flow_wide$geo_code2 %in% datazone_cent$DataZone])

#NOFIXEDWP OFF-SHORE WORKFHOME 
#6976      4406      6976 
flow_wide = flow_wide[flow_wide$geo_code2 %in% datazone_cent$DataZone,]

flow_wide$all = rowSums(flow_wide[,c("car","taxi","foot","bicycle","public_transport")])
summary(flow_wide$all)

# Data Zones
dir.create(file.path(tempdir(),"SIMD"))
unzip("../inputdata/SIMD/simd2020_withgeog.zip",
      exdir = file.path(tempdir(),"SIMD"))
zones <- read_sf(file.path(tempdir(),"SIMD/simd2020_withgeog/sc_dz_11.shp"))
unlink(file.path(tempdir(),"SIMD"), recursive = TRUE)
zones = zones[,c("DataZone","TotPop2011","ResPop2011","HHCnt2011")]
zones = st_make_valid(zones)
saveRDS(zones, "inputdata/DataZones.Rds")

#OA Centroids
dir.create(file.path(tempdir(),"OA"))
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_OA_2011_Centroids.zip",
      exdir = file.path(tempdir(),"OA"))
subpoints_origins <- read_sf(file.path(tempdir(),"OA/OutputArea2011_PWC.shp"))
unlink(file.path(tempdir(),"OA"), recursive = TRUE)
subpoints_origins = subpoints_origins[,"code"]
subpoints_origins = st_transform(subpoints_origins, 4326)


dl = od2line(flow_wide, datazone_cent)

# Match to old format
#names(dl)[names(dl) == "car"] <- "car_driver"
dl$dist_euclidean <- as.numeric(st_length(dl))

#Drop to sub 30km
dl = dl[dl$dist_euclidean < 30000,]

saveRDS(dl, file.path(secure_path,"secure_data/commute/commute_dl_sub30km.Rds"))

