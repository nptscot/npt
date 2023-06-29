# Prep School Locations
library(sf)
library(tmap)
library(stplanr)
library(dplyr)
tmap_mode("view")

secure_path = Sys.getenv("NPT_TEAMS_PATH")

if(!file.exists("../inputdata/Schools/school_locations.geojson")){
  dir.create("temp")
  unzip("D:/GitHub/atumscot/inputdata/Schools/SG_SchoolRoll_2022.zip", exdir = "temp")
  locs = read_sf("temp/SG_SchoolRoll_2022/SG_SchoolRoll_2022.shp")
  unlink("temp", recursive = TRUE)
  #qtm(locs)
  locs = locs[,c("SeedCode","SchoolType","SchoolName")]
  locs = st_transform(locs, 4326)
  
  locs_extra = read.csv("../inputdata/Schools/school_locations_extra.csv")
  locs_extra = st_as_sf(locs_extra, coords = c("lng","lat"), crs = 4326)
  names(locs_extra) = names(locs)
  
  locs = rbind(locs, locs_extra)
  
  st_precision(locs) <- 1000000
  st_write(locs, "../inputdata/Schools/school_locations.geojson")
} else {
  locs <- st_read("../inputdata/Schools/school_locations.geojson")
}

# Prep School Flows if file not already present
flow = readxl::read_excel(file.path(secure_path,"secure_data/schools/raw_data/A2200 pupils by school and data zone.xlsx"))
names(flow)[5] = "DataZone"

if(!file.exists("../inputdata/data_zone_centroids.geojson")){
  dir.create("temp")
  unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_DataZoneCent_2011.zip", exdir = "temp")
  datazone_cent = read_sf("temp/SG_DataZone_Cent_2011.shp")
  unlink("temp", recursive = TRUE)
  #qtm(locs)
  datazone_cent = datazone_cent[,c("DataZone")]
  datazone_cent = st_transform(datazone_cent, 4326)
  
  st_precision(datazone_cent) <- 100000
  st_write(datazone_cent, "../inputdata/data_zone_centroids.geojson")
} else {
  datazone_cent <- st_read("../inputdata/data_zone_centroids.geojson")
}


summary(flow$SeedCode %in% locs$SeedCode)
foo = flow[!flow$SeedCode %in% locs$SeedCode,] # 7 missing schools added in locs_extra, some are closed
flow = flow[flow$SeedCode %in% locs$SeedCode,]

summary(flow$DataZone %in% datazone_cent$DataZone)

# Jitter OD

flow = flow[,c("DataZone","SeedCode","count","LaCode","schooltype","SchlName")]

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

sf_use_s2(FALSE)
locs_buff = st_buffer(locs, 0.0001)
locs_buff = locs_buff[,"SeedCode"]

summary(flow$DataZone %in% zones$DataZone)

# Failing make reprex
flow_jitter = odjitter::jitter(
  od = flow,
  zones = zones,
  zones_d = locs_buff,
  origin_key = "DataZone",
  destination_key = "SeedCode",
  subpoints_origins = subpoints_origins,
  subpoints_destinations = locs,
  disaggregation_key = "count",
  disaggregation_threshold = 30,
  show_command = TRUE
)

# TODO: De duplicate lines, reduces routing

flow_jitter$length_km <- round(as.numeric(st_length(flow_jitter)) / 1000,1)
summary(flow_jitter$length_km)
flow_jitter <- flow_jitter[flow_jitter$length_km < 30,]
flow_jitter <- flow_jitter[,c("DataZone","SeedCode","count")]
flow_jitter$route_id <- 1:nrow(flow_jitter)
flow_jitter$count <- round(flow_jitter$count)

saveRDS(flow_jitter, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_dl_sub30km.Rds")


