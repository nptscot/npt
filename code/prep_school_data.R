# Prep School Locations
library(sf)
library(tmap)
library(stplanr)
library(dplyr)
tmap_mode("view")
remotes::install_github("nptscot/cyclestreets-r")


if(!file.exists("D:/GitHub/atumscot/inputdata/Schools/school_locations.geojson")){
  dir.create("temp")
  unzip("D:/GitHub/atumscot/inputdata/Schools/SG_SchoolRoll_2022.zip", exdir = "temp")
  locs = read_sf("temp/SG_SchoolRoll_2022/SG_SchoolRoll_2022.shp")
  unlink("temp", recursive = TRUE)
  #qtm(locs)
  locs = locs[,c("SeedCode","SchoolType","SchoolName")]
  locs = st_transform(locs, 4326)
  
  st_precision(locs) <- 1000000
  st_write(locs, "D:/GitHub/atumscot/inputdata/Schools/school_locations.geojson")
} else {
  locs <- st_read("D:/GitHub/atumscot/inputdata/Schools/school_locations.geojson")
}

# Prep School Flows
flow = readxl::read_excel("D:/OneDrive - University of Leeds/Documents/NPT Scot/Secure Data/A2200 pupils by school and data zone.xlsx")
names(flow)[5] = "DataZone"

if(!file.exists("D:/GitHub/atumscot/inputdata/data_zone_centroids.geojson")){
  dir.create("temp")
  unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_DataZoneCent_2011.zip", exdir = "temp")
  datazone_cent = read_sf("temp/SG_DataZone_Cent_2011.shp")
  unlink("temp", recursive = TRUE)
  #qtm(locs)
  datazone_cent = datazone_cent[,c("DataZone")]
  datazone_cent = st_transform(datazone_cent, 4326)
  
  st_precision(datazone_cent) <- 100000
  st_write(datazone_cent, "D:/GitHub/atumscot/inputdata/data_zone_centroids.geojson")
} else {
  datazone_cent <- st_read("D:/GitHub/atumscot/inputdata/data_zone_centroids.geojson")
}


summary(flow$SeedCode %in% locs$SeedCode)
foo = flow[!flow$SeedCode %in% locs$SeedCode,] # 7 missing schools
flow = flow[flow$SeedCode %in% locs$SeedCode,]

summary(flow$DataZone %in% datazone_cent$DataZone)


flow_sf = od2line(flow, 
                  zones = datazone_cent, 
                  destinations = locs, dest_code = "SeedCode", origin_code = "DataZone", silent = FALSE)

flow_sf$length_km <- round(as.numeric(st_length(flow_sf)) / 1000,1)
summary(flow_sf$length_km)
flow_sf <- flow_sf[flow_sf$length_km < 30,]
saveRDS(flow_sf, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_dl_sub30km.Rds")

flow_sf <- flow_sf[,c("DataZone","SeedCode","count")]
flow_sf$route_id <- 1:nrow(flow_sf)

flow_nocount = flow_sf[,c("route_id")]

source("R/get_routes.R")

# Works for first 250, but fails on larger batches
routes_school = get_routes(od = flow_nocount,
                            plans = c("fastest","quietest","balanced","ebike"), 
                           purpose = "school",
                            folder = "outputdata", 
                           batch = TRUE, nrow_batch = 20000)

