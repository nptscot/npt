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

datazone = st_read("outputs/data_zones.geojson")
datazone_cent = st_transform(datazone, 27700)
datazone_cent = st_centroid(datazone_cent)
datazone_cent = st_transform(datazone_cent, 4326)
datazone_cent = datazone_cent[,"DataZone"]
names(flow)[5] = "DataZone"

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

