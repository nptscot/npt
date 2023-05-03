# Prep School Locations
library(sf)
library(tmap)
library(stplanr)
library(dplyr)
tmap_mode("view")
remotes::install_dev("cyclestreets")

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
flow_sf <- flow_sf[,c("DataZone","SeedCode","count")]
flow_sf$route_id <- 1:nrow(flow_sf)

saveRDS(flow_sf, "D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_dl_sub30km.Rds")

#flow_nocount = flow_sf[,c("route_id")]

source("R/get_routes.R")

# Works for first 250, but fails on larger batches
# routes_school = get_routes(od = flow_nocount,
#                             plans = c("fastest","quietest","balanced","ebike"), 
#                            purpose = "school",
#                             folder = "outputdata", 
#                            batch = TRUE, nrow_batch = 20000)

# flow_nodup = flow_nocount[!duplicated(flow_nocount$geometry),]
# flow_dup = flow_nocount[duplicated(flow_nocount$geometry),]
# flow_dup$match_id <- flow_nodup$route_id[match(flow_dup$geometry, flow_nodup$geometry)]





fromPlace = st_sf(lwgeom::st_startpoint(flow_sf))
toPlace = st_sf(lwgeom::st_endpoint(flow_sf))

# school_fast_batch = cyclestreets::journey2(fromPlace = fromPlace[1:500,], 
#                                            toPlace = toPlace[1:500,], 
#                                            id = flow_sf$route_id[1:500], 
#                                            plan = "fastest",
#                                            pat = Sys.getenv("CYCLESTREETS"),
#                                            host_con = 10,
#                                            segments = FALSE)
# 
# 
# qtm(school_fast_batch["id"]) + qtm(flow_sf[1:500,"route_id"], lines.col = "red")

# Batches of 10,000
ids <- 1:nrow(flow_sf)
ids <- split(ids, ceiling(seq_along(ids)/10000))

for(i in 1:length(ids)){
  message(i)
  school_fast_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                       toPlace = toPlace[ids[[i]],], 
                                       id = flow_sf$route_id[ids[[i]]], 
                                       plan = "fastest",
                                       pat = Sys.getenv("CYCLESTREETS"),
                                       host_con = 10,
                                       segments = TRUE)
  saveRDS(school_fast_batch,paste0("outputdata/school_fast_batch_",i,".Rds"))
}

school_fast <- list()
for(i in 1:8){
  school_fast[[i]] <- readRDS(paste0("outputdata/school_fast_batch_",i,".Rds"))
}
school_fast <- bind_rows(school_fast)
school_fast_missing <- flow_sf[!flow_sf$route_id %in% school_fast$id,]
school_fast_missing = cyclestreets::journey2(fromPlace = st_sf(lwgeom::st_startpoint(school_fast_missing)), 
                                           toPlace = st_sf(lwgeom::st_endpoint(school_fast_missing)), 
                                           id = school_fast_missing$route_id, 
                                           plan = "fastest",
                                           pat = Sys.getenv("CYCLESTREETS"),
                                           host_con = 1,
                                           segments = TRUE)
school_fast <- rbind(school_fast, school_fast_missing)
school_fast <- school_fast[,c("id","distance","gradient_smooth","length","quietness")]
school_fast[1:5] <- lapply(st_drop_geometry(school_fast[1:5]), as.numeric)
names(school_fast)[1] = "route_id"
saveRDS(school_fast,paste0("outputdata/school_fast_sub30k.Rds"))



# Quiet

for(i in 1:length(ids)){
  message(i)
  school_quiet_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                             toPlace = toPlace[ids[[i]],], 
                                             id = flow_sf$route_id[ids[[i]]], 
                                             plan = "quietest",
                                             pat = Sys.getenv("CYCLESTREETS"),
                                             host_con = 10,
                                             segments = TRUE)
  saveRDS(school_quiet_batch,paste0("outputdata/school_quietest_batch_",i,".Rds"))
}
school_quiet <- list()
for(i in 1:8){
  school_quiet[[i]] <- readRDS(paste0("outputdata/school_quietest_batch_",i,".Rds"))
}
school_quiet <- bind_rows(school_quiet)
school_quiet_missing <- flow_sf[!flow_sf$route_id %in% school_quiet$id,]
fromPlace = st_sf(lwgeom::st_startpoint(school_quiet_missing))
toPlace = st_sf(lwgeom::st_endpoint(school_quiet_missing))
school_quiet_missing = cyclestreets::journey2(fromPlace = fromPlace, 
                                             toPlace = toPlace, 
                                             id = school_quiet_missing$route_id, 
                                             plan = "quietest",
                                             pat = Sys.getenv("CYCLESTREETS"),
                                             host_con = 1,
                                             segments = TRUE)
school_quiet <- rbind(school_quiet, school_quiet_missing)
school_quiet <- school_quiet[,c("id","distance","gradient_smooth","length","quietness")]
school_quiet[1:5] <- lapply(st_drop_geometry(school_quiet[1:5]), as.numeric)
names(school_quiet)[1] = "route_id"
saveRDS(school_quiet,paste0("outputdata/school_quietest_sub30k.Rds"))


# Balanced

for(i in 1:length(ids)){
  message(i)
  school_balance_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                              toPlace = toPlace[ids[[i]],], 
                                              id = flow_sf$route_id[ids[[i]]], 
                                              plan = "balanced",
                                              pat = Sys.getenv("CYCLESTREETS"),
                                              host_con = 10,
                                              segments = TRUE)
  saveRDS(school_balance_batch,paste0("outputdata/school_balanced_batch_",i,".Rds"))
}

school_balanced <- list()
for(i in 1:8){
  school_balanced[[i]] <- readRDS(paste0("outputdata/school_balanced_batch_",i,".Rds"))
}
school_balanced <- bind_rows(school_balanced)
school_balanced_missing <- flow_sf[!flow_sf$route_id %in% school_balanced$id,]
school_balanced_missing = cyclestreets::journey2(fromPlace = st_sf(lwgeom::st_startpoint(school_balanced_missing)), 
                                             toPlace = st_sf(lwgeom::st_endpoint(school_balanced_missing)), 
                                             id = school_balanced_missing$route_id, 
                                             plan = "balanced",
                                             pat = Sys.getenv("CYCLESTREETS"),
                                             host_con = 1,
                                             segments = TRUE)
school_balanced <- rbind(school_balanced, school_balanced_missing)
school_balanced <- school_balanced[,c("id","distance","gradient_smooth","length","quietness")]
school_balanced[1:5] <- lapply(st_drop_geometry(school_balanced[1:5]), as.numeric)
names(school_balanced)[1] = "route_id"
saveRDS(school_balanced,paste0("outputdata/school_balanced_sub30k.Rds"))

# Ebike


for(i in 1:length(ids)){
  message(i)
  school_ebike_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                                toPlace = toPlace[ids[[i]],], 
                                                id = flow_sf$route_id[ids[[i]]], 
                                                plan = "ebike",
                                                pat = Sys.getenv("CYCLESTREETS"),
                                                host_con = 10,
                                                segments = TRUE)
  saveRDS(school_ebike_batch,paste0("outputdata/school_ebike_batch_",i,".Rds"))
}


school_ebike <- list()
for(i in 1:8){
  school_ebike[[i]] <- readRDS(paste0("outputdata/school_ebike_batch_",i,".Rds"))
}
school_ebike <- bind_rows(school_ebike)
school_ebike_missing <- flow_sf[!flow_sf$route_id %in% school_ebike$id,]
school_ebike_missing = cyclestreets::journey2(fromPlace = st_sf(lwgeom::st_startpoint(school_ebike_missing)), 
                                             toPlace = st_sf(lwgeom::st_endpoint(school_ebike_missing)), 
                                             id = school_ebike_missing$route_id, 
                                             plan = "ebike",
                                             pat = Sys.getenv("CYCLESTREETS"),
                                             host_con = 1,
                                             segments = TRUE)
school_ebike <- rbind(school_ebike, school_ebike_missing)
school_ebike <- school_ebike[,c("id","distance","gradient_smooth","length","quietness")]
school_ebike[1:5] <- lapply(st_drop_geometry(school_ebike[1:5]), as.numeric)
names(school_ebike)[1] = "route_id"
saveRDS(school_ebike,paste0("outputdata/school_ebike_sub30k.Rds"))

