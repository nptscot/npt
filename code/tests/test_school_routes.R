





#flow_nocount = flow_sf[,c("route_number")]

source("R/get_routes.R")

# Works for first 250, but fails on larger batches
# routes_school = get_routes(od = flow_nocount,
#                             plans = c("fastest","quietest","balanced","ebike"), 
#                            purpose = "school",
#                             folder = "outputdata", 
#                            batch = TRUE, nrow_batch = 20000)

# flow_nodup = flow_nocount[!duplicated(flow_nocount$geometry),]
# flow_dup = flow_nocount[duplicated(flow_nocount$geometry),]
# flow_dup$match_id <- flow_nodup$route_number[match(flow_dup$geometry, flow_nodup$geometry)]


# Import school flows
school_path = file.path(secure_path, "secure_data/schools/school_dl_sub30km.Rds")
flow_sf = readRDS(school_path)

# Smaller sample
flow_sample = sample_frac(flow_sf, size = 0.01)
flow_sf = flow_sample


fromPlace = st_sf(lwgeom::st_startpoint(flow_sf))
toPlace = st_sf(lwgeom::st_endpoint(flow_sf))

# school_fast_batch = cyclestreets::journey2(fromPlace = fromPlace[1:500,], 
#                                            toPlace = toPlace[1:500,], 
#                                            id = flow_sf$route_number[1:500], 
#                                            plan = "fastest",
#                                            pat = Sys.getenv("CYCLESTREETS"),
#                                            host_con = 10,
#                                            segments = FALSE)
# 
# 
# qtm(school_fast_batch["id"]) + qtm(flow_sf[1:500,"route_number"], lines.col = "red")

# Batches of 10,000
ids <- 1:nrow(flow_sf)
ids <- split(ids, ceiling(seq_along(ids)/10000))

for(i in 1:length(ids)){
  message(i)
  school_fast_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                             toPlace = toPlace[ids[[i]],], 
                                             id = flow_sf$route_number[ids[[i]]], 
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
school_fast_missing <- flow_sf[!flow_sf$route_number %in% school_fast$id,]
school_fast_missing = cyclestreets::journey2(fromPlace = st_sf(lwgeom::st_startpoint(school_fast_missing)), 
                                             toPlace = st_sf(lwgeom::st_endpoint(school_fast_missing)), 
                                             id = school_fast_missing$route_number, 
                                             plan = "fastest",
                                             pat = Sys.getenv("CYCLESTREETS"),
                                             host_con = 1,
                                             segments = TRUE)
school_fast <- rbind(school_fast, school_fast_missing)
school_fast <- school_fast[,c("id","distance","gradient_smooth","length","quietness")]
school_fast[1:5] <- lapply(st_drop_geometry(school_fast[1:5]), as.numeric)
names(school_fast)[1] = "route_number"
saveRDS(school_fast,paste0("outputdata/school_fast_sub30k.Rds"))



# Quiet

for(i in 1:length(ids)){
  message(i)
  school_quiet_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                              toPlace = toPlace[ids[[i]],], 
                                              id = flow_sf$route_number[ids[[i]]], 
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
school_quiet_missing <- flow_sf[!flow_sf$route_number %in% school_quiet$id,]
fromPlace = st_sf(lwgeom::st_startpoint(school_quiet_missing))
toPlace = st_sf(lwgeom::st_endpoint(school_quiet_missing))
school_quiet_missing = cyclestreets::journey2(fromPlace = fromPlace, 
                                              toPlace = toPlace, 
                                              id = school_quiet_missing$route_number, 
                                              plan = "quietest",
                                              pat = Sys.getenv("CYCLESTREETS"),
                                              host_con = 1,
                                              segments = TRUE)
school_quiet <- rbind(school_quiet, school_quiet_missing)
school_quiet <- school_quiet[,c("id","distance","gradient_smooth","length","quietness")]
school_quiet[1:5] <- lapply(st_drop_geometry(school_quiet[1:5]), as.numeric)
names(school_quiet)[1] = "route_number"
saveRDS(school_quiet,paste0("outputdata/school_quietest_sub30k.Rds"))


# Balanced

for(i in 1:length(ids)){
  message(i)
  school_balance_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                                toPlace = toPlace[ids[[i]],], 
                                                id = flow_sf$route_number[ids[[i]]], 
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
school_balanced_missing <- flow_sf[!flow_sf$route_number %in% school_balanced$id,]
school_balanced_missing = cyclestreets::journey2(fromPlace = st_sf(lwgeom::st_startpoint(school_balanced_missing)), 
                                                 toPlace = st_sf(lwgeom::st_endpoint(school_balanced_missing)), 
                                                 id = school_balanced_missing$route_number, 
                                                 plan = "balanced",
                                                 pat = Sys.getenv("CYCLESTREETS"),
                                                 host_con = 1,
                                                 segments = TRUE)
school_balanced <- rbind(school_balanced, school_balanced_missing)
school_balanced <- school_balanced[,c("id","distance","gradient_smooth","length","quietness")]
school_balanced[1:5] <- lapply(st_drop_geometry(school_balanced[1:5]), as.numeric)
names(school_balanced)[1] = "route_number"
saveRDS(school_balanced,paste0("outputdata/school_balanced_sub30k.Rds"))

# Ebike


for(i in 1:length(ids)){
  message(i)
  school_ebike_batch = cyclestreets::journey2(fromPlace = fromPlace[ids[[i]],], 
                                              toPlace = toPlace[ids[[i]],], 
                                              id = flow_sf$route_number[ids[[i]]], 
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
school_ebike_missing <- flow_sf[!flow_sf$route_number %in% school_ebike$id,]
school_ebike_missing = cyclestreets::journey2(fromPlace = st_sf(lwgeom::st_startpoint(school_ebike_missing)), 
                                              toPlace = st_sf(lwgeom::st_endpoint(school_ebike_missing)), 
                                              id = school_ebike_missing$route_number, 
                                              plan = "ebike",
                                              pat = Sys.getenv("CYCLESTREETS"),
                                              host_con = 1,
                                              segments = TRUE)
school_ebike <- rbind(school_ebike, school_ebike_missing)
school_ebike <- school_ebike[,c("id","distance","gradient_smooth","length","quietness")]
school_ebike[1:5] <- lapply(st_drop_geometry(school_ebike[1:5]), as.numeric)
names(school_ebike)[1] = "route_number"
saveRDS(school_ebike,paste0("outputdata/school_ebike_sub30k.Rds"))

