# Prep School Locations
library(sf)
library(tmap)
library(stplanr)
library(dplyr)
tmap_mode("view")

secure_path = Sys.getenv("NPT_TEAMS_PATH")

if(!file.exists("./inputdata/Schools/school_locations.geojson")){
  dir.create("temp")
  unzip("D:/GitHub/atumscot/inputdata/Schools/SG_SchoolRoll_2022.zip", exdir = "temp")
  locs = read_sf("temp/SG_SchoolRoll_2022/SG_SchoolRoll_2022.shp")
  unlink("temp", recursive = TRUE)
  #qtm(locs)
  locs = locs[,c("SeedCode","SchoolType","SchoolName")]
  locs = st_transform(locs, 4326)
  
  locs_extra = read.csv("./inputdata/Schools/school_locations_extra.csv")
  locs_extra = st_as_sf(locs_extra, coords = c("lng","lat"), crs = 4326)
  names(locs_extra) = names(locs)
  
  locs = rbind(locs, locs_extra)
  
  # Jitter overlapping schools
  locs_dup = locs[duplicated(locs$geometry),]
  locs = locs[!duplicated(locs$geometry),]
  jitter_points = function(x){
    x[1] <- x[1] + runif(1,-0.0005,0.0005)
    x[2] <- x[2] + runif(1,-0.0005,0.0005)
    x
  }
  
  locs_dup$geometry = lapply(locs_dup$geometry, jitter_points)
  locs_dup$geometry = sf::st_sfc(locs_dup$geometry, crs = 4326)
  
  st_precision(locs) <- 1000000
  st_write(locs, "./inputdata/Schools/school_locations.geojson")
} else {
  locs <- st_read("./inputdata/Schools/school_locations.geojson")
}

# Prep School Flows if file not already present
flow = readxl::read_excel(file.path(secure_path,"secure_data/schools/raw_data/A2200 pupils by school and data zone.xlsx"))
names(flow)[5] = "DataZone"

if(!file.exists("./inputdata/data_zone_centroids.geojson")){
  dir.create("temp")
  unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_DataZoneCent_2011.zip", exdir = "temp")
  datazone_cent = read_sf("temp/SG_DataZone_Cent_2011.shp")
  unlink("temp", recursive = TRUE)
  #qtm(locs)
  datazone_cent = datazone_cent[,c("DataZone")]
  datazone_cent = st_transform(datazone_cent, 4326)
  
  st_precision(datazone_cent) <- 100000
  st_write(datazone_cent, "./inputdata/data_zone_centroids.geojson")
} else {
  datazone_cent <- st_read("./inputdata/data_zone_centroids.geojson")
}


summary(flow$SeedCode %in% locs$SeedCode)
foo = flow[!flow$SeedCode %in% locs$SeedCode,] # 7 missing schools added in locs_extra, some are closed
flow = flow[flow$SeedCode %in% locs$SeedCode,]

summary(flow$DataZone %in% datazone_cent$DataZone)

# Jitter OD
flow = flow[,c("DataZone","SeedCode","count","LaCode","schooltype","SchlName")]

# Data Zones
dir.create(file.path(tempdir(),"SIMD"))
unzip("./inputdata/SIMD/simd2020_withgeog.zip",
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
  show_command = TRUE,
  deduplicate_pairs = FALSE
)

# TODO: De duplicate lines, reduces routing

flow_jitter$length_km <- round(as.numeric(st_length(flow_jitter)) / 1000,1)
summary(flow_jitter$length_km)

#TODO: excluding long trips distorts overal mode share (edge case but should be considered)
flow_jitter <- flow_jitter[flow_jitter$length_km < 30,]
flow_jitter <- flow_jitter[,c("DataZone","SeedCode","schooltype","count")]
flow_jitter$route_number <- 1:nrow(flow_jitter)
flow_jitter$count <- round(flow_jitter$count)

# Add on mode shares
flow_jitter$lenght_km_str <- as.numeric(st_length(flow_jitter)) / 1000

flow_mode <- readRDS(file.path(secure_path,"secure_data/schools/school_mode_split.Rds"))

# Edge case two primary schools same location, but very little data for Gaelic school, so use main school modeshare
flow_mode <- flow_mode[flow_mode$school_name != "Gilcomstoun Primary School Gaelic",]

flow_mode <- flow_mode[,c("SEED","type","walk","bicycle","public_transport","car","taxi","other")]
names(flow_mode)[names(flow_mode) == "walk"] = "foot"

flow_jitter$schooltype <- tolower(flow_jitter$schooltype)
flow_jitter = left_join(flow_jitter, flow_mode, by = c("SeedCode" = "SEED", "schooltype" = "type"))

# Fill in data for missing schools
flow_jitter$foot <- ifelse(is.na(flow_jitter$foot),0.44, flow_jitter$foot)
flow_jitter$bicycle <- ifelse(is.na(flow_jitter$bicycle),0.03, flow_jitter$bicycle)

flow_jitter$public_transport <- ifelse(is.na(flow_jitter$public_transport),0.06, flow_jitter$public_transport)
flow_jitter$car <- ifelse(is.na(flow_jitter$car),0.32, flow_jitter$car)
flow_jitter$taxi <- ifelse(is.na(flow_jitter$taxi),0.005, flow_jitter$taxi)

flow_jitter$other <- ifelse(is.na(flow_jitter$other),1 - flow_jitter$foot - 
                              flow_jitter$bicycle - flow_jitter$public_transport - flow_jitter$car - flow_jitter$taxi, flow_jitter$other)

summary(flow_jitter)

flow_split = group_by(flow_jitter, SeedCode, schooltype)
flow_split = group_split(flow_split)
#flow_split = split(flow_jitter, flow_jitter$SeedCode)

#x = flow_split[[(1:length(flow_split))[names(flow_split) == "5553024"]]]

random_integers <- function(total_value, num_elements, max_value, seed) {
  #message(total_value, num_elements)
  partition <- sample(0:(total_value - 1), num_elements - 1, replace = TRUE)
  partition <- sort(partition)
  partition <- c(partition, total_value) - c(0, partition)
  if(any(partition > max_value)){
    partition <- partition[order(partition, decreasing = TRUE)]
  }
  if(any(partition > max_value)){
    diff <- partition - max_value
    change_down <- ifelse(diff > 0,diff,0)
    change_down_total = sum(change_down)
    change_up <- ifelse(diff < 0,-diff,0)
    partition = partition - change_down
    
    for(i in seq_along(partition)){
      if(change_up[i] > 0){
        if(change_up[i] >= change_down_total){
          partition[i] = partition[i] + change_down_total
          break
        } else {
          partition[i] = partition[i] + change_up[i]
          change_down_total = change_down_total - change_up[i]
        }
      }
    }
    if(any(partition > max_value)){
      stop(seed)
      #message("Try again t=",total_value," n=",num_elements," max=",paste(max_value,collapse = ","))
      #partition <- random_integers(total_value, num_elements, max_value)
    }
    
  }
  return(partition)
}



distribute_cycling <- function(x){
  mode_foot = x$foot[1]
  mode_bicycle = x$bicycle[1]
  mode_public_transport = x$public_transport[1]
  mode_car = x$car[1]
  mode_taxi = x$taxi[1]
  mode_other = x$other[1]
  
  n_bicycle = round(mode_bicycle * sum(x$count))
  n_foot = round(mode_foot * sum(x$count))
  n_public_transport = round(mode_public_transport * sum(x$count))
  n_car = round(mode_car * sum(x$count))
  n_taxi = round(mode_taxi * sum(x$count))
  n_other = round(mode_other * sum(x$count))
  
  
  #TODO: WHY???
  if(n_bicycle != 0){
    # Only allocated to less than 4km, unless need more routes to meet demand
    x = x[order(x$lenght_km_str),]
    x$count_cumsum <- cumsum(x$count)
    if(nrow(x) == 1){
      x$bicycle_alocated <- TRUE
    } else {
      x$bicycle_alocated <- ifelse(x$lenght_km_str < 3 | c(0,x$count_cumsum[seq(1, nrow(x) - 1)]) <  n_bicycle, TRUE, FALSE)
    }
    
    # Check if no route allocated
    if(sum(x$bicycle_alocated) == 0){
      x$bicycle_alocated[1] = TRUE
    }
    
    x_bike = x[x$bicycle_alocated,]
    x = x[!x$bicycle_alocated,]
    
    #Pick a random number around average cyclists per route
    x_bike$n_bicycle = random_integers(total_value = n_bicycle, 
                                       num_elements = nrow(x_bike), 
                                       max_value = x_bike$count, 
                                       seed = x$SeedCode[1])
    
    if(sum(x_bike$n_bicycle) != n_bicycle){
      stop("3 Wrong number of bikes, SEED = ",x$SeedCode[1]," ",sum(x_bike$n_bicycle)," ",n_bicycle)
    }
    
    x_bike$bicycle = x_bike$n_bicycle
    x$bicycle = 0
    
    x_bike = x_bike[names(x)]
    x = rbind(x,x_bike)
  } else {
    x$bicycle = 0
  }
  
  
  
  
  # Give out other modes randomly
  
  extra_modes = distribute_mode(n_total = x$count - x$bicycle, 
                        n_foot = n_foot, 
                        n_public_transport = n_public_transport, 
                        n_car = n_car, 
                        n_taxi = n_taxi, 
                        n_other = n_other)
  
  x = cbind(x[!names(x) %in% names(extra_modes)], extra_modes)
  
  x$bicycle_alocated = NULL
  x$count_cumsum = NULL
  
  return(x)
  
}

distribute_mode = function(n_total, n_foot, n_public_transport, n_car, n_taxi, n_other){
  
 nms = c(rep("foot", n_foot),
         rep("public_transport", n_public_transport),
         rep("car", n_car),
         rep("taxi", n_taxi),
         rep("other", n_other))
 
 nms = sample(nms, length(nms), replace = FALSE)
 
 if(length(nms) != sum(n_total)){
   if(length(nms) > sum(n_total)){
     nms = nms[seq(1, sum(n_total))]
   } else {
     nms = c(nms, rep("other",sum(n_total) - length(nms)))
   }
 }
 
 
 
 group_index <- rep(1:length(n_total), times = n_total)
 grouped_data <- split(nms, group_index)
 
 names(grouped_data) <- seq_len(length(n_total))[n_total != 0]
 
 #Add any 0s
 if(sum(n_total == 0) > 0){
   grouped_data_0 <- vector("list", length = sum(n_total == 0))
   names(grouped_data_0) <- seq_len(length(n_total))[n_total == 0]
   
   grouped_data = c(grouped_data,grouped_data_0)
   grouped_data = grouped_data[order(as.numeric(names(grouped_data)))]
   
 }
 
 extracols = data.frame(foot = 0,
                        public_transport = 0,
                        car = 0,
                        taxi = 0,
                        other = 0)
 
 # Make into data.frame
 res = list()
 
 for(i in seq_along(grouped_data)){
   sub = grouped_data[[i]]
   if(is.null(sub)){
     res[[i]] = extracols
   } else {
     sub = table(sub)
     
     count_df <- data.frame(matrix(sub, ncol = length(sub), byrow = TRUE))
     colnames(count_df) <- names(sub)
     
     count_df = cbind(count_df, extracols[!names(extracols) %in% names(count_df)])
     res[[i]] = count_df
   }
   
   
 }
 
 res = dplyr::bind_rows(res)
return(res)
  
}


flow_bicycle = pbapply::pblapply(flow_split, distribute_cycling)
flow_bicycle = bind_rows(flow_bicycle)

flow_bicycle$lenght_km_str = NULL

flow_bicycle$schooltype <- gsub("special ","",flow_bicycle$schooltype)

names(flow_bicycle)[names(flow_bicycle) == "count"] <- "all"

saveRDS(flow_bicycle, file.path(secure_path,"secure_data/schools/school_dl_sub30km.Rds"))


