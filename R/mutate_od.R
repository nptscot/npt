

# Taken a dataset with these columns
mutate_od_commute = function(od) {
  nms = names(od)
  
  # Assumes that car_driver is equivalent with car
  name_car = nms %in% "car_driver"
  if(!"car" %in% nms && any(name_car)) {
    if("taxi" %in% nms) {
      od[[which(name_car)]] = od[[which(name_car)]] + od$taxi
      od$taxi = NULL
    }
    names(od)[name_car] = "car"
  }
  
  name_pt = "bus" %in% nms && "train" %in% nms && "underground" %in% nms
  if(!"public_transport" %in% nms && name_pt) {
    od$public_transport = od$bus + od$train + od$underground
    od$bus = NULL
    od$train = NULL
    od$underground = NULL
  }
  
  if(!"foot" %in% nms && "walk" %in% nms) {
    od$foot = od$walk
    od$walk = NULL
  }
  
  # Remove unwanted columns
  od$no_fixed_place = NULL
  od$from_home = NULL
  
  od
  
}

# mutate_od_commute(readr::read_csv("data-raw/od_data_dz_synthetic.csv"))


# School data 

# flow_mode <- flow_mode[,c("SEED","type","walk","bicycle","public_transport","car","taxi","other")]
# names(flow_mode)[names(flow_mode) == "walk"] = "foot"
# 
# flow_jitter$schooltype <- tolower(flow_jitter$schooltype)
# flow_jitter = left_join(flow_jitter, flow_mode, by = c("SeedCode" = "SEED", "schooltype" = "type"))

# od = sf::read_sf("data-raw/school_desire_lines_open.geojson")
# od = schools_dl
# summary(schools_dl)
# od$walk = od$count * 0.5
# od$public_transport * 0.1
mutate_od_school = function(od) {
  nms = names(od)
  
  name_car = nms %in% "car"
  if("taxi" %in% nms) {
    od[[which(name_car)]] = od[[which(name_car)]] + od$taxi
    od$taxi = NULL
  }
  
  if(!"foot" %in% nms && "walk" %in% nms) {
    od$foot = od$walk
    od$walk = NULL
  }
  
  od
  
}