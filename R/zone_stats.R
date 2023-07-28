#' Function to summarise updake data into zone statitics
#' @param comm uptake_list_commute list
#' @param schl uptake_list_school list
#' @param zones sf data frame
#' 
#' the names are appended to column names 
uptake_to_zone_stats <- function(comm, schl, zones){
  
  # Drop Geometry
  comm <- lapply(comm, sf::st_drop_geometry)
  comm <- dplyr::bind_rows(comm, .id = "plan")
  comm <- dplyr::group_by(comm, route_id, plan)
  comm <- dplyr::summarise(comm, 
          geo_code1 = geo_code1[1],
          geo_code2 = geo_code2[1],
          all = all[1],
          from_home = from_home[1],
          train = train[1],
          bus = bus[1],
          car_driver = car_driver[1],
          bicycle = bicycle[1],
          foot = foot[1],
          bicycle_go_dutch = bicycle_go_dutch[1],
          bicycle_increase = bicycle_increase[1],
          car_driver_go_dutch = car_driver_go_dutch[1],
          public_transport = public_transport[1],
          public_transport_go_dutch = public_transport_go_dutch[1],
          foot_go_dutch = foot_go_dutch[1],
          quietness = weighted.mean(quietness, distances),
          route_hilliness = route_hilliness[1]
  )
  
  # School
  schl <- lapply(schl, sf::st_drop_geometry)
  schl <- dplyr::bind_rows(schl, .id = "plan")
  schl <- dplyr::group_by(schl, route_id, plan)
  schl <- dplyr::summarise(schl, 
                           DataZone = DataZone[1],
                           SeedCode = SeedCode[1],
                           all = all[1],
                           #from_home = from_home[1],
                           #train = train[1],
                           #bus = bus[1],
                           #car_driver = car_driver[1],
                           bicycle = bicycle[1],
                           #foot = foot[1],
                           bicycle_go_dutch = bicycle_go_dutch[1],
                           bicycle_increase = bicycle_increase[1],
                           car_driver_go_dutch = car_driver_go_dutch[1],
                           public_transport = public_transport[1],
                           public_transport_go_dutch = public_transport_go_dutch[1],
                           foot_go_dutch = foot_go_dutch[1],
                           quietness = weighted.mean(quietness, distances),
                           route_hilliness = route_hilliness[1]
  )
  
  schl_stats_from <- dplyr::group_by(schl, DataZone, plan)
  schl_stats_from <- dplyr::summarise(schl_stats_from,
                                      all = sum(all, na.rm = TRUE),
                                      bicycle = sum(bicycle, na.rm = TRUE),
                                      bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
                                      bicycle_increase = sum(bicycle_increase, na.rm = TRUE),
                                      car_driver_go_dutch = sum(car_driver_go_dutch, na.rm = TRUE),
                                      public_transport = sum(public_transport, na.rm = TRUE),
                                      public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
                                      foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
                                      quietness = mean(quietness, na.rm = TRUE),
                                      route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )
                                      
                                      
  
}



#' Function to summarise routes into zone statistics
#' @param r sf data frame of routes returned by cyclestreets
#' @param route_type name of route type e.g. fastest, quietest
#' @param route_purose name of route type e.g. commute, school
#' 
#' the names are appended to column names 
routes_to_zone_stats <- function(r, route_type = "fastest", route_purpose = "commute"){
  
  # Drop unneeded data
  r <- sf::st_drop_geometry(r)
  r <- r[,c("route_id","geo_code1","geo_code2",
            "all","from_home","train","bus","car_driver","car_passenger",
            "bicycle","foot","other",
            "bicycle_go_dutch","bicycle_increase",
            "car_driver_go_dutch","car_passenger_go_dutch","public_transport",
            "public_transport_go_dutch","foot_go_dutch","other_go_dutch",
            "quietness","distances","route_hilliness")]
  
  # One row per route
  r <- dplyr::group_by(r, route_id)
  r <- dplyr::summarise(r, 
                        geo_code1 = geo_code1[1],
                        geo_code2 = geo_code2[1],
                        all = all[1],
                        from_home = from_home[1],
                        train = train[1],
                        bus = bus[1],
                        car_driver = car_driver[1],
                        car_passenger = car_passenger[1],
                        bicycle = bicycle[1],
                        foot = foot[1],
                        other = other[1],
                        bicycle_go_dutch = bicycle_go_dutch[1],
                        bicycle_increase = bicycle_increase[1],
                        car_driver_go_dutch = car_driver_go_dutch[1],
                        car_passenger_go_dutch = car_passenger_go_dutch[1],
                        public_transport = public_transport[1],
                        public_transport_go_dutch = public_transport_go_dutch[1],
                        foot_go_dutch = foot_go_dutch[1],
                        other_go_dutch = other_go_dutch[1],
                        quietness = weighted.mean(quietness, distances),
                        route_hilliness = route_hilliness[1]
                        )
  
  # Summarise by origin zone
  stat_origin <- dplyr::group_by(r, geo_code1)
  stat_origin <- dplyr::summarise(stat_origin, 
                        orig_all = round(sum(all)),
                        orig_from_home = round(sum(from_home)),
                        orig_train = round(sum(train)),
                        orig_bus = round(sum(bus)),
                        orig_car_driver = round(sum(car_driver)),
                        orig_car_passenger = round(sum(car_passenger)),
                        orig_bicycle = round(sum(bicycle)),
                        orig_foot = round(sum(foot)),
                        orig_other = round(sum(other)),
                        orig_bicycle_go_dutch = round(sum(bicycle_go_dutch)),
                        orig_bicycle_increase = round(sum(bicycle_increase)),
                        orig_car_driver_go_dutch = round(sum(car_driver_go_dutch)),
                        orig_car_passenger_go_dutch = round(sum(car_passenger_go_dutch)),
                        orig_public_transport = round(sum(public_transport)),
                        orig_public_transport_go_dutch = round(sum(public_transport_go_dutch)),
                        orig_foot_go_dutch = round(sum(foot_go_dutch)),
                        orig_other_go_dutch = round(sum(other_go_dutch)),
                        orig_quietness_mean = round(mean(quietness)),
                        orig_hilliness_mean = round(mean(route_hilliness*100),1)
  )
  names(stat_origin)[1] <- "geo_code"
  
  # Summarise by destination
  stat_dest <- dplyr::group_by(r, geo_code2)
  stat_dest <- dplyr::summarise(stat_dest, 
                                  dest_all = round(sum(all)),
                                  dest_from_home = round(sum(from_home)),
                                  dest_train = round(sum(train)),
                                  dest_bus = round(sum(bus)),
                                  dest_car_driver = round(sum(car_driver)),
                                  dest_car_passenger = round(sum(car_passenger)),
                                  dest_bicycle = round(sum(bicycle)),
                                  dest_foot = round(sum(foot)),
                                  dest_other = round(sum(other)),
                                  dest_bicycle_go_dutch = round(sum(bicycle_go_dutch)),
                                  dest_bicycle_increase = round(sum(bicycle_increase)),
                                  dest_car_driver_go_dutch = round(sum(car_driver_go_dutch)),
                                  dest_car_passenger_go_dutch = round(sum(car_passenger_go_dutch)),
                                  dest_public_transport = round(sum(public_transport)),
                                  dest_public_transport_go_dutch = round(sum(public_transport_go_dutch)),
                                  dest_foot_go_dutch = round(sum(foot_go_dutch)),
                                  dest_other_go_dutch = round(sum(other_go_dutch)),
                                  dest_quietness_mean = round(mean(quietness)),
                                  dest_hilliness_mean = round(mean(route_hilliness*100),1)
  )
  names(stat_dest)[1] <- "geo_code"
  
  # Bind togther
  
  stat_all <- dplyr::left_join(stat_origin, stat_dest, by = "geo_code")
  
  # Update names
  names(stat_all) <- paste0(names(stat_all),"_",route_type,"_",route_purpose)
  names(stat_all)[1] <- "geo_code"
  
  return(stat_all)
  
  
}

#' Function to convert a data.frame into a folder of JSON files
#' @param x data frame with column called geo_code
#' @param path folder to save JSON
#' @param zip logical, if true zips the json into a single zip folder
#' 
#' Will drop any sf geometry and name files based on geo_code

export_zone_json <- function(x,path = "outputdata/json", zip = TRUE){
  
  if(!dir.exists(path)){
    stop("path is not a valid folder")
  }
  
  if(!inherits(x, "data.frame")){
    stop("x is not a data.frame")
  }
  
  if(inherits(x, "sf")){
    x <- sf::st_drop_geometry(x)
  }
  
  x <- split(x, x$geo_code)
  
  if(zip){
    dir.create(file.path(tempdir(),"jsonzip"))
    message("Writing JSON")
    for(i in seq(1,length(x))){
      sub <- x[[i]]
      jsonlite::write_json(sub, file.path(tempdir(),"jsonzip",paste0(sub$geo_code,".json")))
    }
    files <- list.files(file.path(tempdir(),"jsonzip"))
    message("Zipping")
    my_wd <- getwd()
    setwd(file.path(tempdir(),"jsonzip"))
    utils::zip(file.path(my_wd,path,"zipped_json.zip"),files)
    setwd(my_wd)
    unlink(file.path(tempdir(),"jsonzip"), recursive = TRUE)
    
  } else {
    message("Writing JSON")
    for(i in seq(1,length(x))){
      sub <- x[[i]]
      jsonlite::write_json(sub, file.path(path,paste0(sub$geo_code,".json")))
    }
  }
  

  
}




