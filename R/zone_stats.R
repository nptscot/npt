#' Function to summarise uptake data into zone
#' @param comm uptake_list_commute list
#' @param schl uptake_list_school list
#' @param zones sf data frame
#' 
#' the names are appended to column names 
uptake_to_zone_stats <- function(comm, schl, zones){
  
  #comm = uptake_list_commute
  #schl = uptake_list_school
  
  comm <- dplyr::bind_rows(comm, .id = "plan")
  comm <- dplyr::group_by(comm, route_id, plan)
  # Go to one row per route
  comm <- dplyr::summarise(comm, 
          geo_code1 = geo_code1[1],
          geo_code2 = geo_code2[1],
          all = all[1],
          car = car[1],
          bicycle = bicycle[1],
          foot = foot[1],
          public_transport = public_transport[1],
          
          bicycle_go_dutch = bicycle_go_dutch[1],
          car_go_dutch = car_go_dutch[1],
          public_transport_go_dutch = public_transport_go_dutch[1],
          foot_go_dutch = foot_go_dutch[1],
          
          bicycle_ebike = bicycle_ebike[1],
          car_ebike = car_ebike[1],
          public_transport_ebike = public_transport_ebike[1],
          foot_ebike = foot_ebike[1],
          
          quietness = weighted.mean(quietness, distances),
          route_hilliness = route_hilliness[1]
  )
  
  # Commute Origin Stats
  comm_from <- dplyr::group_by(comm, geo_code1, plan)
  comm_from <- dplyr::summarise(comm_from, 
                                orig_all = sum(all, na.rm = TRUE),
                                orig_car = sum(car, na.rm = TRUE),
                                orig_bicycle = sum(bicycle, na.rm = TRUE),
                                orig_foot = sum(foot, na.rm = TRUE),
                                orig_public_transport = sum(public_transport, na.rm = TRUE),
                                
                                orig_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
                                orig_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
                                orig_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
                                orig_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
                                
                                orig_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
                                orig_car_ebike = sum(car_ebike, na.rm = TRUE),
                                orig_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
                                orig_foot_ebike = sum(foot_ebike, na.rm = TRUE),
                                
                                orig_quietness = mean(quietness),
                                orig_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )
  comm_from <- tidyr::pivot_wider(comm_from, 
                                  id_cols = c("geo_code1","orig_all", 
                                              "orig_car", 
                                              "orig_bicycle", "orig_foot","orig_public_transport"),
                                  names_from = "plan",
                                  values_from = c("orig_bicycle_go_dutch",
                                                  "orig_car_go_dutch",
                                                  "orig_public_transport_go_dutch",
                                                  "orig_foot_go_dutch",
                                                  "orig_bicycle_ebike",
                                                  "orig_car_ebike",
                                                  "orig_public_transport_ebike",
                                                  "orig_foot_ebike",
                                                  "orig_quietness",
                                                  "orig_route_hilliness")
                                  )
  
  
  comm_to <- dplyr::group_by(comm, geo_code2, plan)
  comm_to <- dplyr::summarise(comm_to, 
                                dest_all = sum(all, na.rm = TRUE),
                                dest_car = sum(car, na.rm = TRUE),
                                dest_bicycle = sum(bicycle, na.rm = TRUE),
                                dest_foot = sum(foot, na.rm = TRUE),
                                dest_public_transport = sum(public_transport, na.rm = TRUE),
                              
                                dest_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
                                dest_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
                                dest_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
                                dest_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
                              
                                dest_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
                                dest_car_ebike = sum(car_ebike, na.rm = TRUE),
                                dest_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
                                dest_foot_ebike = sum(foot_ebike, na.rm = TRUE),
                              
                                dest_quietness = mean(quietness),
                                dest_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )
  
  comm_to <- tidyr::pivot_wider(comm_to, 
                                  id_cols = c("geo_code2","dest_all", 
                                              "dest_car", 
                                              "dest_bicycle", "dest_foot","dest_public_transport"),
                                  names_from = "plan",
                                  values_from = c("dest_bicycle_go_dutch",
                                                  "dest_car_go_dutch",
                                                  "dest_public_transport_go_dutch",
                                                  "dest_foot_go_dutch",
                                                  "dest_bicycle_ebike",
                                                  "dest_car_ebike",
                                                  "dest_public_transport_ebike",
                                                  "dest_foot_ebike",
                                                  "dest_quietness",
                                                  "dest_route_hilliness")
  )
  
  
  schl <- lapply(schl, sf::st_drop_geometry)
  schl <- dplyr::bind_rows(schl, .id = "plan")
  schl <- dplyr::group_by(schl, route_id, plan, schooltype)
  # Go to one row per route
  schl <- dplyr::summarise(schl, 
                           DataZone = DataZone[1],
                           SeedCode = SeedCode[1],
                           all = all[1],
                           bicycle = bicycle[1],
                           public_transport = public_transport[1],
                           
                           bicycle_go_dutch = bicycle_go_dutch[1],
                           car_go_dutch = car_go_dutch[1],
                           public_transport_go_dutch = public_transport_go_dutch[1],
                           foot_go_dutch = foot_go_dutch[1],
                           
                           bicycle_ebike = bicycle_ebike[1],
                           car_ebike = car_ebike[1],
                           public_transport_ebike = public_transport_ebike[1],
                           foot_ebike = foot_ebike[1],
                           
                           quietness = weighted.mean(quietness, distances),
                           route_hilliness = route_hilliness[1]
  )
  
  schl_from <- dplyr::group_by(schl, DataZone, plan, schooltype)
  schl_from <- dplyr::summarise(schl_from,
                                orig_all = sum(all, na.rm = TRUE),
                                orig_bicycle = sum(bicycle, na.rm = TRUE),
                                orig_public_transport = sum(public_transport, na.rm = TRUE),
                                
                                orig_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
                                orig_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
                                orig_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
                                orig_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
                                
                                orig_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
                                orig_car_ebike = sum(car_ebike, na.rm = TRUE),
                                orig_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
                                orig_foot_ebike = sum(foot_ebike, na.rm = TRUE),
                                
                                orig_quietness = mean(quietness, na.rm = TRUE),
                                orig_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )
  
  schl_from <- tidyr::pivot_wider(schl_from, 
                                  id_cols = c("DataZone",),
                                  names_from = c("schooltype","plan"),
                                  values_from = c("orig_all",
                                                  "orig_bicycle",
                                                  
                                                  "orig_bicycle_go_dutch",
                                                  "orig_car_go_dutch",
                                                  "orig_public_transport_go_dutch",
                                                  "orig_foot_go_dutch",
                                                  
                                                  "orig_bicycle_ebike",
                                                  "orig_car_ebike",
                                                  "orig_public_transport_ebike",
                                                  "orig_foot_ebike",
                                                  
                                                  "orig_quietness",
                                                  "orig_route_hilliness")
  )
  
  
  schl_to <- dplyr::group_by(schl, SeedCode, plan, schooltype)
  schl_to <- dplyr::summarise(schl_to,
                                all = sum(all, na.rm = TRUE),
                                bicycle = sum(bicycle, na.rm = TRUE),
                                public_transport = sum(public_transport, na.rm = TRUE),
                                
                                bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
                                car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
                                public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
                                foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
                              
                                bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
                                car_ebike = sum(car_ebike, na.rm = TRUE),
                                public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
                                foot_ebike = sum(foot_ebike, na.rm = TRUE),
                              
                                quietness = mean(quietness, na.rm = TRUE),
                                route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )
  
  schl_to <- dplyr::ungroup(schl_to)
  
  schl_to <- tidyr::pivot_wider(schl_to, 
                                  id_cols = c("SeedCode"),
                                  names_from = c("schooltype","plan"),
                                  values_from = c("all",
                                                  "bicycle","public_transport",
                                                  
                                                  "bicycle_go_dutch",
                                                  "car_go_dutch",
                                                  "public_transport_go_dutch",
                                                  "foot_go_dutch",
                                                  
                                                  "bicycle_ebike",
                                                  "car_ebike",
                                                  "public_transport_ebike",
                                                  "foot_ebike",
                                                  "quietness",
                                                  "route_hilliness")
  )
  
  
  # Combine Commute Origin/Dest and School Origin into a single DataZone Dataset
  names(comm_from) <- paste0("comm_", names(comm_from))
  names(comm_to) <- paste0("comm_", names(comm_to))
  names(schl_from) <- paste0("schl_", names(schl_from))
  zones2 <- dplyr::left_join(zones, comm_from, by = c("DataZone" = "comm_geo_code1"))
  zones2 <- dplyr::left_join(zones2, comm_to, by = c("DataZone" = "comm_geo_code2"))
  zones2 <- dplyr::left_join(zones2, schl_from, by = c("DataZone" = "schl_DataZone"))
  
  return(list(zones = zones2, schools = schl_to))
  
                                      
  
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
            "all","car","car_passenger",
            "bicycle","foot","other",
            "bicycle_go_dutch",
            "car_go_dutch","public_transport",
            "public_transport_go_dutch","foot_go_dutch","other_go_dutch",
            "quietness","distances","route_hilliness")]
  
  # One row per route
  r <- dplyr::group_by(r, route_id)
  r <- dplyr::summarise(r, 
                        geo_code1 = geo_code1[1],
                        geo_code2 = geo_code2[1],
                        all = all[1],
                        car = car[1],
                        bicycle = bicycle[1],
                        foot = foot[1],
                        other = other[1],
                        bicycle_go_dutch = bicycle_go_dutch[1],
                        car_go_dutch = car_go_dutch[1],
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
                        orig_car = round(sum(car)),
                        orig_bicycle = round(sum(bicycle)),
                        orig_foot = round(sum(foot)),
                        orig_other = round(sum(other)),
                        orig_bicycle_go_dutch = round(sum(bicycle_go_dutch)),
                        orig_car_go_dutch = round(sum(car_go_dutch)),
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
                                  dest_car = round(sum(car)),
                                  dest_car_passenger = round(sum(car_passenger)),
                                  dest_bicycle = round(sum(bicycle)),
                                  dest_foot = round(sum(foot)),
                                  dest_other = round(sum(other)),
                                  dest_bicycle_go_dutch = round(sum(bicycle_go_dutch)),
                                  dest_car_go_dutch = round(sum(car_go_dutch)),
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
#' @parm idcol name of column with unique id
#' @param path folder to save JSON
#' @param zip logical, if true zips the json into a single zip folder
#' 
#' Will drop any sf geometry and name files based on geo_code

export_zone_json <- function(x,  idcol = "DataZone", path = "outputdata/json", zip = TRUE){
  
  if(!dir.exists(path)){
    if(dir.exists("outputdata")) {
      dir.create(path)
    } else {
      stop("path is not a valid folder")
    }
  }
  
  if(!inherits(x, "data.frame")){
    stop("x is not a data.frame")
  }
  
  if(inherits(x, "sf")){
    x <- sf::st_drop_geometry(x)
  }
  
  x <- dplyr::group_split(x, x[idcol])
  
  if(zip){
    dir.create(file.path(tempdir(),"jsonzip"))
    message("Writing JSON")
    for(i in seq(1,length(x))){
      sub <- x[[i]]
      jsonlite::write_json(sub, file.path(tempdir(),"jsonzip",paste0(sub[idcol],".json")))
    }
    files <- list.files(file.path(tempdir(),"jsonzip"))
    message("Zipping JSON")
    my_wd <- getwd()
    setwd(file.path(tempdir(),"jsonzip"))
    utils::zip(file.path(my_wd,path,paste0(idcol,"_json.zip")),files, flags="-q")
    setwd(my_wd)
    unlink(file.path(tempdir(),"jsonzip"), recursive = TRUE)
    
    return(file.path(my_wd,path,paste0(idcol,"_json.zip")))
    
  } else {
    message("Writing JSON")
    for(i in seq(1,length(x))){
      sub <- x[[i]]
      jsonlite::write_json(sub, file.path(path,paste0(sub[idcol],".json")))
    }
  }
  
  return(path)
  
}




