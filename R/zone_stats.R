#' Function to summarise uptake data into zone
#' @param comm uptake_list_commute list
#' @param nm name to append to columns
#'
#' the names are appended to column names
make_commute_stats = function(comm, nm) {
  # Drop Geometry
  comm = sf::st_drop_geometry(comm)

  comm$quietness = as.numeric(comm$quietness)

  # Commute Origin Stats
  comm_from = dplyr::group_by(comm, geo_code1)
  comm_from = dplyr::summarise(comm_from,
    orig_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
    orig_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
    orig_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
    orig_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
    orig_taxi_go_dutch = sum(taxi_go_dutch, na.rm = TRUE),
    orig_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
    orig_car_ebike = sum(car_ebike, na.rm = TRUE),
    orig_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
    orig_foot_ebike = sum(foot_ebike, na.rm = TRUE),
    orig_taxi_ebike = sum(taxi_ebike, na.rm = TRUE),
    orig_quietness = mean(quietness),
    orig_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )


  comm_to = dplyr::group_by(comm, geo_code2)
  comm_to = dplyr::summarise(comm_to,
    dest_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
    dest_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
    dest_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
    dest_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
    dest_taxi_go_dutch = sum(taxi_go_dutch, na.rm = TRUE),
    dest_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
    dest_car_ebike = sum(car_ebike, na.rm = TRUE),
    dest_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
    dest_foot_ebike = sum(foot_ebike, na.rm = TRUE),
    dest_taxi_ebike = sum(taxi_ebike, na.rm = TRUE),
    dest_quietness = mean(quietness),
    dest_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )



  # Combine Commute Origin/Dest and School Origin into a single DataZone Dataset
  names(comm_from) = paste0("comm_", names(comm_from))
  names(comm_to) = paste0("comm_", names(comm_to))

  names(comm_from)[1] = "DataZone"
  names(comm_to)[1] = "DataZone"

  comm = dplyr::full_join(comm_from, comm_to, by = "DataZone")

  names(comm)[2:ncol(comm)] = paste0(names(comm)[2:ncol(comm)], "_", nm)

  return(comm)
}



#' Function to summarise uptake data to schools
#' @param schl uptake_list_school list
#' @param nm name to append to columns
#'
make_school_stats = function(schl, nm) {
  # School
  schl = sf::st_drop_geometry(schl)
  schl = dplyr::group_by(schl, route_number, schooltype)

  schl$quietness = as.numeric(schl$quietness)

  schl_to = dplyr::group_by(schl, SeedCode, schooltype)
  schl_to = dplyr::summarise(schl_to,
    bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
    car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
    public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
    foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
    other_go_dutch = sum(other_go_dutch, na.rm = TRUE),
    bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
    car_ebike = sum(car_ebike, na.rm = TRUE),
    public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
    foot_ebike = sum(foot_ebike, na.rm = TRUE),
    other_ebike = sum(other_ebike, na.rm = TRUE),
    quietness = mean(quietness, na.rm = TRUE),
    route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )

  schl_to = dplyr::ungroup(schl_to)
  schl_to$schooltype = paste0("schl_", schl_to$schooltype, "_dest")

  schl_to = tidyr::pivot_wider(schl_to,
    id_cols = c("SeedCode"),
    names_from = c("schooltype"),
    values_from = c(
      "bicycle_go_dutch",
      "car_go_dutch",
      "public_transport_go_dutch",
      "foot_go_dutch",
      "other_go_dutch",
      "bicycle_ebike",
      "car_ebike",
      "public_transport_ebike",
      "foot_ebike",
      "other_ebike",
      "quietness",
      "route_hilliness"
    ),
    names_glue = "{schooltype}_{.value}"
  )
  names(schl_to)[2:ncol(schl_to)] = paste0(names(schl_to)[2:ncol(schl_to)], "_", nm)
  return(schl_to)
}

#' Function to summarise uptake data from schools (i.e. Datazones)
#' @param schl uptake_list_school list
#' @param nm name to append to columns
#'
make_school_stats_from = function(schl, nm) {
  # School
  schl = sf::st_drop_geometry(schl)

  schl_from = dplyr::group_by(schl, DataZone, schooltype)
  schl_from = dplyr::summarise(schl_from,
    orig_all = sum(all, na.rm = TRUE),
    orig_bicycle = sum(bicycle, na.rm = TRUE),
    orig_public_transport = sum(public_transport, na.rm = TRUE),
    orig_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
    orig_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
    orig_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
    orig_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
    orig_other_go_dutch = sum(other_go_dutch, na.rm = TRUE),
    orig_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
    orig_car_ebike = sum(car_ebike, na.rm = TRUE),
    orig_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
    orig_foot_ebike = sum(foot_ebike, na.rm = TRUE),
    orig_other_ebike = sum(other_ebike, na.rm = TRUE),
    orig_quietness = mean(quietness, na.rm = TRUE),
    orig_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )

  schl_from = schl_from[!schl_from$schooltype == "unknown", ]
  schl_from = tidyr::pivot_wider(schl_from,
    id_cols = "DataZone",
    names_from = "schooltype",
    values_from = c(
      "orig_bicycle_go_dutch",
      "orig_car_go_dutch",
      "orig_public_transport_go_dutch",
      "orig_foot_go_dutch",
      "orig_other_go_dutch",
      "orig_bicycle_ebike",
      "orig_car_ebike",
      "orig_public_transport_ebike",
      "orig_foot_ebike",
      "orig_other_ebike",
      "orig_quietness",
      "orig_route_hilliness"
    ),
    names_glue = "{schooltype}_{.value}"
  )

  names(schl_from)[2:ncol(schl_from)] = paste0("schl_", names(schl_from)[2:ncol(schl_from)])
  names(schl_from)[2:ncol(schl_from)] = paste0(names(schl_from)[2:ncol(schl_from)], "_", nm)
  return(schl_from)
}

#' Function to convert a data.frame into a folder of JSON files
#' @param x data frame with column called geo_code
#' @parm idcol name of column with unique id
#' @param path folder to save JSON
#' @param zip logical, if true zips the json into a single zip folder
#'
#' Will drop any sf geometry and name files based on geo_code

export_zone_json = function(x, idcol = "DataZone", path = "outputdata/json", zip = TRUE) {
  if (!dir.exists(path)) {
    if (dir.exists("outputdata")) {
      dir.create(path)
    } else {
      stop("path is not a valid folder")
    }
  }

  if (!inherits(x, "data.frame")) {
    stop("x is not a data.frame")
  }

  if (inherits(x, "sf")) {
    x = sf::st_drop_geometry(x)
  }

  if (!inherits(x, "tibble")) {
    x = as.data.frame(x)
  }

  x = dplyr::group_split(x, x[idcol])

  if (zip) {
    dir.create(file.path(tempdir(), paste0("jsonzip", idcol)))
    message("Writing JSON")
    for (i in seq(1, length(x))) {
      sub = as.data.frame(x[[i]])
      jsonlite::write_json(sub, file.path(tempdir(), paste0("jsonzip", idcol), paste0(sub[idcol], ".json")))
    }
    files = list.files(file.path(tempdir(), paste0("jsonzip", idcol)))
    message("Zipping JSON")
    my_wd = getwd()
    setwd(file.path(tempdir(), paste0("jsonzip", idcol)))
    utils::zip(file.path(my_wd, path, paste0(idcol, "_json.zip")), files, flags = "-q")
    setwd(my_wd)
    unlink(file.path(tempdir(), paste0("jsonzip", idcol)), recursive = TRUE)

    return(file.path(my_wd, path, paste0(idcol, "_json.zip")))
  } else {
    message("Writing JSON")
    for (i in seq(1, length(x))) {
      sub = x[[i]]
      jsonlite::write_json(sub, file.path(path, paste0(sub[idcol], ".json")))
    }
  }

  return(path)
}


#' Function to summarise uptake data into zone
#' @param comm uptake_utility_fastest df
#' @param nm name to append to columns
#'
#' the names are appended to column names
make_utility_stats = function(comm, nm, zones) {
  # Get start and end data zones
  # end_point = lwgeom::st_endpoint(comm)
  # end_point = sf::st_join(sf::st_as_sf(end_point), zones)
  # comm$endDZ = end_point$DataZone

  # start_point = lwgeom::st_startpoint(comm)
  # start_point = sf::st_join(sf::st_as_sf(start_point), zones)
  # comm$startDZ = start_point$DataZone


  # Drop Geometry
  comm = sf::st_drop_geometry(comm)
  comm$quietness = as.numeric(comm$quietness)

  # Utility Origin Stats
  comm_from = dplyr::group_by(comm, startDZ, purpose)
  comm_from = dplyr::summarise(comm_from,
    orig_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
    orig_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
    orig_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
    orig_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
    orig_taxi_go_dutch = sum(taxi_go_dutch, na.rm = TRUE),
    orig_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
    orig_car_ebike = sum(car_ebike, na.rm = TRUE),
    orig_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
    orig_foot_ebike = sum(foot_ebike, na.rm = TRUE),
    orig_taxi_ebike = sum(taxi_ebike, na.rm = TRUE),
    orig_quietness = mean(quietness, na.rm = TRUE),
    orig_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )


  comm_to = dplyr::group_by(comm, endDZ, purpose)
  comm_to = dplyr::summarise(comm_to,
    dest_bicycle_go_dutch = sum(bicycle_go_dutch, na.rm = TRUE),
    dest_car_go_dutch = sum(car_go_dutch, na.rm = TRUE),
    dest_public_transport_go_dutch = sum(public_transport_go_dutch, na.rm = TRUE),
    dest_foot_go_dutch = sum(foot_go_dutch, na.rm = TRUE),
    dest_taxi_go_dutch = sum(taxi_go_dutch, na.rm = TRUE),
    dest_bicycle_ebike = sum(bicycle_ebike, na.rm = TRUE),
    dest_car_ebike = sum(car_ebike, na.rm = TRUE),
    dest_public_transport_ebike = sum(public_transport_ebike, na.rm = TRUE),
    dest_foot_ebike = sum(foot_ebike, na.rm = TRUE),
    dest_taxi_ebike = sum(taxi_ebike, na.rm = TRUE),
    dest_quietness = mean(quietness, na.rm = TRUE),
    dest_route_hilliness = mean(route_hilliness, na.rm = TRUE)
  )

  # Pivot Wider
  comm_from = tidyr::pivot_wider(comm_from,
    id_cols = startDZ,
    names_from = "purpose",
    values_from = orig_bicycle_go_dutch:orig_route_hilliness,
    names_glue = "{purpose}_{.value}"
  )

  comm_to = tidyr::pivot_wider(comm_to,
    id_cols = endDZ,
    names_from = "purpose",
    values_from = dest_bicycle_go_dutch:dest_route_hilliness,
    names_glue = "{purpose}_{.value}"
  )


  names(comm_from)[1] = "DataZone"
  names(comm_to)[1] = "DataZone"

  comm = dplyr::full_join(comm_from, comm_to, by = "DataZone")

  names(comm)[2:ncol(comm)] = paste0(names(comm)[2:ncol(comm)], "_", nm)

  return(comm)
}




#' Function to summarise routes into zone statistics
#' @param r sf data frame of routes returned by cyclestreets
#' @param route_type name of route type e.g. fastest, quietest
#' @param route_purose name of route type e.g. commute, school
#'
#' the names are appended to column names
# routes_to_zone_stats <- function(r, route_type = "fastest", route_purpose = "commute"){
#
#   # Drop unneeded data
#   r <- sf::st_drop_geometry(r)
#   r <- r[,c("route_number","geo_code1","geo_code2",
#             "all","car","car_passenger",
#             "bicycle","foot","other",
#             "bicycle_go_dutch",
#             "car_go_dutch","public_transport",
#             "public_transport_go_dutch","foot_go_dutch","other_go_dutch",
#             "quietness","distances","route_hilliness")]
#
#   # One row per route
#   r <- dplyr::group_by(r, route_number)
#   r <- dplyr::summarise(r,
#                         geo_code1 = geo_code1[1],
#                         geo_code2 = geo_code2[1],
#                         all = all[1],
#                         car = car[1],
#                         bicycle = bicycle[1],
#                         foot = foot[1],
#                         other = other[1],
#                         bicycle_go_dutch = bicycle_go_dutch[1],
#                         car_go_dutch = car_go_dutch[1],
#                         public_transport = public_transport[1],
#                         public_transport_go_dutch = public_transport_go_dutch[1],
#                         foot_go_dutch = foot_go_dutch[1],
#                         other_go_dutch = other_go_dutch[1],
#                         quietness = weighted.mean(quietness, distances),
#                         route_hilliness = route_hilliness[1]
#   )
#
#   # Summarise by origin zone
#   stat_origin <- dplyr::group_by(r, geo_code1)
#   stat_origin <- dplyr::summarise(stat_origin,
#                                   orig_all = round(sum(all)),
#                                   orig_car = round(sum(car)),
#                                   orig_bicycle = round(sum(bicycle)),
#                                   orig_foot = round(sum(foot)),
#                                   orig_other = round(sum(other)),
#                                   orig_bicycle_go_dutch = round(sum(bicycle_go_dutch)),
#                                   orig_car_go_dutch = round(sum(car_go_dutch)),
#                                   orig_public_transport = round(sum(public_transport)),
#                                   orig_public_transport_go_dutch = round(sum(public_transport_go_dutch)),
#                                   orig_foot_go_dutch = round(sum(foot_go_dutch)),
#                                   orig_other_go_dutch = round(sum(other_go_dutch)),
#                                   orig_quietness_mean = round(mean(quietness)),
#                                   orig_hilliness_mean = round(mean(route_hilliness*100),1)
#   )
#   names(stat_origin)[1] <- "geo_code"
#
#   # Summarise by destination
#   stat_dest <- dplyr::group_by(r, geo_code2)
#   stat_dest <- dplyr::summarise(stat_dest,
#                                 dest_all = round(sum(all)),
#                                 dest_car = round(sum(car)),
#                                 dest_car_passenger = round(sum(car_passenger)),
#                                 dest_bicycle = round(sum(bicycle)),
#                                 dest_foot = round(sum(foot)),
#                                 dest_other = round(sum(other)),
#                                 dest_bicycle_go_dutch = round(sum(bicycle_go_dutch)),
#                                 dest_car_go_dutch = round(sum(car_go_dutch)),
#                                 dest_car_passenger_go_dutch = round(sum(car_passenger_go_dutch)),
#                                 dest_public_transport = round(sum(public_transport)),
#                                 dest_public_transport_go_dutch = round(sum(public_transport_go_dutch)),
#                                 dest_foot_go_dutch = round(sum(foot_go_dutch)),
#                                 dest_other_go_dutch = round(sum(other_go_dutch)),
#                                 dest_quietness_mean = round(mean(quietness)),
#                                 dest_hilliness_mean = round(mean(route_hilliness*100),1)
#   )
#   names(stat_dest)[1] <- "geo_code"
#
#   # Bind togther
#
#   stat_all <- dplyr::left_join(stat_origin, stat_dest, by = "geo_code")
#
#   # Update names
#   names(stat_all) <- paste0(names(stat_all),"_",route_type,"_",route_purpose)
#   names(stat_all)[1] <- "geo_code"
#
#   return(stat_all)
#
#
# }
